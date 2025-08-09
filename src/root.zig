const std = @import("std");
const mem = std.mem;
const meta = std.meta;
const process = std.process;
const ArenaAllocator = std.heap.ArenaAllocator;
const ArrayList = std.ArrayList;
const ArgIterator = @import("ArgIterator.zig");
const Allocator = mem.Allocator;
const StructField = std.builtin.Type.StructField;

pub fn Arg(comptime T: anytype) type {
    return struct {
        type: type = T,
        short: ?[]const u8 = null,
        long: ?[]const u8 = null,
        description: ?[]const u8 = null,

        pub fn eql(self: Arg(T), comptime name: []const u8, arg: []const u8) bool {
            const short = self.short orelse "-" ++ name[0..1];
            const long = self.long orelse "--" ++ name;
            return mem.eql(u8, arg, short) or mem.eql(u8, arg, long);
        }
    };
}

pub fn Command(comptime T: anytype) type {
    return struct {
        type: type = T,
        description: ?[]const u8 = null,
        subcommand: bool = true,

        pub fn eql(_: Command(T), comptime name: []const u8, arg: []const u8) bool {
            return mem.eql(u8, arg, name);
        }
    };
}

const FlagInfo = struct {
    short: ?[]const u8,
    long: ?[]const u8,
    context: []const u8,
    field_name: []const u8,
};

fn validateFlags(comptime T: type, comptime context: []const u8) []const FlagInfo {
    comptime var flags: []const FlagInfo = &[_]FlagInfo{};

    const fields = @typeInfo(T).@"struct".fields;
    inline for (fields) |field| {
        const arg_info = field.defaultValue().?;

        for (flags) |flag2| {
            if (arg_info.short != null and flag2.short != null and
                mem.eql(u8, arg_info.short.?, flag2.short.?)) {
                const context1 = if (context.len == 0) "root" else context;
                const context2 = if (flag2.context.len == 0) "root" else flag2.context;
                @compileError(std.fmt.comptimePrint(
                        "Short flag conflict: '{s}' is used by both '{s}.{s}' and '{s}.{s}'",
                        .{ arg_info.short.?, context1, field.name, context2, flag2.field_name }
                ));
            }

            if (arg_info.long != null and flag2.long != null and
                mem.eql(u8, arg_info.long.?, flag2.long.?)) {
                const context1 = if (context.len == 0) "root" else context;
                const context2 = if (flag2.context.len == 0) "root" else flag2.context;
                @compileError(std.fmt.comptimePrint(
                        "Long flag conflict: '{s}' is used by both '{s}.{s}' and '{s}.{s}'",
                        .{ arg_info.long.?, context1, field.name, context2, flag2.field_name }
                ));
            }
        }

        const has_subcommand = @hasField(@TypeOf(arg_info), "subcommand");
        if (has_subcommand and arg_info.subcommand) {
            const subcommand_context = if (context.len == 0) field.name else context ++ "." ++ field.name;
            const subcommand_flags = validateFlags(arg_info.type, subcommand_context);
            const subcommand_flag = [_]FlagInfo{.{
                .short = null,
                .long = field.name,
                .context = context,
                .field_name = field.name,
            }};
            flags = flags ++ &subcommand_flag ++ subcommand_flags;
        } else {
            const short = arg_info.short orelse "-" ++ field.name[0..1];
            const long = arg_info.long orelse "--" ++ field.name;
            const flag_info = [_]FlagInfo{.{
                .short = short,
                .long = long,
                .context = context,
                .field_name = field.name,
            }};
            flags = flags ++ &flag_info;
        }
    }

    return flags;
}

pub fn Args(A: anytype) type {
    return struct {
        const Self = @This();
        header: []const u8 = std.fmt.comptimePrint("Usage: {s} [options]", .{@typeName(A)}),
        arena: ArenaAllocator,
        writer: ?std.io.AnyWriter = null,
        token: ?[]const u8 = null,

        pub fn init(allocator: Allocator) Self {
            return .{
                .arena = ArenaAllocator.init(allocator),
            };
        }

        pub fn deinit(self: *Self) void {
            self.arena.deinit();
        }

        pub fn parse(self: *Self) !ArgStruct(A) {
            var iter = ArgIterator.init();
            _ = iter.skip(); // skip the program name
            return try self.parseWithIterator(&iter);
        }

        pub fn parseWithIterator(self: *Self, iter: anytype) !ArgStruct(A) {
            _ = comptime validateFlags(A, "");
            if (!@hasDecl(@TypeOf(iter.*), "next"))
                @compileError("Iterator must implement next");


            var args: ArgStruct(A) = .{};
            const fields = @typeInfo(A).@"struct".fields;
            loop: while (true) {
                const current_token = token: {
                    if (self.token) |t| {
                        self.token = null;
                        break :token t;
                    } else if (iter.next()) |t| break :token t else break :loop;
                };

                inline for (fields) |f| {
                    const arg_info = f.defaultValue().?;
                    const arg_type = @TypeOf(arg_info);
                    if (arg_info.eql(f.name, current_token)) {
                        if (@hasField(arg_type, "subcommand") and arg_info.subcommand) {
                            @field(args, f.name) = try self.readSubcommand(arg_info.type, iter);
                        } else {
                            @field(args, f.name) = try self.readType(arg_info.type, null, iter);
                        }
                    }
                }
            }
            return args;
        }

        fn readSubcommand(self: *Self, comptime T: anytype, iter: anytype) !ArgStruct(T) {
            if (@typeInfo(T) != .@"struct") @compileError("subcommand must be a struct type");
            var sub_args: ArgStruct(T) = .{};
            const fields = @typeInfo(T).@"struct".fields;
            loop: while (true) {
                const current_token = token: {
                    if (self.token) |t| {
                        self.token = null;
                        break :token t;
                    } else if (iter.next()) |t| break :token t else break :loop;
                };
                inline for (fields) |f| {
                    const arg_info = f.defaultValue().?;
                    if (arg_info.eql(f.name, current_token)) {
                        const is_struct = @typeInfo(arg_info.type) == .@"struct";
                        if (is_struct and arg_info.type != String) {
                            @field(sub_args, f.name) = try self.readSubcommand(arg_info.type, iter);
                        } else {
                            @field(sub_args, f.name) = try self.readType(arg_info.type, null, iter);
                        }
                    }
                }
            }
            return sub_args;
        }

        fn readType(self: *Self, comptime T: anytype, token: ?[]const u8, iter: anytype) !T {
            return switch (@typeInfo(T)) {
                .@"bool" => true,
                .int => std.fmt.parseInt(T, token orelse iter.next() orelse return error.InvalidArgument, 0),
                .float => std.fmt.parseFloat(T, token orelse iter.next() orelse return error.InvalidArgument),
                .pointer => |ptr_info| switch (ptr_info.size) {
                    .slice => slice: {
                        const CT = ptr_info.child;
                        var list = ArrayList(CT).init(self.arena.allocator());
                        while (iter.next()) |t| {
                            if (t[0] == '-') {
                                self.token = t;
                                break;
                            }
                            const value = try self.readType(CT, t, iter);
                            try list.append(value);
                        }
                        break :slice try list.toOwnedSlice();
                    },
                    .one,
                    .many,
                    .c => @compileError("only slice ptr types are supported")
                },
                .array => |array_info| array: {
                    const CT = array_info.child;
                    var list: T = undefined;
                    for (0..array_info.len) |i| {
                        if (iter.next()) |t| {
                            list[i] = try self.readType(CT, t, iter);
                        } else return error.MissingArgument;
                    }
                    break :array list;
                },
                .@"struct" => switch (T) {
                    String => .{ .inner = token orelse iter.next() orelse return error.MissingArgument },
                    else => @compileError("struct argument " ++ @typeName(T) ++ " is not supported"),
                },
                .optional => |op| op: {
                    const ret: T = try self.readType(op.child, token, iter);
                    break :op ret;
                },
                // enum_literal: void,
                .@"enum" => meta.stringToEnum(T, token orelse iter.next() orelse "")
                    orelse return error.InvalidEnum,
                // @"union": Union,

                // .type: void,
                // .void: void,
                // .noretur: voidn
                // comptime_float: void,
                // comptime_int: void,
                // undefined: void,
                // null: void,
                // error_union: ErrorUnion,
                // error_set: ErrorSet,
                // @"fn": Fn,
                // @"opaque": Opaque,
                // frame: Frame,
                // @"anyframe": AnyFrame,
                // vector: Vector,
                else => @compileError("argument of type: " ++ @typeName(T) ++ " not supported"),
            };
        }

        pub fn printHelp(self: *Self) !void {
            const stdout = self.writer orelse std.io.getStdOut().writer().any();

            const arg_space, const type_space, const has_subcommands = comptime blk: {
                var max_arg: usize = 0;
                var max_type: usize = 0;
                var has_subs = false;

                for (@typeInfo(A).@"struct".fields) |field| {
                    if (field.defaultValue()) |arg| {
                        const has_subcommand = @hasField(@TypeOf(arg), "subcommand");
                        if (has_subcommand and arg.subcommand) {
                            has_subs = true;
                        } else {
                            const long = arg.long orelse "--" ++ field.name;
                            const type_name = @typeName(arg.type);
                            if (long.len > max_arg) max_arg = long.len;
                            if (type_name.len > max_type) max_type = type_name.len;
                        }
                    }
                }
                break :blk .{ max_arg, max_type, has_subs };
            };

            const fields = @typeInfo(A).@"struct".fields;
            var options = std.ArrayList(u8).init(self.arena.allocator());
            const options_writer = options.writer();
            var subcommands = std.ArrayList(u8).init(self.arena.allocator());
            const subcommands_writer = subcommands.writer();

            const separation_spacing = 2;

            inline for (fields) |field| {
                if (field.defaultValue()) |arg| {
                    const has_subcommand = @hasField(@TypeOf(arg), "subcommand");
                    if (has_subcommand and arg.subcommand) {
                        try std.fmt.format(subcommands_writer, "    {s}    {s}\n",
                            .{ field.name, arg.description.? }
                        );
                    } else {
                        const short = arg.short orelse "-" ++ field.name[0..1];
                        const long = arg.long orelse "--" ++ field.name;
                        const type_name = @typeName(arg.type);
                        const type_spacing = arg_space - long.len + separation_spacing;
                        const description_spacing = type_space - type_name.len + separation_spacing;

                        try std.fmt.format(options_writer, "    {s}, {s}{s}<{s}>{s}{s}\n",
                            .{
                                short,
                                long,
                                " " ** type_spacing,
                                type_name,
                                " " ** description_spacing,
                                arg.description.?
                            }
                        );
                    }
                }
            }

            const options_text = try options.toOwnedSlice();
            defer self.arena.allocator().free(options_text);

            const subcommands_text = try subcommands.toOwnedSlice();
            defer self.arena.allocator().free(subcommands_text);

            if (has_subcommands) {
                try std.fmt.format(stdout,
                \\{s}
                \\
                \\Options:
                \\{s}
                \\Subcommands:
                \\{s}
                , .{self.header, options_text, subcommands_text});
            } else {
                try std.fmt.format(stdout,
                \\{s}
                \\
                \\Options:
                \\{s}
                , .{self.header, options_text});
            }
        }
    };
}

fn ArgStruct(comptime T: anytype) type {
    const info = @typeInfo(T);
    if (info != .@"struct") @compileError(@typeName(T) ++ " is not a struct, args must be a struct type");
    const in_fields = info.@"struct".fields;
    var fields: [in_fields.len]StructField = undefined;
    for (in_fields, 0..) |f, i| {
        const arg_info = f.defaultValue().?;
        comptime var t: type = arg_info.type;
        const has_subcommand = @hasField(@TypeOf(arg_info), "subcommand");
        if (has_subcommand and arg_info.subcommand) {
            t = ArgStruct(t);
        }

        const default: t = undefined;
        fields[i] = StructField{
            .name = f.name,
            .type = t,
            .default_value_ptr = @ptrCast(&default),
            .is_comptime = false,
            .alignment = @alignOf(arg_info.type),
        };
    }
    return @Type(.{
        .@"struct" = .{
            .layout = .auto,
            .fields = &fields,
            .decls = &.{},
            .is_tuple = false,
        }
    });
}


pub const String = struct {
    inner: []const u8,
};

const ArgError = error{
    InvalidArgument,
    InvalidEnum,
    MissingArgument,
};

test Args {
    const allocator = std.testing.allocator;
    const TestEnum = enum {
        less,
        more,
        eq,
    };

    const SubCmd = struct {
        init: Arg(bool) = .{ .short = "-i", .description = "Initialize something" },
    };

    const TestArgs = struct {
        sub: Command(SubCmd) = .{ .description = "A subcommand" },
        flag: Arg(bool) = .{ .short = "-b", .description = "A flag" },
        value: Arg(String) = .{ .description = "a string file" },
        num: Arg(?u32) = .{ .description = "a number" },
        float: Arg(?f16) = .{ .description = "a float" },
        variant: Arg(?TestEnum) = .{
            .short = "-o",
            .description = "A enum"
        },
        multivalue: Arg([]u8) = .{
            .short = "-m",
            .long = "--nums",
            .description = "Multiple values",
        },
        multistring: Arg([]String) = .{
            .short = "-e",
            .long = "--entries",
            .description = "Multiple strings",
        },
        sized_array: Arg([3]u8) = .{
            .long = "--sized",
            .description = "A sized array of u8",
        },
    };

    const tests = [_]struct {
        args: []const u8,
        expected: struct { ?bool, ?[]const u8, ?u32, ?f16, ?TestEnum, ?[]const u8, ?[]const String, ?[3]u8 },
    }{
        .{
            .args = "-v config.zig",
            .expected = .{ false, "config.zig", null, null, null, null, null, null },
        },
        .{
            .args = "-v config.zig --flag",
            .expected = .{ true, "config.zig", null, null, null, null, null, null },
        },
        .{
            .args = "--num 2147483648",
            .expected = .{ false, null, 2147483648, null, null, null, null, null },
        },
        .{
            .args = "-o less",
            .expected = .{ false, null, null, null, .less, null, null, null },
        },
        .{
            .args = "-o more",
            .expected = .{ false, null, null, null, .more, null, null, null },
        },
        .{
            .args = "--nums 1 2 3 --flag",
            .expected = .{ true, null, null, null, null, &[_]u8{1, 2, 3}, null, null },
        },
        .{
            .args = "-v config.zig --nums 1 2 3 --flag",
            .expected = .{ true, "config.zig", null, null, null, &[_]u8{1, 2, 3}, null, null },
        },
        .{
            .args = "--entries one two three",
            .expected = .{
                false, null, null, null, null, null,
                &[_]String{.{ .inner = "one" }, .{ .inner = "two" }, .{ .inner = "three" }},
                null
            },
        },
        .{
            .args = "--sized 255 255 255",
            .expected = .{ false, null, null, null, null, null, null, [_]u8{ 255, 255, 255 } },
        },
        .{
            .args = "--flag -v config.zig -n 1 --float 1.2 -o eq --nums 4 3 2 1 --entries one two three --sized 1 2 3",
            .expected = .{
                true, "config.zig", 1, 1.2, .eq, &[_]u8{4, 3, 2, 1},
                &[_]String{.{ .inner = "one" }, .{ .inner = "two" }, .{ .inner = "three" }},
                [3]u8{1, 2, 3}
            },
        },
    };


    for (tests) |t| {
        var iter = try process.ArgIteratorGeneral(.{}).init(allocator, t.args);
        defer iter.deinit();

        var argz = Args(TestArgs).init(allocator);
        defer argz.deinit();
        const args = try argz.parseWithIterator(&iter);

        const flag = t.expected.@"0";
        try std.testing.expectEqual(flag, args.flag);

        const value = t.expected.@"1" orelse "";
        try std.testing.expectEqualStrings(value, args.value.inner);

        const num = t.expected.@"2";
        try std.testing.expectEqual(num, args.num);

        const float = t.expected.@"3";
        try std.testing.expectEqual(float, args.float);

        const variant = t.expected.@"4";
        try std.testing.expectEqual(variant, args.variant);

        const multivalue = t.expected.@"5" orelse &[_]u8{};
        try std.testing.expectEqualSlices(u8, multivalue, args.multivalue);

        const multistring = t.expected.@"6" orelse &[_]String{};
        try std.testing.expectEqualDeep(multistring, args.multistring);

        const sized_array = t.expected.@"7" orelse [3]u8{0, 0, 0};
        try std.testing.expectEqualDeep(sized_array, args.sized_array);

        try std.testing.expect(!args.sub.init);
    }
}

test "subcommand" {
    const allocator = std.testing.allocator;
    const SubCmd = struct {
        init: Arg(bool) = .{ .short = "-i", .description = "Initialize something" },
    };

    const TestArgs = struct {
        sub: Command(SubCmd) = .{ .description = "A subcommand" },
        flag: Arg(bool) = .{ .short = "-b", .description = "A flag" },
    };

    const tests = [_]struct {
        args: []const u8,
        expected: struct { bool, bool },
    }{
        .{
            .args = "sub -i",
            .expected = .{ true, false },
        },
        // .{
        //     .args = "-f",
        //     .expected = .{ false, true },
        // },
    };


    for (tests) |t| {
        var iter = try process.ArgIteratorGeneral(.{}).init(allocator, t.args);
        defer iter.deinit();

        var argz = Args(TestArgs).init(allocator);
        defer argz.deinit();
        const args = try argz.parseWithIterator(&iter);

        const sub_init = t.expected.@"0";
        try std.testing.expectEqual(sub_init, args.sub.init);

        const flag = t.expected.@"1";
        try std.testing.expectEqual(flag, args.flag);
    }
}

test "flag conflict detection" {
    const ValidArgs = struct {
        flag1: Arg(bool) = .{ .short = "-f", .description = "First flag" },
        flag2: Arg(bool) = .{ .short = "-g", .description = "Second flag" },
    };
    _ = comptime validateFlags(ValidArgs, "");
}

test "help text with subcommands" {
    const allocator = std.testing.allocator;

    const SubCmd = struct {
        init: Arg(bool) = .{ .short = "-i", .description = "Initialize something" },
    };

    const TestArgs = struct {
        sub: Command(SubCmd) = .{ .description = "A subcommand" },
        flag: Arg(bool) = .{ .short = "-f", .description = "A flag" },
        value: Arg(String) = .{ .description = "Configuration file" },
    };

    var buf: [2048]u8 = undefined;
    var fbs = std.io.fixedBufferStream(buf[0..]);
    const writer = fbs.writer();

    var argz = Args(TestArgs).init(allocator);
    defer argz.deinit();

    argz.writer = writer.any();
    try argz.printHelp();

    const expected =
        \\Usage: root.test.help text with subcommands.TestArgs [options]
        \\
        \\Options:
        \\    -f, --flag   <bool>         A flag
        \\    -v, --value  <root.String>  Configuration file
        \\
        \\Subcommands:
        \\    sub    A subcommand
        \\
        ;
    try std.testing.expectEqualStrings(expected, fbs.getWritten());
}

test "help text" {
    const allocator = std.testing.allocator;
    const TestArgs = struct {
        flag: Arg(bool) = .{ .description = "A simple flag" },
        value: Arg(String) = .{ .description = "Configuration file" },
        multivalue: Arg([]u8) = .{
            .long = "--nums",
            .description = "Multiple values",
        },
        multistring: Arg([]String) = .{
            .long = "--entries",
            .description = "Multiple strings",
        },
    };

    var buf: [1024]u8 = undefined;
    var fbs = std.io.fixedBufferStream(buf[0..]);
    const writer = fbs.writer();

    var argz = Args(TestArgs).init(allocator);
    defer argz.deinit();

    argz.writer = writer.any();
    try argz.printHelp();

    const expected =
        \\Usage: root.test.help text.TestArgs [options]
        \\
        \\Options:
        \\    -f, --flag     <bool>           A simple flag
        \\    -v, --value    <root.String>    Configuration file
        \\    -m, --nums     <[]u8>           Multiple values
        \\    -m, --entries  <[]root.String>  Multiple strings
        \\
        ;
    try std.testing.expectEqualStrings(expected, fbs.getWritten());
}
