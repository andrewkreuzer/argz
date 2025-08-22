const std = @import("std");
const mem = std.mem;
const meta = std.meta;
const process = std.process;
const Allocator = mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const ArrayList = std.ArrayList;
const StructField = std.builtin.Type.StructField;


const ArgIterator = @import("ArgIterator.zig");

pub const ArgType = enum {
    arg,
    positional,
    subcommand,
};

pub fn Arg(comptime T: anytype) type {
    return struct {
        type: type = T,
        arg_type: ArgType = .arg,
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

pub fn Positional(comptime T: anytype) type {
    return struct {
        type: type = T,
        arg_type: ArgType = .positional,
        description: ?[]const u8 = null,

        pub fn eql(_: Positional(T), comptime _: []const u8, _: []const u8) bool {
            return false;
        }
    };
}

pub fn SubCommand(comptime T: anytype) type {
    return struct {
        type: type = T,
        arg_type: ArgType = .subcommand,
        description: ?[]const u8 = null,

        pub fn eql(_: SubCommand(T), comptime name: []const u8, arg: []const u8) bool {
            return mem.eql(u8, arg, name);
        }
    };
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
            if (!@hasDecl(@TypeOf(iter.*), "next"))
                @compileError("Iterator must implement next");

            _ = comptime validateFlags(A, "");
            return self.readCommand(A, iter);
        }

        fn readCommand(self: *Self, comptime T: anytype, iter: anytype) !ArgStruct(T) {
            if (@typeInfo(T) != .@"struct") @compileError("commands must be a struct type");
            var args: ArgStruct(T) = .{};
            const fields = @typeInfo(T).@"struct".fields;
            var pos_index: usize = 0;

            // How can we track fields which have been filled
            // vs ones that can be set to a default? and check
            // that all required fields are set after we're done?

            loop: while (true) {
                const token = token: {
                    if (self.token) |t| {
                        self.token = null;
                        break :token t;
                    } else if (iter.next()) |t| break :token t else break :loop;
                };

                inline for (fields, 0..) |f, i| {
                    const arg_info = f.defaultValue().?;
                    if (arg_info.eql(f.name, token)) {
                        switch (arg_info.arg_type) {
                            .arg => @field(args, f.name) = try self.readType(arg_info.type, null, iter),
                            .positional => unreachable,
                            .subcommand => switch (@typeInfo(arg_info.type)) {
                                .optional => |op| @field(args, f.name) = try self.readCommand(op.child, iter),
                                else => @field(args, f.name) = try self.readCommand(arg_info.type, iter),
                            },
                        }
                        continue :loop;
                    }

                    const is_flag = if (token.len > 0) (token[0] == '-') else false;
                    if (!is_flag and arg_info.arg_type == .positional) {
                        // we keep track of positionals we've seen
                        // as long as we're above that index we can
                        // consume the token as the positional arg
                        if (i >= pos_index) {
                            @field(args, f.name) = try self.readType(arg_info.type, token, iter);
                            pos_index += 1;
                            continue :loop;
                        }
                    }
                }

                // didn't match anything put it back for the parent context iterator
                self.token = token;
                break;

            }
            return args;
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

            const arg_space, const type_space, const has_positional, const has_subcommands = comptime blk: {
                var max_arg: usize = 0;
                var max_type: usize = 0;
                var has_pos = false;
                var has_subs = false;

                for (@typeInfo(A).@"struct".fields) |field| {
                    if (field.defaultValue()) |arg| {
                        switch (arg.arg_type) {
                            .arg => {
                                const long = arg.long orelse "--" ++ field.name;
                                const type_name = @typeName(arg.type);
                                if (long.len > max_arg) max_arg = long.len;
                                if (type_name.len > max_type) max_type = type_name.len;
                            },
                            .positional => {
                                const type_name = @typeName(arg.type);
                                if (type_name.len > max_type) max_type = type_name.len;
                                has_pos = true;
                            },
                            .subcommand => has_subs = true,
                        }
                    }
                }
                break :blk .{ max_arg, max_type, has_pos, has_subs };
            };

            const separation_spacing = 2;
            const fields = @typeInfo(A).@"struct".fields;
            var arguments = std.ArrayList(u8).init(self.arena.allocator());
            const arg_writer = arguments.writer();
            var options = std.ArrayList(u8).init(self.arena.allocator());
            const options_writer = options.writer();
            var subcommands = std.ArrayList(u8).init(self.arena.allocator());
            const subcommands_writer = subcommands.writer();

            inline for (fields) |field| {
                if (field.defaultValue()) |arg| {
                    switch (arg.arg_type) {
                        .arg => {
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
                        },
                        .positional => {
                            const type_name = @typeName(arg.type);
                            const description_spacing = type_space - type_name.len + separation_spacing;
                            var upper_name: [field.name.len]u8 = undefined;
                            _ = std.ascii.upperString(&upper_name, field.name);
                            try std.fmt.format(arg_writer, "    [{s}]{s}<{s}>{s}{s}\n",
                                .{
                                    upper_name,
                                    " " ** (arg_space - field.name.len + separation_spacing),
                                    type_name,
                                    " " ** description_spacing,
                                    arg.description.?
                                }
                            );
                        },
                        .subcommand => {
                            try std.fmt.format(subcommands_writer, "    {s}    {s}\n",
                                .{ field.name, arg.description.? }
                            );
                        },
                    }
                }
            }

            const arguments_text = try arguments.toOwnedSlice();
            defer self.arena.allocator().free(arguments_text);

            const options_text = try options.toOwnedSlice();
            defer self.arena.allocator().free(options_text);

            const subcommands_text = try subcommands.toOwnedSlice();
            defer self.arena.allocator().free(subcommands_text);

            try std.fmt.format(stdout,
                \\{s}
                \\
                , .{self.header}
            );

            if (has_positional) {
                try std.fmt.format(stdout,
                    \\
                    \\Arguments:
                    \\{s}
                    , .{arguments_text}
                );
            }

            try std.fmt.format(stdout,
                \\
                \\Options:
                \\{s}
                , .{ options_text }
            );

            if (has_subcommands) {
                try std.fmt.format(stdout,
                    \\
                    \\Subcommands:
                    \\{s}
                    , .{subcommands_text}
                );
            }
        }
    };
}

const FlagInfo = struct {
    short: ?[]const u8,
    long: ?[]const u8,
    context: []const u8,
    field_name: []const u8,
};

test "flag conflict detection" {
    const Sub1 = struct {
        sub_flag1: Arg(bool) = .{ .short = "-c", .description = "Second flag" },
        sub_flag2: Arg(bool) = .{ .short = "-d", .description = "First flag" },
        // should produce compiler error
        // sub_flag3: Arg(bool) = .{ .short = "-a", .description = "Second flag" },
    };
    const Sub2 = struct {
        sub_flag1: Arg(bool) = .{ .short = "-c", .description = "Second flag" },
        sub_flag2: Arg(bool) = .{ .short = "-d", .description = "First flag" },
        // should produce compiler error
        // sub_flag3: Arg(bool) = .{ .short = "-a", .description = "Second flag" },
    };

    const ValidArgs = struct {
        sub: SubCommand(Sub1) = .{ .description = "A subcommand with flags" },
        sub2: SubCommand(Sub2) = .{ .description = "A subcommand with flags" },
        flag1: Arg(bool) = .{ .short = "-a", .description = "First flag" },
        flag2: Arg(bool) = .{ .short = "-b", .description = "Second flag" },
        // should produce compiler error
        // flag3: Arg(bool) = .{ .description = "Second flag" },
        // flag4: Arg(bool) = .{ .description = "Second flag" },
    };
    _ = comptime validateFlags(ValidArgs, "");
}

fn validateFlags(comptime T: type, comptime context: []const u8) []const FlagInfo {
    const info = @typeInfo(T);
    if (info != .@"struct")
        @compileError(@typeName(T) ++ " is not a struct, args must be a struct type");

    comptime var flags: []const FlagInfo = &[_]FlagInfo{};

    const fields = info.@"struct".fields;
    inline for (fields) |field| {
        const arg_info = field.defaultValue().?;
        const t: type = arg_info.type;
        const t_info = @typeInfo(t);

        var short: ?[]const u8 = null;
        var long: ?[]const u8 = null;
        switch (arg_info.arg_type) {
            .arg => {
                short = arg_info.short orelse "-" ++ field.name[0..1];
                long = arg_info.long orelse "--" ++ field.name;
            },
            .positional => continue,
            .subcommand => {
                const command_ctx = switch (context.len) {
                    0 => field.name,
                    else => context ++ "." ++ field.name,
                };

                const command_flags = switch (t_info) {
                    .optional => |op| validateFlags(op.child, command_ctx),
                    else => validateFlags(t, command_ctx),
                };

                long = field.name;
                flags = flags ++ command_flags;
            },
        }

        for (flags) |flag| {
            const ctx1 = if (context.len == 0) "root" else context;
            const ctx2 = if (flag.context.len == 0) "root" else flag.context;

            if (short != null and flag.short != null and mem.eql(u8, short.?, flag.short.?)) {
                @compileError(std.fmt.comptimePrint(
                    "Short flag conflict: '{s}' is used by both '{s}.{s}' and '{s}.{s}'",
                    .{ short.?, ctx1, field.name, ctx2, flag.field_name }
                ));
            }

            if (long != null and flag.long != null and mem.eql(u8, long.?, flag.long.?)) {
                @compileError(std.fmt.comptimePrint(
                    "Long flag conflict: '{s}' is used by both '{s}.{s}' and '{s}.{s}'",
                    .{ long.?, ctx1, field.name, ctx2, flag.field_name }
                ));
            }
        }

        const flag_info = [_]FlagInfo{.{
            .short = short,
            .long = long,
            .context = context,
            .field_name = field.name,
        }};
        flags = flags ++ &flag_info;

    }

    return flags;
}

fn ArgStruct(comptime T: anytype) type {
    const info = @typeInfo(T);
    if (info != .@"struct")
        @compileError(@typeName(T) ++ " is not a struct, args must be a struct type");

    const in_fields = info.@"struct".fields;
    var out_fields: [in_fields.len]StructField = undefined;
    for (in_fields, 0..) |f, i| {
        const arg_info = f.defaultValue().?;
        var t: type = arg_info.type;
        const t_info = @typeInfo(t);

        switch (arg_info.arg_type) {
            .subcommand => switch (t_info) {
                .optional => |op| t = ?ArgStruct(op.child),
                else => t = ArgStruct(arg_info.type),
            },
            else => {}
        }

        const default: t = switch (t_info) {
            .optional => null,
            .@"bool" => false,
            .@"struct" => switch (t) {
                // hmm ??
                // ArrayList => .empty,
                else => undefined,
            },
            else => undefined,
        };

        out_fields[i] = StructField{
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
            .fields = &out_fields,
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
        sub: SubCommand(?SubCmd) = .{ .description = "A subcommand" },
        flag: Arg(bool) = .{ .short = "-b", .description = "A flag" },
        value: Arg(?String) = .{ .description = "a string file" },
        num: Arg(?u32) = .{ .description = "a number" },
        float: Arg(?f16) = .{ .description = "a float" },
        variant: Arg(?TestEnum) = .{
            .short = "-o",
            .description = "A enum"
        },
        multivalue: Arg(?[]u8) = .{
            .short = "-m",
            .long = "--nums",
            .description = "Multiple values",
        },
        multistring: Arg(?[]String) = .{
            .short = "-e",
            .long = "--entries",
            .description = "Multiple strings",
        },
        sized_array: Arg(?[3]u8) = .{
            .long = "--sized",
            .description = "A sized array of u8",
        },
    };

    const tests = [_]struct {
        args: []const u8,
        expected: struct {
            ?bool, // flag
            ?String, // value
            ?u32, // num
            ?f16, // float
            ?TestEnum, // variant
            ?[]const u8, // multivalue
            ?[]const String, // multistring
            ?[3]u8 // sized_array
        },
    }{
        .{
            .args = "-v config.zig",
            .expected = .{ false, .{ .inner = "config.zig" }, null, null, null, null, null, null },
        },
        .{
            .args = "-v config.zig --flag",
            .expected = .{ true, .{ .inner = "config.zig" }, null, null, null, null, null, null },
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
            .expected = .{ true, .{ .inner = "config.zig" }, null, null, null, &[_]u8{1, 2, 3}, null, null },
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
                true, .{ .inner = "config.zig" }, 1, 1.2, .eq, &[_]u8{4, 3, 2, 1},
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

        const value = t.expected.@"1";
        try std.testing.expectEqualDeep(value, args.value);

        const num = t.expected.@"2";
        try std.testing.expectEqual(num, args.num);

        const float = t.expected.@"3";
        try std.testing.expectEqual(float, args.float);

        const variant = t.expected.@"4";
        try std.testing.expectEqual(variant, args.variant);

        const multivalue = t.expected.@"5";
        try std.testing.expectEqualDeep(multivalue, args.multivalue);

        const multistring = t.expected.@"6";
        try std.testing.expectEqualDeep(multistring, args.multistring);

        const sized_array = t.expected.@"7";
        try std.testing.expectEqualDeep(sized_array, args.sized_array);

        if (args.sub) |sub| try std.testing.expectEqual(sub.init, false);
    }
}

// test "required" {
//     const allocator = std.testing.allocator;
//     const TestArgs = struct {
//         pos: Positional(String) = .{ .description = "A positional argument" },
//     };

//     const tests = [_]struct {
//         args: []const u8,
//     }{
//         .{ .args = "" },
//     };


//     for (tests) |t| {
//         var iter = try process.ArgIteratorGeneral(.{}).init(allocator, t.args);
//         defer iter.deinit();

//         var argz = Args(TestArgs).init(allocator);
//         defer argz.deinit();
//         try std.testing.expectError(error.MissingArgument, argz.parseWithIterator(&iter));
//     }
// }

test "positional" {
    const allocator = std.testing.allocator;
    const TestArgs = struct {
        pos: Positional(String) = .{ .description = "A positional argument" },
        optional_pos: Positional(?String) = .{ .description = "An optional positional argument" },
        flag: Arg(bool) = .{ .description = "A flag" },
    };

    const tests = [_]struct {
        args: []const u8,
        expected: struct { String, ?String, bool },
    }{
        .{ .args = "one", .expected = .{ .{ .inner = "one" }, null, false } },
        .{ .args = "one two", .expected = .{  .{ .inner = "one" }, .{ .inner = "two" }, false } },
        .{ .args = "one -f", .expected = .{ .{ .inner = "one" }, null, true } },
        .{ .args = "-f one", .expected = .{ .{ .inner = "one" }, null, true } },
        // TODO: should throw error that required positional argument is missing
        // .{ .args = "", .expected = .{ null, null, true } },
    };


    for (tests) |t| {
        var iter = try process.ArgIteratorGeneral(.{}).init(allocator, t.args);
        defer iter.deinit();

        var argz = Args(TestArgs).init(allocator);
        defer argz.deinit();
        const args = try argz.parseWithIterator(&iter);

        const pos = t.expected.@"0";
        try std.testing.expectEqualStrings(pos.inner, args.pos.inner);

        const optional_pos = t.expected.@"1";
        if (optional_pos) |op| {
            try std.testing.expectEqualStrings(op.inner, args.optional_pos.?.inner);
        } else {
            try std.testing.expect(args.optional_pos == null);
        }

        const flag = t.expected.@"2";
        try std.testing.expectEqual(flag, args.flag);
    }
}

test "subcommand" {
    const allocator = std.testing.allocator;
    const SubCmd = struct {
        init: Arg(bool) = .{ .short = "-i", .description = "Initialize something" },
    };

    const TestArgs = struct {
        sub: SubCommand(?SubCmd) = .{ .description = "A subcommand" },
        flag: Arg(bool) = .{ .short = "-b", .description = "A flag" },
    };

    const tests = [_]struct {
        args: []const u8,
        expected: struct { bool, bool },
    }{
        .{ .args = "sub -i", .expected = .{ true, false } },
        .{ .args = "-b", .expected = .{ false, true } },
        .{ .args = "sub -i -b", .expected = .{ true, true } },
    };


    for (tests) |t| {
        var iter = try process.ArgIteratorGeneral(.{}).init(allocator, t.args);
        defer iter.deinit();

        var argz = Args(TestArgs).init(allocator);
        defer argz.deinit();
        const args = try argz.parseWithIterator(&iter);

        const sub_init = t.expected.@"0";
        if (args.sub) |s| {
            try std.testing.expectEqual(sub_init, s.init);
        } else {
            try std.testing.expect(sub_init == false);
        }

        const flag = t.expected.@"1";
        try std.testing.expectEqual(flag, args.flag);
    }
}

// TODO: We probably shouldn't support multiple subcommands
test "multi subcommand" {
    const allocator = std.testing.allocator;
    const SubCmd1 = struct {
        init: Arg(bool) = .{ .short = "-i", .description = "Initialize something" },
    };
    const SubCmd2 = struct {
        init: Arg(bool) = .{ .short = "-i", .description = "Initialize something" },
    };

    const TestArgs = struct {
        sub: SubCommand(?SubCmd1) = .{ .description = "A subcommand" },
        sub2: SubCommand(?SubCmd2) = .{ .description = "A subcommand" },
        flag: Arg(bool) = .{ .short = "-b", .description = "A flag" },
    };

    const tests = [_]struct {
        args: []const u8,
        expected: struct { bool, bool, bool },
    }{
        .{ .args = "", .expected = .{ false, false, false } },
        .{ .args = "-b", .expected = .{ true, false, false } },
        .{ .args = "sub -i", .expected = .{ false, true, false } },
        .{ .args = "sub2 -i", .expected = .{ false, false, true } },
        .{ .args = "sub -i -b", .expected = .{ true, true, false } },
        .{ .args = "sub2 -i -b", .expected = .{ true, false, true } },
        .{ .args = "sub -i -b sub2 -i", .expected = .{ true, true, true } },
        .{ .args = "sub2 -i -b sub -i", .expected = .{ true, true, true } },
        .{ .args = "sub2 -i sub", .expected = .{ false, false, true } },
        .{ .args = "sub2 sub -i", .expected = .{ false, true, false } },
    };


    for (tests) |t| {
        var iter = try process.ArgIteratorGeneral(.{}).init(allocator, t.args);
        defer iter.deinit();

        var argz = Args(TestArgs).init(allocator);
        defer argz.deinit();
        const args = try argz.parseWithIterator(&iter);

        const flag = t.expected.@"0";
        try std.testing.expectEqual(flag, args.flag);

        const sub_init1 = t.expected.@"1";
        if (args.sub) |s| {
            try std.testing.expectEqual(sub_init1, s.init);
        } else {
            try std.testing.expect(sub_init1 == false);
        }

        const sub_init2 = t.expected.@"2";
        if (args.sub2) |s| {
            try std.testing.expectEqual(sub_init2, s.init);
        } else {
            try std.testing.expect(sub_init2 == false);
        }
    }
}

test "positional subcommand" {
    const allocator = std.testing.allocator;
    const SubCmd = struct {
        pos: Positional(String) = .{ .description = "A positional argument" },
        init: Arg(bool) = .{ .short = "-i", .description = "Initialize something" },
    };

    const TestArgs = struct {
        sub: SubCommand(?SubCmd) = .{ .description = "A subcommand" },
        flag: Arg(bool) = .{ .short = "-a", .description = "A flag" },
        flag2: Arg(bool) = .{ .short = "-b", .description = "A flag" },
    };

    const tests = [_]struct {
        args: []const u8,
        expected: struct { String, bool, bool, bool },
    }{
        .{ .args = "sub one -i", .expected = .{ .{ .inner = "one" }, true, false, false } },
        .{ .args = "-a", .expected = .{ .{ .inner = "" }, false, true, false } },
        .{ .args = "sub -i two -a", .expected = .{ .{ .inner = "two" }, true, true, false } },
        .{ .args = "sub two -i -a", .expected = .{ .{ .inner = "two" }, true, true, false } },
        .{ .args = "-a sub two -i", .expected = .{ .{ .inner = "two" }, true, true, false } },
        .{ .args = "-a -b sub two -i", .expected = .{ .{ .inner = "two" }, true, true, true } },
        .{ .args = "sub two -i -a -b", .expected = .{ .{ .inner = "two" }, true, true, true } },
    };


    for (tests) |t| {
        var iter = try process.ArgIteratorGeneral(.{}).init(allocator, t.args);
        defer iter.deinit();

        var argz = Args(TestArgs).init(allocator);
        defer argz.deinit();
        const args = try argz.parseWithIterator(&iter);

        const sub_pos = t.expected.@"0";
        const sub_init = t.expected.@"1";
        if (args.sub) |s| {
            try std.testing.expectEqualStrings(sub_pos.inner, s.pos.inner);
            try std.testing.expectEqualDeep(sub_init, s.init);
        } else {
            // try std.testing.expect(sub_pos); // undefined
            try std.testing.expect(sub_init == false);
        }

        const flag = t.expected.@"2";
        try std.testing.expectEqual(flag, args.flag);

        const flag2 = t.expected.@"3";
        try std.testing.expectEqual(flag2, args.flag2);
    }
}

test "multivalue before subcommand" {
    const allocator = std.testing.allocator;
    const SubCmd = struct {
        init: Arg(bool) = .{ .short = "-i", .description = "Initialize something" },
    };

    const TestArgs = struct {
        multi: Arg([]String) = .{
            .short = "-n",
            .long = "--nums",
            .description = "Multiple values",
        },
        sub: SubCommand(?SubCmd) = .{ .description = "A subcommand" },
    };

    const tests = [_]struct {
        args: []const u8,
        expected: struct { []String, bool },
    }{
        // this is a bit of an edge case, but want to test similarity to other arg parsers
        // because we have a multi arg before the sub command, the sub command arg gets eaten
        .{
            .args = "-n one two three sub -i",
            .expected = .{
                @constCast(&[_]String{
                .{ .inner = "one" },
                .{ .inner = "two" },
                .{ .inner = "three"},
                .{ .inner = "sub"},
                }), false
            },
        }
    };


    for (tests) |t| {
        var iter = try process.ArgIteratorGeneral(.{}).init(allocator, t.args);
        defer iter.deinit();

        var argz = Args(TestArgs).init(allocator);
        defer argz.deinit();
        const args = try argz.parseWithIterator(&iter);

        const multi = t.expected.@"0";
        try std.testing.expectEqualDeep(multi, args.multi);

        const sub_init = t.expected.@"1";
        if (args.sub) |s| {
            try std.testing.expectEqualDeep(sub_init, s.init);
        } else {
            try std.testing.expect(sub_init == false);
        }
    }
}

test "help text" {
    const allocator = std.testing.allocator;
    const TestArgs = struct {
        flag: Arg(bool) = .{ .description = "A simple flag" },
        value: Arg(String) = .{ .description = "Configuration file" },
        pos: Positional(String) = .{ .description = "Positional argument" },
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
        \\Arguments:
        \\    [POS]        <root.String>    Positional argument
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

test "help text with subcommands" {
    const allocator = std.testing.allocator;

    const SubCmd = struct {
        init: Arg(bool) = .{ .short = "-i", .description = "Initialize something" },
    };

    const TestArgs = struct {
        sub: SubCommand(SubCmd) = .{ .description = "A subcommand" },
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
