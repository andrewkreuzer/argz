const std = @import("std");
const mem = std.mem;
const meta = std.meta;
const process = std.process;
const ArenaAllocator = std.heap.ArenaAllocator;
const ArrayList = std.ArrayList;
const ArgIterator = process.ArgIterator;
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
            var iter = try ArgIterator.initWithAllocator(self.arena.allocator());
            defer iter.deinit();
            _ = iter.skip(); // skip the program name
            return try self.parseWithIterator(&iter);
        }

        pub fn parseWithIterator(self: *Self, iter: anytype) !ArgStruct(A) {
            comptime {
                if (!@hasDecl(@TypeOf(iter.*), "next"))
                    @compileError("Iterator must implement next");

                const fields = @typeInfo(A).@"struct".fields;
                var shorts: [fields.len][]const u8 = [_][]const u8{""} ** fields.len;
                var longs: [fields.len][]const u8 = [_][]const u8{""} ** fields.len;
                for (fields, 0..) |f, i| {
                    const arg_info = f.defaultValue().?;
                    const type_info = @typeInfo(arg_info.type);
                    if (type_info == .@"struct" and arg_info.type != String) {
                        shorts[i] = "";
                        longs[i] = f.name;
                        continue;
                    }

                    const short = arg_info.short orelse "-" ++ f.name[0..1];
                    if (in(short, &shorts))
                        @compileError("short flag " ++ short ++ " is being used. please provide a different .short option for " ++ f.name);

                    const long = arg_info.long orelse "--" ++ f.name;
                    if (in(long, &longs))
                        @compileError("long flag " ++ long ++ " is being used. please provide a different .long option for " ++ f.name);

                    shorts[i] = short;
                    longs[i] = long;
                }
            }

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
                    if (arg_info.eql(f.name, current_token)) {
                        const is_struct = @typeInfo(arg_info.type) == .@"struct";
                        if (is_struct and arg_info.type != String) {
                            @field(args, f.name) = try self.readSubcommand(arg_info.type, iter);
                        } else {
                            @field(args, f.name) = try self.readType(arg_info.type, null, iter);
                        }
                    }
                }
            }
            return args;
        }

        fn in(comptime name: []const u8, list: [][]const u8) bool {
            for (list) |l| if (mem.eql(u8, l, name)) return true;
            return false;
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

            const arg_space, const type_space = comptime blk: {
                var max_arg: usize = 0;
                var max_type: usize = 0;
                for (@typeInfo(A).@"struct".fields) |field| {
                    if (field.defaultValue()) |arg| {
                        const long = arg.long orelse "--" ++ field.name;
                        const type_name = @typeName(arg.type);
                        if (long.len > max_arg) max_arg = long.len;
                        if (type_name.len > max_arg) max_type = type_name.len;
                    }
                }
                break :blk .{ max_arg, max_type };
            };

            const fields = @typeInfo(A).@"struct".fields;
            var options = try std.ArrayList(u8).initCapacity(self.arena.allocator(), fields.len);
            const writer = options.writer();
            const seperation_spacing = 2;
            inline for (fields) |field| {
                if (field.defaultValue()) |arg| {
                    const short = arg.short orelse "-" ++ field.name[0..1];
                    const long = arg.long orelse "--" ++ field.name;
                    const type_name = @typeName(arg.type);
                    //TODO: it'd be cooler to center this value
                    const type_spacing = arg_space - long.len + seperation_spacing;
                    const description_spacing = type_space - type_name.len + seperation_spacing;
                    try std.fmt.format(writer, "    {s}, {s}{s}<{s}>{s}{s}\n",
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

            const options_text = try options.toOwnedSlice();
            defer self.arena.allocator().free(options_text);

            // TOOD: footer
            try std.fmt.format(stdout,
            \\{s}
            \\
            \\Options:
            \\{s}
            , .{self.header, options_text});
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
            if (f.type != String) {
                t = ArgStruct(t);
            }
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
