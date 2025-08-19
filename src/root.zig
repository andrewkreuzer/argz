const std = @import("std");
const mem = std.mem;
const meta = std.meta;
const process = std.process;
const ArenaAllocator = std.heap.ArenaAllocator;
const ArrayList = std.ArrayList;
const ArgIterator = @import("ArgIterator.zig");
const Allocator = mem.Allocator;
const StructField = std.builtin.Type.StructField;

pub const ArgType = enum {
    Arg,
    Positional,
    SubCommand,
};

pub fn Arg(comptime T: anytype) type {
    return struct {
        type: type = T,
        arg_type: ArgType = .Arg,
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
        arg_type: ArgType = .Positional,
        description: ?[]const u8 = null,
        pub fn eql(_: Positional(T), comptime _: []const u8, _: []const u8) bool {
            return false;
        }
    };
}

pub fn SubCommand(comptime T: anytype) type {
    return struct {
        type: type = T,
        arg_type: ArgType = .SubCommand,
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
        positional_index: usize = 0,

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
            const positional_fields = comptime getPositionalFields(A);

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
                        switch (arg_info.arg_type) {
                            .Arg => @field(args, f.name) = try self.readType(arg_info.type, null, iter),
                            .SubCommand => @field(args, f.name) = try self.readSubcommand(arg_info.type, iter),
                            .Positional => unreachable,
                        }
                        continue :loop;
                    }
                }

                const is_flag = current_token[0] == '-';
                if (!is_flag) {
                    inline for (positional_fields, 0..) |field, field_index| {
                        if (field_index == self.positional_index) {
                            const arg_info = field.defaultValue().?;
                            @field(args, field.name) = try self.readType(arg_info.type, current_token, iter);
                            self.positional_index += 1;
                            continue :loop;
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
            const positional_fields = comptime getPositionalFields(T);
            var subcommand_positional_index: usize = 0;

            loop: while (true) {
                const current_token = token: {
                    if (self.token) |t| {
                        self.token = null;
                        break :token t;
                    } else if (iter.next()) |t| break :token t else break :loop;
                };

                inline for (fields) |f| {
                    const arg_info = f.defaultValue().?;
                    if (arg_info.arg_type == .Positional) continue;
                    if (arg_info.eql(f.name, current_token)) {
                        switch (arg_info.arg_type) {
                            .Arg => @field(sub_args, f.name) = try self.readType(arg_info.type, null, iter),
                            .SubCommand => @field(sub_args, f.name) = try self.readSubcommand(arg_info.type, iter),
                            .Positional => unreachable,
                        }
                        continue :loop;
                    }
                }

                const is_flag = current_token[0] == '-';
                if (!is_flag) {
                    inline for (positional_fields, 0..) |field, field_index| {
                        if (field_index == subcommand_positional_index) {
                            const arg_info = field.defaultValue().?;
                            @field(sub_args, field.name) = try self.readType(arg_info.type, current_token, iter);
                            subcommand_positional_index += 1;
                            continue :loop;
                        }
                    }
                }

                // didn't match anything put it back for the parent context iterator
                self.token = current_token;
                break;

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

            const arg_space, const type_space, const has_positional, const has_subcommands = comptime blk: {
                var max_arg: usize = 0;
                var max_type: usize = 0;
                var has_pos = false;
                var has_subs = false;

                for (@typeInfo(A).@"struct".fields) |field| {
                    if (field.defaultValue()) |arg| {
                        switch (arg.arg_type) {
                            .Arg => {
                                const long = arg.long orelse "--" ++ field.name;
                                const type_name = @typeName(arg.type);
                                if (long.len > max_arg) max_arg = long.len;
                                if (type_name.len > max_type) max_type = type_name.len;
                            },
                            .Positional => {
                                const type_name = @typeName(arg.type);
                                if (type_name.len > max_type) max_type = type_name.len;
                                has_pos = true;
                            },
                            .SubCommand => has_subs = true,
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
                        .Arg => {
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
                        .Positional => {
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
                        .SubCommand => {
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

fn getPositionalFields(comptime T: type) []const StructField {
    const info = @typeInfo(T);
    if (info != .@"struct")
        @compileError(@typeName(T) ++ " is not a struct, args must be a struct type");

    comptime var positional_fields: []const StructField = &[_]StructField{};
    const fields = info.@"struct".fields;
    inline for (fields) |field| {
        const arg_info = field.defaultValue().?;
        if (arg_info.arg_type == .Positional) {
            positional_fields = positional_fields ++ &[_]StructField{field};
        }
    }
    return positional_fields;
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

        var short: ?[]const u8 = null;
        var long: ?[]const u8 = null;
        switch (arg_info.arg_type) {
            .Arg => {
                short = arg_info.short orelse "-" ++ field.name[0..1];
                long = arg_info.long orelse "--" ++ field.name;
            },
            .Positional => continue, // positional arguments do not have flags
            .SubCommand => {
                const subcommand_context = if (context.len == 0) field.name else context ++ "." ++ field.name;
                const subcommand_flags = validateFlags(arg_info.type, subcommand_context);
                long = field.name;
                flags = flags ++ subcommand_flags;
            },
        }

        for (flags) |flag2| {
            if (short != null and flag2.short != null and mem.eql(u8, short.?, flag2.short.?)) {
                const context1 = if (context.len == 0) "root" else context;
                const context2 = if (flag2.context.len == 0) "root" else flag2.context;
                @compileError(std.fmt.comptimePrint(
                        "Short flag conflict: '{s}' is used by both '{s}.{s}' and '{s}.{s}'",
                        .{ short.?, context1, field.name, context2, flag2.field_name }
                ));
            }

            if (long != null and flag2.long != null and mem.eql(u8, long.?, flag2.long.?)) {
                const context1 = if (context.len == 0) "root" else context;
                const context2 = if (flag2.context.len == 0) "root" else flag2.context;
                @compileError(std.fmt.comptimePrint(
                        "Long flag conflict: '{s}' is used by both '{s}.{s}' and '{s}.{s}'",
                        .{ long.?, context1, field.name, context2, flag2.field_name }
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
    var fields: [in_fields.len]StructField = undefined;
    for (in_fields, 0..) |f, i| {
        const arg_info = f.defaultValue().?;
        comptime var t: type = arg_info.type;
        switch (arg_info.arg_type) {
            .SubCommand => t = ArgStruct(arg_info.type),
            else => {}
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
        sub: SubCommand(SubCmd) = .{ .description = "A subcommand" },
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
        expected: struct {
            ?bool, // flag
            ?[]const u8, // value
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
        sub: SubCommand(SubCmd) = .{ .description = "A subcommand" },
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
        try std.testing.expectEqual(sub_init, args.sub.init);

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
        sub: SubCommand(SubCmd1) = .{ .description = "A subcommand" },
        sub2: SubCommand(SubCmd2) = .{ .description = "A subcommand" },
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
        try std.testing.expectEqual(sub_init1, args.sub.init);

        const sub_init2 = t.expected.@"2";
        try std.testing.expectEqual(sub_init2, args.sub2.init);
    }
}

test "positional subcommand" {
    const allocator = std.testing.allocator;
    const SubCmd = struct {
        pos: Positional(String) = .{ .description = "A positional argument" },
        init: Arg(bool) = .{ .short = "-i", .description = "Initialize something" },
    };

    const TestArgs = struct {
        sub: SubCommand(SubCmd) = .{ .description = "A subcommand" },
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
        try std.testing.expectEqualStrings(sub_pos.inner, args.sub.pos.inner);

        const sub_init = t.expected.@"1";
        try std.testing.expectEqual(sub_init, args.sub.init);

        const flag = t.expected.@"2";
        try std.testing.expectEqual(flag, args.flag);

        const flag2 = t.expected.@"3";
        try std.testing.expectEqual(flag2, args.flag2);
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
