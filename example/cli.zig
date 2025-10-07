const std = @import("std");
const ArrayList = std.ArrayList;

const argz = @import("argz");
const String = argz.String;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    const allocator = gpa.allocator();

    const Switch = enum {
        one,
        two,
        three,
    };

    const Args = struct {
        one: argz.Arg(bool) = .{ .description = "A simple flag" },
        two: argz.Arg([]u8) = .{ .description = "a u8 list" },
        three: argz.Arg(?String) = .{ .short = "-s", .description = "a string type" },
        four: argz.Arg(?Switch) = .{ .description = "an enum type" },
        five: argz.Arg(ArrayList(String)) = .{ .short = "-d", .description = "a string type" },
    };

    var cli = argz.Parse(Args).init(allocator);
    defer cli.deinit();

    const args = try cli.parse();

    // zig build example -- --one --two 1 2 3 --three string --four two
    std.debug.print("one: value '{any}' of type {s}\n", .{args.one, @typeName(@TypeOf(args.one))});
    std.debug.print("two: value '{any}' with length: {d} of type {s}\n", .{args.two, args.two.len, @typeName(@TypeOf(args.two))});

    if (args.three) |s| {
        std.debug.print("three: value '{s}' of type {s}\n", .{s.inner, @typeName(@TypeOf(s))});
    } else {
        std.debug.print("three: value 'null' of type {s}\n", .{@typeName(@TypeOf(args.three))});
    }

    if (args.four) |s| {
        std.debug.print("four: value '{s}' of type {s}\n", .{@tagName(s), @typeName(@TypeOf(s))});
    } else {
        std.debug.print("four: value 'null' of type {s}\n", .{@typeName(@TypeOf(args.four))});
    }

    std.debug.print("five: value with length: {d} of type {s}\n", .{args.five.items.len, @typeName(@TypeOf(args.five))});
    for (args.five.items, 0..) |item, i| {
        std.debug.print("  five[{d}] = '{s}'\n", .{i, item.inner});
    }
}
