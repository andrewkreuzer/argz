const std = @import("std");

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
        three: argz.Arg(String) = .{ .short = "-s", .description = "a string type" },
        four: argz.Arg(Switch) = .{ .description = "an enum type" },
    };

    var cli = argz.Args(Args).init(allocator);
    defer cli.deinit();

    const args = try cli.parse();

    // zig build example -- --one --two 1 2 3 --three string --four two
    std.debug.print("one: value '{any}' of type {s}\n", .{args.one, @typeName(@TypeOf(args.one))});
    std.debug.print("teo: value '{d}' of type {s}\n", .{args.two, @typeName(@TypeOf(args.two))});
    std.debug.print("three: value '{s}' of type {s}\n", .{args.three.inner, @typeName(@TypeOf(args.three.inner))});
    std.debug.print("four: value '{s}' of type {s}\n", .{@tagName(args.four), @typeName(@TypeOf(args.four))});
}
