const std = @import("std");

const argz = @import("argz");
const String = argz.String;

const Args = struct {
    one: argz.Arg(bool) = .{ .description = "A simple flag" },
    two: argz.Arg([]const u8) = .{ .description = "a u8 string" },
    three: argz.Arg(String) = .{ .description = "a string type" },
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    const allocator = gpa.allocator();

    var cli = argz.Args(Args).init(allocator);
    defer cli.deinit();

    const args = try cli.parse();
    _ = args;
    try cli.printHelp();
}
