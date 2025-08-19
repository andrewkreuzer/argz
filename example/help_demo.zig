const std = @import("std");
const argz = @import("argz");
const String = argz.String;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const SubCmd = struct {
        init: argz.Arg(bool) = .{ .short = "-i", .description = "Initialize the project" },
        verbose: argz.Arg(bool) = .{ .short = "-v", .description = "Verbose output" },
    };

    const Args = struct {
        help: argz.Arg(bool) = .{ .short = "-h", .long = "--help", .description = "Show help" },
        pos: argz.Positional(String) = .{ .description = "Position argument" },
        config: argz.Arg(String) = .{ .short = "-c", .description = "Configuration file" },
        sub: argz.SubCommand(SubCmd) = .{ .description = "Initialize subcommand" },
    };

    var cli = argz.Args(Args).init(allocator);
    defer cli.deinit();

    try cli.printHelp();
}
