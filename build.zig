const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const lib_mod = b.addModule("argz", .{ .root_source_file = b.path("src/root.zig") });

    const example = b.addExecutable(.{
        .name = "argz-example",
        .root_module = b.createModule(.{
            .root_source_file = b.path("example/cli.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    example.root_module.addImport("argz", lib_mod);
    const example_cmd = b.addRunArtifact(example);
    example_cmd.step.dependOn(b.getInstallStep());
    const run_step = b.step("example", "Run the example");
    run_step.dependOn(&example_cmd.step);
    if (b.args) |args| example_cmd.addArgs(args);

    const lib_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize
    });
    const run_lib_unit_tests = b.addRunArtifact(lib_unit_tests);
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_lib_unit_tests.step);
}
