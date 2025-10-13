# argz

A type-safe, declarative CLI argument parser for zig with compile-time validation.

## Installation

Add argz to your build.zig.zon:

```zig
.dependencies = .{
    .argz = .{
        .url = "https://github.com/andrewkreuzer/argz/archive/[commit-hash].tar.gz",
        .hash = "[hash]",
    },
},
```

Then in your build.zig:

```zig
const argz = b.dependency("argz", .{
    .target = target,
    .optimize = optimize,
});
exe.root_module.addImport("argz", argz.module("argz"));
```

## Quick Start

```zig
const std = @import("std");
const argz = @import("argz");

const Args = struct {
    verbose: argz.Arg(bool) = .{ .description = "Enable verbose output" },
    config: argz.Arg(?argz.String) = .{ .description = "Configuration file path" },
    count: argz.Arg(?u32) = .{ .short = "-n", .description = "Number of iterations" },
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    const allocator = gpa.allocator();

    var cli = argz.Parse(Args).init(allocator);
    defer cli.deinit();

    const args = try cli.parse();

    if (args.verbose) {
        std.debug.print("Verbose mode enabled\n", .{});
    }

    if (args.config) |config| {
        std.debug.print("config: {s}\n", .{config.inner});
    }

    if (args.count) |count| for (0..count) |i| {
        std.debug.print("count: {d}\n", .{i});
    };
}
```

## Argument Types

### Flags

Boolean flags are automatically set to true when present:

```zig
const Args = struct {
    help: argz.Arg(bool) = .{
        .short = "-h",
        .long = "--help",
        .description = "Show help message"
    },
};
```

### String Arguments

Use argz.String for string arguments:

```zig
const Args = struct {
    name: argz.Arg(argz.String) = .{ .description = "Your name" },
};

// Access the string value
std.debug.print("Hello, {s}!\n", .{args.name.inner});
```

### Numeric Arguments

Support for integers and floats:

```zig
const Args = struct {
    port: argz.Arg(u16) = .{ .description = "Port number" },
    threshold: argz.Arg(f32) = .{ .description = "Threshold value" },
};
```

### Enum Arguments

Parse enum values from strings:

```zig
const LogLevel = enum { debug, info, warn, @"error" };

const Args = struct {
    log_level: argz.Arg(LogLevel) = .{ .description = "Logging level" },
};
```

### Optional Arguments

Wrap any type in optional to make it non-required:

```zig
const Args = struct {
    input: argz.Arg(?argz.String) = .{ .description = "Input file" },
};

if (args.input) |file| {
    std.debug.print("Processing {s}\n", .{file.inner});
}
```

### Multi-Value Arguments

Collect multiple values for the same flag using an ArrayList:

```zig
const Args = struct {
    files: argz.Arg(std.ArrayList(argz.String)) = .{ .description = "Files to process" },
};

// Usage: program --files one.txt --files two.txt --files three.txt
for (args.files.items) |file| {
    std.debug.print("File: {s}\n", .{file.inner});
}
```

### Positional Arguments

Arguments without flags that are matched by position:

```zig
const Args = struct {
    source: argz.Positional(argz.String) = .{ .description = "Source file" },
    dest: argz.Positional(?argz.String) = .{ .description = "Destination file" },
};

// Usage: program source.txt dest.txt
```

## Subcommands

Create nested command structures with isolated argument sets:

```zig
const InitCmd = struct {
    force: argz.Arg(bool) = .{ .short = "-f", .description = "Force initialization" },
    path: argz.Positional(argz.String) = .{ .description = "Project path" },
};

const BuildCmd = struct {
    release: argz.Arg(bool) = .{ .short = "-r", .description = "Release build" },
};

const Args = struct {
    verbose: argz.Arg(bool) = .{ .short = "-v", .description = "Verbose output" },
    init: argz.SubCommand(?InitCmd) = .{ .description = "Initialize a new project" },
    build: argz.SubCommand(?BuildCmd) = .{ .description = "Build the project" },
};

// Usage: program -v init -f /path/to/project
if (args.init) |init| {
    if (init.force) {
        std.debug.print("Force initializing {s}\n", .{init.path.inner});
    }
}
```

## Help Text

Generate help text automatically:

```zig
try cli.printHelp();
```

Output:

```
Usage: program [options]

Arguments:
    [SOURCE]  <argz.String>  Source file

Options:
    -v, --verbose  <bool>          Enable verbose output
    -c, --config   <argz.String>   Configuration file

Subcommands:
    init     Initialize a new project
    build    Build the project
```

## Required Arguments

Arguments are automatically required except when nullable they'll default to
null, booleans will default to false, slice pointers default to &.{} (an empty
slice), and ArrayList will default to .empty.

```zig
const Args = struct {
    required_field: argz.Arg(argz.String) = .{ .description = "Must be provided" },
    optional_field: argz.Arg(?argz.String) = .{ .description = "Can be omitted" },
};

// Missing required_field will return error.MissingRequiredArgument
```

## Flag Aliases

Specify both short and long forms:

```zig
const Args = struct {
    output: argz.Arg(argz.String) = .{
        .short = "-o",
        .long = "--output",
        .description = "Output file"
    },
};

// Both work: program -o file.txt
//            program --output file.txt
```

Default aliases are generated automatically if not specified. The short flag is
the first character of the struct field name, and the long flag the full field
name. Mostly following GNU style guidelines prefixing short flags with a single
dash and long with a double. If two fields clash using the same short flag it
will produce a compiler error letting you know which are incompatible.

## Error Handling

The parser returns specific errors:

```zig
error.InvalidArgument          // Failed to parse value
error.InvalidEnum              // Invalid enum variant
error.MissingArgument          // Expected value for flag
error.MissingRequiredArgument  // Required field not provided
```

## Examples

See the [examples](./examples) directory for complete working examples:

```sh
zig build example -- --one --two 1 2 3 --three string --four two
zig build help-demo
```

## Testing

Run the test suite:

```sh
zig build test
```

## License

Licensed under the MIT License. See LICENSE file for details.
