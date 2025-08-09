const std = @import("std");
const mem = std.mem;

const Self = @This();

index: usize,
    count: usize,

    pub const InitError = error{};

pub fn init() Self {
    return .{
        .index = 0,
        .count = std.os.argv.len,
    };
}

pub fn next(self: *Self) ?[:0]const u8 {
    const result = self.peek();
    if (result != null) {
        self.index += 1;
    }
    return result;
}

pub fn skip(self: *Self) bool {
    if (self.index == self.count) return false;

    self.index += 1;
    return true;
}

pub fn peek(self: *Self) ?[:0]const u8 {
    if (self.index == self.count) return null;

    const s = std.os.argv[self.index];
    return mem.sliceTo(s, 0);
}
