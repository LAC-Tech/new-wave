const std = @import("std");

pub const Sig = struct {
    in: u8,
    out: u8,

    pub fn init(in: u8, out: u8) Sig {
        return Sig{ .in = in, .out = out };
    }

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        // TODO: hardcoding num

        var in_index: usize = 0;
        while (in_index < self.in) : (in_index += 1) {
            try writer.writeAll("num ");
        }

        try writer.writeAll("->");

        var out_index: usize = 0;
        while (out_index < self.out) : (out_index += 1) {
            try writer.writeAll(" num");
        }
    }

    pub fn compose(self: @This(), other: Sig) Sig {
        const max_depth_read = @max(
            self.in,
            std.math.sub(u8, other.in, self.out) catch 0,
        );
        const final_stack_pos = self.out - self.in + other.out - other.in;

        return .{
            .in = max_depth_read,
            .out = max_depth_read + final_stack_pos,
        };
    }
};

// pub const util = struct {
//     identity: Sig,
//     operand: Sig

//     fn init(allocator: std.mem.Allocator) {

//     }
// };

const Elem = enum { num };

const NewSig = struct {
    in: []const Elem,
    out: []const Elem,

    pub fn init(in: []const Elem, out: []const Elem) NewSig {
        return NewSig{ .in = in, .out = out };
    }

    pub fn compose(
        _: @This(),
        _: std.mem.Allocator,
        _: NewSig,
    ) NewSig {
        return .{ .in = &.{}, .out = &.{} };
    }
};

test "initialize new sig" {
    _ = NewSig.init(&.{.num}, &.{.num});
}

// test "compose two operands" {
//     const left = NewSig.init(&.{}, &.{.num});
//     const right = NewSig.init(&.{}, &.{.num});

//     _ = left.compose(std.testing.allocator, right);
// }
