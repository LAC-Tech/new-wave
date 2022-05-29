const std = @import("std");

pub const Sig = struct {
    in: i8,
    out: i8,

    pub fn init(in: i8, out: i8) Sig {
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
        const max_depth_read = @maximum(self.in, other.in - self.out);
        const final_stack_pos = -self.in + self.out - other.in + other.out;

        return .{ .in = max_depth_read, .out = max_depth_read + final_stack_pos };
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
};

test "initialize new sig" {
    _ = NewSig.init(&.{.num}, &.{.num});
}
