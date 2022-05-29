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
        self: @This(),
        _: std.mem.Allocator,
        other: NewSig,
    ) NewSig {
        const max_depth_read =
            @maximum(self.in.len, other.in.len - self.out.len);

        const final_stack_pos =
            self.out.len - self.in.len + other.out.len - other.in.len;

        // ([] -> [num]) * ([] -> [num]) = ([] -> [num, num])

        return .{
            .in = self.in[self.in.len - max_depth_read .. self.in.len],
            .out = other.out[max_depth_read + final_stack_pos ..],
        };
    }
};

test "initialize new sig" {
    _ = NewSig.init(&.{.num}, &.{.num});
}

test "compose two operands" {
    const left = NewSig.init(&.{}, &.{.num});
    const right = NewSig.init(&.{}, &.{.num});

    _ = left.compose(std.testing.allocator, right);
}
