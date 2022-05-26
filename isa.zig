const std = @import("std");

fn arrayList(comptime T: type, allocator: std.mem.Allocator) !std.ArrayList(T) {
    return std.ArrayList(T).initCapacity(allocator, 0xFF);
}

pub const OpCode = enum(u8) {
    // Internal
    end, push, call, ret,
    // Math
    add, sub, mul, div,
    // Stack manipulation
    dup, drop
};

pub const Num = f64;
pub const Operand = union {num: Num};

pub const Address = usize;

pub const Word = struct {
    operands: []const Operand,
    addresses: []const Address,
    code: []const u8
};

const Frame = struct {
    pc: usize,
    word: *const Word,

    pub fn code(self: @This()) u8 {
        return self.word.code[self.pc];
    }
};

pub const VM = struct {
    allocator: std.mem.Allocator,
    temp: Operand,
    ds: std.ArrayList(Operand),
    rs: std.ArrayList(Frame),
    frame: Frame,
    memory: std.ArrayList(Word),

    pub fn init(allocator: std.mem.Allocator) !VM {
        return VM {
            .allocator = allocator,
            .temp = undefined,
            .ds = try arrayList(Operand, allocator),
            .rs = try arrayList(Frame, allocator),
            .frame = undefined,
            .memory = try arrayList(Word, allocator)
        };
    }

    pub fn exec(self: *@This(), entry_word: *const Word) !void {
        self.frame = .{.pc = 0, .word = entry_word};

        while (true) : (self.frame.pc +%= 1) {
            var op_code = @intToEnum(OpCode, self.frame.code());
            
            switch (op_code) {
                .end => break,
                .push => {
                    self.frame.pc += 1;
                    const index = self.frame.code();
                    self.temp = self.frame.word.operands[index];
                    try self.ds.append(self.temp);
                },
                .call => {
                    self.frame.pc += 1;
                    try self.rs.append(self.frame);

                    const index = self.frame.code();

                    self.frame = .{
                        .word = &self.memory.items[index],
                        .pc = std.math.maxInt(usize)
                    };
                },
                .ret => self.frame = self.rs.pop(),
                .add => {
                    self.temp = self.ds.pop();
                    self.top().*.num += self.temp.num;
                },
                .sub => {
                    self.temp = self.ds.pop();
                    self.top().*.num -= self.temp.num;
                },
                .mul => {
                    self.temp = self.ds.pop();
                    self.top().*.num *= self.temp.num;
                },
                .div => {
                    self.temp = self.ds.pop();
                    var tos = self.top();
                    tos.*.num = @divExact(tos.*.num, self.temp.num);
                },
                .dup => try self.ds.append(self.top().*),
                .drop => _ = self.ds.pop()
            }
        }
    }

    pub fn store(self: *@This(), word: Word) !Address {
        const address = self.memory.items.len;
        try self.memory.append(word);
        return address;
    }

    fn top(self: *@This()) *Operand {
        return &self.ds.items[self.ds.items.len-1];
    }

    pub fn print(self: *@This(), writer: anytype) !void {
        if (self.ds.items.len == 0) return;

        try writer.print("{d}", .{self.ds.items[0].num});

        if (self.ds.items.len > 1) {
            for (self.ds.items[1..]) |d| {
                try writer.print(" {d}", .{d.num});
            }
        }
    }
};
