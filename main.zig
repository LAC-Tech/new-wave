//const mem = @import("mem");
const std = @import("std");

const Operand = f64;
const Address = usize;

const OpCode = enum(u8) {
    // Internal
    end, push, call, ret,

    // Math
    add, sub, mul, div,

    // Stack manipulation
    dup,
};

fn arrayList(comptime T: type, allocator: std.mem.Allocator) !std.ArrayList(T) {
    return std.ArrayList(T).initCapacity(allocator, 0xFF);
}

const Word = struct {
    operands: []const Operand,
    addresses: []const Address,
    code: []const u8
};

const WordBuffer = struct {
    operands: std.ArrayList(Operand),
    addresses: std.ArrayList(Address),
    code: std.ArrayList(u8),
    type_sig: TypeSig,

    pub fn init(allocator: std.mem.Allocator) !WordBuffer {
        return WordBuffer {
            .operands = try arrayList(Operand, allocator),
            .addresses = try arrayList(Address, allocator),
            .code = try arrayList(u8, allocator),
            .type_sig = TypeSig.init(0, 0)
        };
    }

    pub fn deinit(self: @This()) void {
        self.operands.deinit();
        self.addresses.deinit();
        self.code.deinit();
    }

    pub fn toEntryWord(self: *@This()) !Word {
        try self.code.append(@enumToInt(OpCode.end));
        return Word {
            .operands = self.operands.items,
            .addresses = self.addresses.items,
            .code = self.code.items,
        };
    }

    pub fn toUserWord(self: *@This()) !Word {
        try self.code.append(@enumToInt(OpCode.ret));
        return Word {
            .operands = self.operands.toOwnedSlice(),
            .addresses = self.addresses.toOwnedSlice(),
            .code = self.code.toOwnedSlice(),
        };
    }

    pub fn writeEntry(self: *@This(), entry: DictEntry) !void {
        self.type_sig = self.type_sig.compose(entry.type_sig); 

        switch(entry.impl) {
            .op_code => {
                try self.code.append(@enumToInt(entry.impl.op_code));
            },
            .address => {
                try self.code.appendSlice(&[_]u8{
                    @enumToInt(OpCode.call),
                    @intCast(u8, entry.impl.address)
                });
                try self.addresses.append(entry.impl.address);
            }   
        }
    }

    pub fn writeOperand(self: *@This(), operand: Operand) !void {
        self.type_sig = self.type_sig.compose(TypeSig.init(0, 1));

        try self.code.appendSlice(&[_]u8{
            @enumToInt(OpCode.push),
            @intCast(u8, self.operands.items.len)
        });
        try self.operands.append(operand);
    }

    pub fn clear(self: *@This()) void {
        self.operands.clearRetainingCapacity();
        self.addresses.clearRetainingCapacity();
        self.code.clearRetainingCapacity();
        self.type_sig = TypeSig.init(0, 0);
    }
};

const Frame = struct {
    pc: usize,
    word: *const Word,

    pub fn code(self: @This()) u8 {
        return self.word.code[self.pc];
    }
};

const VM = struct {
    allocator: std.mem.Allocator,
    ds: std.ArrayList(Operand),
    temp: Operand,
    rs: std.ArrayList(Frame),
    frame: Frame,
    memory: std.ArrayList(Word),

    pub fn init(allocator: std.mem.Allocator) !VM {
        return VM {
            .allocator = allocator,
            .ds = try arrayList(Operand, allocator),
            .temp = undefined,
            .rs = try arrayList(Frame, allocator),
            .frame = undefined,
            .memory = try arrayList(Word, allocator)
        };
    }

    pub fn deinit(self: @This()) void {
        self.ds.deinit();
        self.rs.deinit();
            
        for (self.memory.items) |word| {
            self.allocator.free(word.code);
            self.allocator.free(word.addresses);
            self.allocator.free(word.operands);
        }

        self.memory.deinit();
    }

    pub fn exec(self: *@This(), entry_word: *const Word) !void {
        self.frame = Frame {.pc = 0, .word = entry_word};

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

                    self.frame = Frame {
                        .word = &self.memory.items[index],
                        .pc = std.math.maxInt(usize)
                    };
                },
                .ret => self.frame = self.rs.pop(),
                .add => {
                    self.temp = self.ds.pop();
                    self.top().* += self.temp;
                },
                .sub => {
                    self.temp = self.ds.pop();
                    self.top().* -= self.temp;
                },
                .mul => {
                    self.temp = self.ds.pop();
                    self.top().* *= self.temp;
                },
                .div => {
                    self.temp = self.ds.pop();
                    var tos = self.top();
                    tos.* = @divExact(tos.*, self.temp);
                },
                .dup => try self.ds.append(self.top().*)
            }
        }
    }

    pub fn store(self: *@This(), word_buffer: *WordBuffer) !Address {
        const address = self.memory.items.len;
        try self.memory.append(try word_buffer.toUserWord());
        return address;
    }

    fn top(self: *@This()) *Operand {
        return &self.ds.items[self.ds.items.len-1];
    }

    pub fn print(self: *@This(), writer: anytype) !void {
        if (self.ds.items.len == 0) return;

        try writer.print("{d}", .{self.ds.items[0]});

        if (self.ds.items.len > 1) {
            for (self.ds.items[1..]) |d| {
                try writer.print(" {d}", .{d});
            }
        }
    }
};

const DictEntryTag = enum {
    op_code,
    address,
};

const DictEntry = struct {
    type_sig: TypeSig,
    impl: union(DictEntryTag) {
        op_code: OpCode,
        address: Address
    }
};

const Dict = std.StringHashMap(DictEntry);

const std_library = [_]std.meta.Tuple(&.{ []const u8, TypeSig, OpCode}) {
    .{"+",      TypeSig.init(2, 1), OpCode.add},
    .{"-",      TypeSig.init(2, 1), OpCode.sub},
    .{"*",      TypeSig.init(2, 1), OpCode.mul},
    .{"/",      TypeSig.init(2, 1), OpCode.div},
    .{"dup",    TypeSig.init(1, 2), OpCode.dup}
};

const TypeSigErr = error { Empty, ArityMismatch };

const TypeSig = struct {
    in: i8,
    out: i8,

    pub fn init(in: i8, out: i8) TypeSig {
        return TypeSig {.in = in, .out = out};
    }

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype
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

    pub fn compose(self: @This(), other: TypeSig) TypeSig {
        const max_depth_read = @maximum(self.in, other.in - self.out);
        const final_stack_pos = -self.in + self.out - other.in + other.out;

        return .{
            .in = max_depth_read,
            .out = max_depth_read + final_stack_pos
        };
    }
};

const Interpreter = struct {
    vm: VM,
    dict: Dict,
    output_buffer: std.ArrayList(u8),
    entry_word_buffer: WordBuffer,
    new_word_buffer: WordBuffer,
    new_word_name: []const u8,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) !Interpreter {
        var dict = Dict.init(allocator);

        for (std_library) |sl| {
            const entry = DictEntry {
                .type_sig = sl[1],
                .impl = .{.op_code = sl[2]}
            };
            try dict.put(try allocator.dupe(u8, sl[0]), entry);
        }

        return Interpreter {
            .vm = try VM.init(allocator),
            .dict = dict,
            .output_buffer = try arrayList(u8, allocator),
            .entry_word_buffer = try WordBuffer.init(allocator),
            .new_word_buffer = try WordBuffer.init(allocator),
            .new_word_name = "",
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *@This()) void {
        self.vm.deinit();

        var dict_it = self.dict.iterator();
        while (dict_it.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
        }

        self.dict.deinit();
        self.output_buffer.deinit();
        self.entry_word_buffer.deinit();
        self.new_word_buffer.deinit();
    }

    pub fn eval(self: *@This(), s: []const u8) ![]const u8 {
        if (s.len == 0) return s;

        self.entry_word_buffer.clear();
        self.output_buffer.clearRetainingCapacity();
        const writer = self.output_buffer.writer();
        
        var tokens = std.mem.tokenize(u8, s, &std.ascii.spaces);

        var word_buffer: *WordBuffer = &self.entry_word_buffer;

        while (tokens.next()) |token| {
            if (std.mem.eql(u8, token, ":")) {
                word_buffer = &self.new_word_buffer;

                if (tokens.next()) |name| {
                    self.new_word_name = name;
                }
            } else if (std.mem.eql(u8, token, ";")) {
                const addr = try self.vm.store(word_buffer);
                const entry = DictEntry{
                    .type_sig = word_buffer.type_sig,
                    .impl = .{.address = addr}
                };
                
                // Add it to dictionary
                const copied_name = try self.allocator.dupe(
                    u8,
                    self.new_word_name);

                try self.dict.put(copied_name, entry);

                try writer.print("{s} : {s}\n", .{
                    self.new_word_name,
                    self.new_word_buffer.type_sig
                });

                self.new_word_buffer.clear();
                word_buffer = &self.entry_word_buffer;
            } else if (self.dict.get(token)) |entry| {
                try word_buffer.writeEntry(entry);
            } else if (std.fmt.parseFloat(Operand, token)) |operand| {
                try word_buffer.writeOperand(operand);
            } else |_| {
                std.debug.print("available words: \n", .{});

                var names = self.dict.keyIterator();

                while (names.next()) |name| {
                    std.debug.print("{s} ", .{name.*});                    
                }

                try writer.print("unknown token: {s}\n", .{token});
                return self.output_buffer.items;
            }
        }

        if (self.entry_word_buffer.code.items.len > 0) {
            const entry_word = try self.entry_word_buffer.toEntryWord();

            try self.vm.exec(&entry_word);
            try self.vm.print(writer);

            try writer.writeAll(" :");
            
            var i: usize = 0;        
            while (i < self.vm.ds.items.len) : (i += 1) {
                try writer.writeAll(" num");
            }

            try writer.writeAll("\n");
        }

        // NOTE: assuming we print this to the screen immediately, as underlying
        // buffer will no longer be valid next time it's called
        return self.output_buffer.items;
    }
};

fn test_eval(expected: []const u8, actual: []const u8) !void {
    var interpreter = try Interpreter.init(std.testing.allocator);
    defer interpreter.deinit();
    return std.testing.expectEqualStrings(
        expected, try interpreter.eval(actual));

}

fn test_multiline_eval(output: []const []const u8, input: []const []const u8, ) !void {
    
    const allocator = std.testing.allocator;

    var interpreter = try Interpreter.init(allocator);
    defer interpreter.deinit();

    var actual_buf = try arrayList([]const u8, allocator);
    defer actual_buf.deinit();
    
    for (input) |s| {
        const eval_ouput = try interpreter.eval(s);
        try actual_buf.append(try allocator.dupe(u8, eval_ouput));
    }

    var expected_buf = try arrayList(u8, allocator);

    for (output) |s| {
        try expected_buf.writer().print("{s}\n", .{s});
    }

    defer expected_buf.deinit();

    const actual = try std.mem.join(allocator, "", actual_buf.items);

    defer for (actual_buf.items) |s| {
        allocator.free(s);
    };

    defer allocator.free(actual);

    return std.testing.expectEqualStrings(expected_buf.items, actual);
}

test "infer type of constant" {
    try test_eval("42 : num\n", "42");
}

test "infer type of two constants" {
    try test_eval("42 39 : num num\n", "42 39");
}

test "infer type of square" {
    try test_eval("sq : num -> num\n", ": sq dup * ;");
}

test "infer type of cube" {
    try test_eval("cube : num -> num\n", ": cube dup dup * * ;");
}

test  "type of add one" {
    try test_eval("add1 : num -> num\n", ": add1 1 + ;");
}

test "type of two constants" {
    try test_eval("3 4 : num num\n", "3 4");
}

// SICP Tests

test "eval empty input" {
    try test_eval("", "");
}

test "primitive expression" {
    try test_eval("486 : num\n", "486");
}

test "add ints" {
    try test_eval("486 : num\n", "137 349 +");
}

test "subtract ints" {
    try test_eval("666 : num\n", "1000 334 -");
}

test "divide ints" {
    try test_eval("2 : num\n", "10 5 /");
}

test "add real to int" {
    try test_eval("12.7 : num\n", "2.7 10 +");
}

test "add multiple ints" {
    try test_eval("75 : num\n", "21 35 + 12 + 7 +");
}

test "multiply multiple ints" {
    try test_eval("1200 : num\n", "25 4 * 12 *");
}

test "nested combinations" {
    try test_eval("19 : num\n", "3 5 * 10 6 - +");
}

test "relatively simple expressions" {
    try test_eval("57 : num\n", "3 2 4 * 3 5 + + * 10 7 - 6 + +");
}

test "naming a value" {
    const input = .{
        ": size 2 ;",
        "size",
        "5 size *"
    };

    const output = .{
        "size : -> num",
        "2 : num",
        "2 10 : num num"
    };

    try test_multiline_eval(&output, &input);
}

test "further examples of defining a value" {
    const input = .{
        ": pi 3.14159 ;",
        ": radius 10 ;",
        "radius radius * pi *",
        ": circumference 2 pi * radius * ;",
        "circumference"
    };

    const output = .{
        "pi : -> num",
        "radius : -> num",
        "314.159 : num",
        "circumference : -> num",
        "314.159 62.8318 : num num"
    };

    try test_multiline_eval(&output, &input);
}

test "procedure definition" {
    const input = .{
        ": square dup * ;",
        "21 square",
        "2 5 + square",
        "3 square square"
    };

    const output = .{
        "square : num -> num",
        "441 : num",
        "441 49 : num num",
        "441 49 81 : num num num"
    };

    try test_multiline_eval(&output, &input);
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var interpreter = try Interpreter.init(allocator);
    defer interpreter.deinit();

    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    var input: std.ArrayList(u8) = try arrayList(u8, allocator);

    while (true) {
        try stdout.print("> ", .{});
        // Read
        try stdin.readUntilDelimiterArrayList(&input, '\n', 512);
        // Eval
        var output = try interpreter.eval(input.items);
        // Print
        try stdout.print("{s}", .{output});
        // Loop
        input.clearRetainingCapacity();
    }
}
