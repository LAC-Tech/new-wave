//const mem = @import("mem");
const std = @import("std");
const expectStrings = std.testing.expectEqualStrings;

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
    type_sigs: std.ArrayList(TypeSig),

    pub fn init(allocator: std.mem.Allocator) !WordBuffer {
        return WordBuffer {
            .operands = try arrayList(Operand, allocator),
            .addresses = try arrayList(Address, allocator),
            .code = try arrayList(u8, allocator),
            .type_sigs = try arrayList(TypeSig, allocator)
        };
    }

    pub fn deinit(self: @This()) void {
        self.operands.deinit();
        self.addresses.deinit();
        self.code.deinit();
        self.type_sigs.deinit();
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
        try self.type_sigs.append(entry.type_sig); 

        const impl = entry.implementation;
        switch(entry.implementation) {
            .op_code => {
                try self.code.append(@enumToInt(impl.op_code));
            },
            .address => {
                try self.code.appendSlice(&[_]u8{
                    @enumToInt(OpCode.call),
                    @intCast(u8, impl.address)
                });
                try self.addresses.append(impl.address);
            }   
        }
    }

    pub fn writeOperand(self: *@This(), operand: Operand) !void {
        try self.type_sigs.append(TypeSig {
            .inputs = 0,
            .outputs = 1
        });

        try self.code.appendSlice(&[_]u8{
            @enumToInt(OpCode.push),
            @intCast(u8, self.operands.items.len)
        });
        try self.operands.append(operand);
    }

    pub fn typeSig(self: @This()) TypeSigErr!TypeSig {
        std.debug.print("\nType check begins...\n", .{});
        std.debug.print("\ncode...{d}\n", .{self.code.items});
        std.debug.print("\nall type sigs...{s}\n", .{self.type_sigs.items});

        const tss = self.type_sigs.items;

        if (tss.len == 0) return TypeSigErr.Empty;
        
        var result = tss[0];

        //std.debug.print("\n first type sig {s}\n", .{result});

        // curr = (0, 1), next = (0, 1)
        // curr = (0, 2), next = (2, 1)


        var i: usize = 1;
        while (i < tss.len) : (i += 1) {
            std.debug.print("\nResult before: {s}...\n", .{result});

            const curr = tss[i];

            if (result.outputs < curr.inputs) {
                return TypeSigErr.ArityMismatch;
            }

            result = TypeSig {
                .inputs = result.outputs - curr.inputs,
                .outputs = result.outputs + curr.outputs
            };

            std.debug.print("\nResult after: {s}...\n", .{result});
         

        }    

        return result;
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

    pub fn print(self: *@This(), output_buffer: *std.ArrayList(u8)) !void {
        const writer = output_buffer.writer();

        // TODO: assumes stack length > 1
        try writer.print("{d}", .{self.ds.items[0]});

        if (self.ds.items.len > 1) {
            for (self.ds.items[1..]) |d| {
                try writer.print(" {d}", .{d});
            }
        }
    }
};

test "can init and deinit VM" {
    const vm = try VM.init(std.testing.allocator);
    vm.deinit();
}

const DictEntryTag = enum {
    op_code,
    address,
};

const DictEntry = struct {
    type_sig: TypeSig,
    implementation: union(DictEntryTag) {
        op_code: OpCode,
        address: Address
    }
};

const Dict = std.StringHashMap(DictEntry);

const BuiltIns = struct {
    const add = DictEntry {
        .type_sig = TypeSig {.inputs = 2, .outputs = 1},
        .implementation = .{.op_code = OpCode.add}
    };

    const sub = DictEntry {
        .type_sig = TypeSig {.inputs = 2, .outputs = 1},
        .implementation = .{.op_code = OpCode.sub}
    };

    const mul = DictEntry {
        .type_sig = TypeSig {.inputs = 2, .outputs = 1},
        .implementation = .{.op_code = OpCode.mul}
    };

    const div = DictEntry {
        .type_sig = TypeSig {.inputs = 2, .outputs = 1},
        .implementation = .{.op_code = OpCode.div}
    };

    const dup = DictEntry{
        .type_sig = TypeSig {.inputs = 1, .outputs = 2},
        .implementation = .{.op_code = OpCode.dup}
    };
};

const std_library = [_]std.meta.Tuple(&.{ []const u8, DictEntry }) {
    .{"+",      BuiltIns.add},
    .{"-",      BuiltIns.sub},
    .{"*",      BuiltIns.mul},
    .{"/",      BuiltIns.div},
    .{"dup",    BuiltIns.dup}
};

const TypeSigErr = error { Empty, ArityMismatch };

const TypeSig = struct {
    inputs: u8,
    outputs: u8,

    pub fn format(
        value: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype
    ) !void {

        //(inputs -> outputs)


        try writer.print("{d} -> {d}", .{value.inputs, value.outputs});

    }
};

const Interpreter = struct {
    vm: VM,
    dict: Dict,
    output_buffer: std.ArrayList(u8),
    entry_word_buffer: WordBuffer,
    new_word_buffer: WordBuffer,
    new_word_name: []const u8,

    pub fn init(allocator: std.mem.Allocator) !Interpreter {
        var dict = Dict.init(allocator);

        for (std_library) |sl| {
            try dict.put(sl[0], sl[1]);
        }

        return Interpreter {
            .vm = try VM.init(allocator),
            .dict = dict,
            .output_buffer = try arrayList(u8, allocator),
            .entry_word_buffer = try WordBuffer.init(allocator),
            .new_word_buffer = try WordBuffer.init(allocator),
            .new_word_name = ""
        };
    }

    pub fn deinit(self: *@This()) void {
        self.vm.deinit();
        self.dict.deinit();
        self.output_buffer.deinit();
        self.entry_word_buffer.deinit();
        self.new_word_buffer.deinit();
    }

    pub fn eval(self: *@This(), s: []const u8) ![]const u8 {
        if (s.len == 0) return s;

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
                    .type_sig = TypeSig {.inputs = 0, .outputs = 0},
                    .implementation = .{
                        .address = addr    
                    }
                };
                    
                try self.dict.put(self.new_word_name, entry);
                word_buffer = &self.entry_word_buffer;
            } else if (self.dict.get(token)) |entry| {
                try word_buffer.writeEntry(entry);
            } else if (std.fmt.parseFloat(Operand, token)) |operand| {
                try word_buffer.writeOperand(operand);
            } else |_| {
                std.debug.print("can't find token: {s}", .{token});
                return "unknown token";
            }
        }

        const ts = try self.entry_word_buffer.typeSig();
        std.debug.print("\n type of entry: {s}\n", .{ts});

        const entry_word = try self.entry_word_buffer.toEntryWord();

        try self.vm.exec(&entry_word);
        try self.vm.print(&self.output_buffer);
        return self.output_buffer.items;
    }
};

fn test_eval(actual: []const u8, expected: []const u8) !void {
    var interpreter = try Interpreter.init(std.testing.allocator);
    defer interpreter.deinit();
    return expectStrings(actual, try interpreter.eval(expected));
}

test "bug" {
    const foo = [_]std.meta.Tuple(&.{ []const u8, i32 }) {
        .{"one", 1},
        .{"two", 2},
        .{"three"}
    };

    for (foo) |f| {
        std.debug.print("{d}", .{f[1]});
    }

    try std.testing.expectEqual(foo[0][1], 1);
}

// test "detects stack underflow" {
//     try test_eval("ArityMismatch", "1 +");
// }

// test "infer type of constant" {
//     var wb = try WordBuffer.init(std.testing.allocator);
//     defer wb.deinit();

//     try wb.writeOperand(42.0);
//     const ts = try wb.typeSig();

//     try std.testing.expectEqual(@as(u8, 0), ts.inputs);
//     try std.testing.expectEqual(@as(u8, 1), ts.outputs);
// }

// test "infer type of square" {
//     var wb = try WordBuffer.init(std.testing.allocator);
//     defer wb.deinit();

//     try wb.writeEntry(BuiltIns.dup);
//     try wb.writeEntry(BuiltIns.mul);
    
//     const ts = try wb.typeSig();

//     try std.testing.expectEqual(TypeSig {.inputs = 1, .outputs = 1}, ts);
// }

// test "infer type of cube" {
//     var wb = try WordBuffer.init(std.testing.allocator);
//     defer wb.deinit();

//     try wb.writeEntry(BuiltIns.dup);
//     try wb.writeEntry(BuiltIns.dup);
//     try wb.writeEntry(BuiltIns.mul);
//     try wb.writeEntry(BuiltIns.mul);
    
//     const ts = try wb.typeSig();

//     try std.testing.expectEqual(TypeSig {.inputs = 1, .outputs = 1}, ts);
// }

// SICP Tests

// test "eval empty input" {
//     try test_eval("", "");
// }

// test "primitive expression" {
//     try test_eval("486", "486");
// }

// test "add ints" {
//     try test_eval("486", "137 349 +");
// }

// test "subtract ints" {
//     try test_eval("666", "1000 334 -");
// }

// test "divide ints" {
//     try test_eval("2", "10 5 /");
// }

// test "add real to int" {
//     try test_eval("12.7", "2.7 10 +");
// }

// test "add multiple ints" {
//     try test_eval("75", "21 35 + 12 + 7 +");
// }

// test "multiply multiple ints" {
//     try test_eval("1200", "25 4 * 12 *");
// }

// test "nested combinations" {
//     try test_eval("19", "3 5 * 10 6 - +");
// }

// test "relatively simple expressions" {
//     try test_eval("57", "3 2 4 * 3 5 + + * 10 7 - 6 + +");
// }

// test "naming a value" {
//     const input = 
//         \\ : size 2 ;
//         \\ size
//         \\ 5 size *
//     ;

//     try test_eval("2 10", input);
// }

test "further examples of defining a value" {
    const input =
        \\ : pi 3.14159 ;
        \\ : radius 10 ;
        \\ radius radius * pi *
        //\\ : circumference 2 pi * radius * ;
        //\\ circumference
    ;

    try test_eval("314.159 62.8318", input);
}

// test "procedure definition" {
//     const input =
//         \\ : square dup * ;
//         \\ 21 square
//         \\ 2 5 + square
//         \\ 3 square square
//     ;

//     try test_eval("441 49 81", input);
// }

pub fn main() void {
    
}
