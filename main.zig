const std = @import("std");
const isa = @import("./isa.zig");

fn arrayList(comptime T: type, allocator: std.mem.Allocator) !std.ArrayList(T) {
    return std.ArrayList(T).initCapacity(allocator, 0xFF);
}

const WordBuffer = struct {
    quotes: std.ArrayList(isa.Word),
    operands: std.ArrayList(isa.Operand),
    addresses: std.ArrayList(isa.Address),
    code: std.ArrayList(u8),
    type_sig: TypeSig,

    pub fn init(allocator: std.mem.Allocator) !WordBuffer {
        return WordBuffer {
            .quotes = try arrayList(isa.Word, allocator),
            .operands = try arrayList(isa.Operand, allocator),
            .addresses = try arrayList(isa.Address, allocator),
            .code = try arrayList(u8, allocator),
            .type_sig = TypeSig.static(0, 0)
        };
    }

    pub fn toEntryWord(self: *@This()) !isa.Word {
        try self.code.append(@enumToInt(isa.OpCode.end));
        return isa.Word {
            .quotes = self.quotes.items,
            .operands = self.operands.items,
            .addresses = self.addresses.items,
            .code = self.code.items,
        };
    }

    pub fn toUserWord(self: *@This()) !isa.Word {
        try self.code.append(@enumToInt(isa.OpCode.ret));
        return isa.Word {
            .quotes = self.quotes.toOwnedSlice(),
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
                    @enumToInt(isa.OpCode.call),
                    @intCast(u8, entry.impl.address)
                });
                try self.addresses.append(entry.impl.address);
            }   
        }
    }

    pub fn writeOperand(self: *@This(), operand: isa.Operand) !void {
        self.type_sig = self.type_sig.compose(TypeSig.static(0, 1));

        try self.code.appendSlice(&[_]u8{
            @enumToInt(isa.OpCode.push),
            @intCast(u8, self.operands.items.len)
        });
        try self.operands.append(operand);
    }

    pub fn clear(self: *@This()) void {
        self.operands.clearRetainingCapacity();
        self.addresses.clearRetainingCapacity();
        self.code.clearRetainingCapacity();
        self.type_sig = TypeSig.static(0, 0);
    }
};

const DictEntryTag = enum {
    op_code,
    address,
};

const DictEntry = struct {
    type_sig: TypeSig,
    impl: union(DictEntryTag) {
        op_code: isa.OpCode,
        address: isa.Address
    }
};

const Dict = std.StringHashMap(DictEntry);

// Some of these are dynamically typed for now.
const std_library = [_]std.meta.Tuple(&.{ []const u8, TypeSig, isa.OpCode}) {
    .{"+",      TypeSig.static(2, 1),   .add},
    .{"-",      TypeSig.static(2, 1),   .sub},
    .{"*",      TypeSig.static(2, 1),   .mul},
    .{"/",      TypeSig.static(2, 1),   .div},
    .{"dup",    TypeSig.static(1, 2),   .dup},
    .{"drop",   TypeSig.static(1, 0),   .drop},
    .{"bi@",    TypeSig.static(3, 2),   .bi_at},
};

const TypeSig = struct {
    in: i8,
    out: i8,

    pub fn static(in: i8, out: i8) TypeSig {
        return .{.in = in, .out = out};
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
    vm: isa.VM,
    dict: Dict,
    output_buffer: std.ArrayList(u8),
    entry_word_buffer: WordBuffer,
    new_word_buffer: WordBuffer,
    new_word_name: []const u8,
    quote_buffer: WordBuffer,
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
            .vm = try isa.VM.init(allocator),
            .dict = dict,
            .output_buffer = try arrayList(u8, allocator),
            .entry_word_buffer = try WordBuffer.init(allocator),
            .new_word_buffer = try WordBuffer.init(allocator),
            .new_word_name = "",
            .quote_buffer = try WordBuffer.init(allocator),
            .allocator = allocator,
        };
    }

    pub fn eval(self: *@This(), s: []const u8) ![]const u8 {
        if (s.len == 0) return s;


        self.entry_word_buffer.clear();
        self.output_buffer.clearRetainingCapacity();
        const writer = self.output_buffer.writer();
        
        var tokens = std.mem.tokenize(u8, s, &std.ascii.spaces);

        var word_buffer: *WordBuffer = &self.entry_word_buffer;
        var prev_word_buffer: *WordBuffer = undefined;

        while (tokens.next()) |token| {
            if (std.mem.eql(u8, token, ":")) {
                prev_word_buffer = word_buffer;
                word_buffer = &self.new_word_buffer;

                if (tokens.next()) |name| {
                    self.new_word_name = name;
                }
            } else if (std.mem.eql(u8, token, ";")) {
                const copied_name = 
                    try self.allocator.dupe(u8, self.new_word_name);

                const addr = try self.vm.store(try word_buffer.toUserWord());

                try self.dict.put(copied_name, .{
                    .type_sig = word_buffer.type_sig,
                    .impl = .{.address = addr}
                });

                try writer.print("{s} : {s}\n", .{
                    self.new_word_name,
                    self.new_word_buffer.type_sig
                });

                self.new_word_buffer.clear();
                word_buffer = prev_word_buffer;
            } else if (std.mem.eql(u8, token, "[")) {
                prev_word_buffer = word_buffer;
                word_buffer = &self.quote_buffer;
            } else if (std.mem.eql(u8, token, "]")) {
                const addr = @intCast(u8, prev_word_buffer.quotes.items.len);

                try prev_word_buffer.quotes.append(
                    try word_buffer.toUserWord());

                try word_buffer.writeOperand(.{.quote = addr});
                word_buffer = prev_word_buffer;
            } else if (self.dict.get(token)) |entry| {
                try word_buffer.writeEntry(entry);
            } else if (std.fmt.parseFloat(isa.Num, token)) |num| {
                try word_buffer.writeOperand(.{.num = num});
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
            const stack_len = self.vm.ds.items.len;

            if (self.entry_word_buffer.type_sig.in > stack_len) {
                return "type err\n";
            }

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
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    var allocator = arena.allocator();
    var interpreter = try Interpreter.init(allocator);
    
    return std.testing.expectEqualStrings(
        expected, try interpreter.eval(actual));

}

fn test_multiline_eval(
    output: []const []const u8,
    input: []const []const u8
) !void {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    var allocator = arena.allocator();
    var interpreter = try Interpreter.init(allocator);
    var actual_buf = try arrayList([]const u8, allocator);
    
    var expected_buf = try arrayList(u8, allocator);
    
    for (input) |s| {
        const eval_ouput = try interpreter.eval(s);
        try actual_buf.append(try allocator.dupe(u8, eval_ouput));
    }

    for (output) |s| {
        try expected_buf.writer().print("{s}\n", .{s});
    }

    const actual = try std.mem.join(allocator, "", actual_buf.items);

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

test "type error when not enough items on stack" {
    try test_eval("type err\n", "1 +");
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

test "sum of squares" {
    const input = .{
        ": square dup * ;",
        ": sum-of-square [ square ] bi@ + ;",
        "3 4 sum-of-square"
    };

    const output = .{
        "square : num -> num",
        "sum-of-square : num num -> num",
        "25 : num"
    };

    try test_multiline_eval(&output, &input);
}
// [ square ] bi@ +
// (define (sum-of-squares x y)
// (+ (square x) (square y)))

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();

    var allocator = arena.allocator();

    var interpreter = try Interpreter.init(allocator);

    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    var input: std.ArrayList(u8) = try arrayList(u8, allocator);

    try stdout.print("New Wave\n", .{});

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
