const std = @import("std");
const assert = std.debug.assert;

const Op = enum { Add, Sub, Mul, Div, None };

const Command = struct {
    lhs: u8,
    a: u8,
    b: u8,
    op: Op,
    idx: usize,
};

const Block = struct {
    commands: std.ArrayList(Command),

    pub fn init(allocator: std.mem.Allocator) @This() {
        return .{ .commands = std.ArrayList(Command).init(allocator) };
    }
    pub fn deinit(self: *@This()) void {
        self.commands.deinit();
    }
};

const Cfg = struct {
    entry: usize = 0,
    exit: usize = 1,
    blocks: std.ArrayList(Block),
    buf_size: usize = 10,
    pred_ids: std.ArrayList(std.DynamicBitSet),
    succ_ids: std.ArrayList(std.DynamicBitSet),

    dfa_in_sets: std.ArrayList(std.DynamicBitSet),
    dfa_out_sets: std.ArrayList(std.DynamicBitSet),
    dfa_gen_sets: std.ArrayList(std.DynamicBitSet),
    dfa_kill_sets: std.ArrayList(std.DynamicBitSet),

    temp_gen: std.DynamicBitSet,
    temp_kill: std.DynamicBitSet,
    temp: std.DynamicBitSet,

    allocator: std.mem.Allocator,

    const Self = @This();

    pub fn deinit(self: *@This()) void {
        for (self.blocks.items) |*b| b.deinit();
        for (self.pred_ids.items) |*b| b.deinit();
        for (self.succ_ids.items) |*b| b.deinit();
        for (self.dfa_in_sets.items) |*b| b.deinit();
        for (self.dfa_out_sets.items) |*b| b.deinit();
        for (self.dfa_gen_sets.items) |*b| b.deinit();
        for (self.dfa_kill_sets.items) |*b| b.deinit();
        self.blocks.deinit();
        self.pred_ids.deinit();
        self.succ_ids.deinit();
        self.dfa_in_sets.deinit();
        self.dfa_out_sets.deinit();
        self.dfa_gen_sets.deinit();
        self.dfa_kill_sets.deinit();
        self.temp.deinit();
        self.temp_gen.deinit();
        self.temp_kill.deinit();
    }

    inline fn initSetTo(set: *std.DynamicBitSet, val: *const std.DynamicBitSet) !void {
        const new_len = val.unmanaged.bit_length;
        try Self.initSetToEmpty(set, new_len);
        set.setUnion(val.*);
    }
    fn initSetToEmpty(set: *std.DynamicBitSet, new_len: usize) !void {
        const old_len = set.unmanaged.bit_length;

        if (old_len != new_len) {
            try set.resize(new_len, false);
        }
        set.setRangeValue(.{ .start = 0, .end = new_len }, false);
    }
    fn initSetToFull(set: *std.DynamicBitSet, new_len: usize) !void {
        const old_len = set.unmanaged.bit_length;

        if (old_len != new_len) {
            try set.resize(new_len, true);
        }
        set.setRangeValue(.{ .start = 0, .end = new_len }, false);
    }

    pub const TransferFn = fn (
        *const Block,
        *const std.DynamicBitSet, // cur
        *std.DynamicBitSet, // GEN
        *std.DynamicBitSet, // KILL
        anytype, // extra
    ) void;
    pub const MeetFn = fn (
        *std.DynamicBitSet, // result of MEET
        *const std.DynamicBitSet, // inds of args
        []std.DynamicBitSet, // all possible args
    ) void;
    pub const Dir = enum { Forward, Backward };
    pub fn dfa(
        self: *Self,
        edge: std.DynamicBitSet,
        default: std.DynamicBitSet,
        dir: Dir,
        transfer: TransferFn,
        extra: anytype,
        meet: MeetFn,
    ) !void {
        const cnt = edge.unmanaged.bit_length;
        assert(cnt == default.unmanaged.bit_length);
        for (self.dfa_out_sets.items) |*old| try Self.initSetToEmpty(old, cnt);
        for (self.dfa_in_sets.items) |*old| try Self.initSetToEmpty(old, cnt);
        for (self.dfa_gen_sets.items) |*old| try Self.initSetToEmpty(old, cnt);
        for (self.dfa_kill_sets.items) |*old| try Self.initSetToEmpty(old, cnt);

        if (dir == .Forward) {
            for (self.dfa_out_sets.items) |*set| try Self.initSetTo(set, &default);
            try Self.initSetTo(&self.dfa_out_sets.items[self.entry], &edge);

            var changed = true;
            while (changed) {
                changed = false;
                for (self.blocks.items[1..], 1..) |*block, cur| {
                    const cur_in = &self.dfa_in_sets.items[cur];
                    const cur_out = &self.dfa_out_sets.items[cur];

                    try Self.initSetToEmpty(&self.temp, cnt);
                    try Self.initSetToEmpty(&self.temp_gen, cnt);
                    try Self.initSetToEmpty(&self.temp_kill, cnt);

                    meet(cur_in, &self.pred_ids.items[cur], self.dfa_out_sets.items);
                    transfer(block, cur_in, &self.temp_gen, &self.temp_kill, extra);

                    self.dfa_gen_sets.items[cur].setUnion(self.temp_gen);
                    self.dfa_kill_sets.items[cur].setUnion(self.temp_kill);

                    try Self.initSetTo(&self.temp, cur_in);
                    self.temp_kill.toggleAll();
                    self.temp.setIntersection(self.temp_kill);
                    self.temp.setUnion(self.temp_gen);

                    if (!self.temp.eql(cur_out.*)) {
                        changed = true;
                        try Self.initSetTo(cur_out, &self.temp);
                    }
                }
            }
        } else if (dir == .Backward) {
            for (self.dfa_in_sets.items) |*set| try Self.initSetTo(set, &default);
            try Self.initSetTo(&self.dfa_in_sets.items[self.exit], &edge);

            var changed = true;
            while (changed) {
                changed = false;
                for (self.blocks.items, 0..) |*block, cur| {
                    if (cur == self.exit) continue;

                    const cur_in = &self.dfa_in_sets.items[cur];
                    const cur_out = &self.dfa_out_sets.items[cur];

                    try Self.initSetToEmpty(&self.temp, cnt);
                    try Self.initSetToEmpty(&self.temp_gen, cnt);
                    try Self.initSetToEmpty(&self.temp_kill, cnt);

                    meet(cur_out, &self.succ_ids.items[cur], self.dfa_in_sets.items);
                    transfer(block, cur_out, &self.temp_gen, &self.temp_kill, extra);

                    self.dfa_gen_sets.items[cur].setUnion(self.temp_gen);
                    self.dfa_kill_sets.items[cur].setUnion(self.temp_kill);

                    try Self.initSetTo(&self.temp, cur_out);
                    self.temp_kill.toggleAll();
                    self.temp.setIntersection(self.temp_kill);
                    self.temp.setUnion(self.temp_gen);

                    if (!self.temp.eql(cur_in.*)) {
                        changed = true;
                        try Self.initSetTo(cur_in, &self.temp);
                    }
                }
            }
        }
    }

    inline fn printSet(writer: std.io.AnyWriter, set: *const std.DynamicBitSet) !void {
        try writer.print("[", .{});
        var it = set.iterator(.{});
        while (it.next()) |idx| {
            try writer.print("{}, ", .{idx});
        }
        try writer.print("]", .{});
    }

    pub const NameFn = fn (std.io.AnyWriter, usize, anytype) anyerror!void;
    fn printSetRenamed(
        writer: std.io.AnyWriter,
        set: *const std.DynamicBitSet,
        name_fn: NameFn,
        extra: anytype,
    ) !void {
        try writer.print("[", .{});
        var it = set.iterator(.{});
        while (it.next()) |idx| {
            try name_fn(writer, idx, extra);
            try writer.print(", ", .{});
        }
        try writer.print("]", .{});
    }

    pub fn dump(self: *const Self, writer: std.io.AnyWriter, name_fn: NameFn, extra: anytype) !void {
        try writer.print("digraph {{\nnode [shape=record];\n", .{});
        try writer.print("{} [label=\"ENTRY\"];\n{} [label=\"EXIT\"];\n", .{ self.entry, self.exit });

        for (self.blocks.items, 0..) |*block, idx| {
            if (idx == self.entry or idx == self.exit) {
                continue;
            }
            const in = &self.dfa_in_sets.items[idx];
            const out = &self.dfa_out_sets.items[idx];
            const gen = &self.dfa_gen_sets.items[idx];
            const kill = &self.dfa_kill_sets.items[idx];

            try writer.print("{} [label=<{{Block {}|", .{ idx, idx });
            for (block.commands.items) |cmd| {
                try writer.print("({}) {c} = {c}", .{ cmd.idx, cmd.lhs, cmd.a });
                switch (cmd.op) {
                    .Add => try writer.print(" + {c};", .{cmd.b}),
                    .Sub => try writer.print(" - {c};", .{cmd.b}),
                    .Mul => try writer.print(" * {c};", .{cmd.b}),
                    .Div => try writer.print(" / {c};", .{cmd.b}),
                    .None => try writer.print(";", .{}),
                }
            }
            try writer.print("|{{in:", .{});
            try Self.printSetRenamed(writer, in, name_fn, extra);
            try writer.print("|out:", .{});
            try Self.printSetRenamed(writer, out, name_fn, extra);
            try writer.print("}}|{{gen:", .{});
            try Self.printSetRenamed(writer, gen, name_fn, extra);
            try writer.print("|kill:", .{});
            try Self.printSetRenamed(writer, kill, name_fn, extra);

            try writer.print("}}}}>];\n", .{});
        }

        for (self.succ_ids.items, 0..) |*set, idx| {
            var next_it = set.iterator(.{});
            while (next_it.next()) |i| {
                try writer.print("{} -> {};\n", .{ idx, i });
            }
        }

        try writer.print("}}\n", .{});
    }

    pub fn init(allocator: std.mem.Allocator) !Self {
        var self = Self{
            .blocks = std.ArrayList(Block).init(allocator),
            .pred_ids = std.ArrayList(std.DynamicBitSet).init(allocator),
            .succ_ids = std.ArrayList(std.DynamicBitSet).init(allocator),
            .dfa_in_sets = std.ArrayList(std.DynamicBitSet).init(allocator),
            .dfa_out_sets = std.ArrayList(std.DynamicBitSet).init(allocator),
            .dfa_gen_sets = std.ArrayList(std.DynamicBitSet).init(allocator),
            .dfa_kill_sets = std.ArrayList(std.DynamicBitSet).init(allocator),
            .temp = try std.DynamicBitSet.initEmpty(allocator, 10),
            .temp_gen = try std.DynamicBitSet.initEmpty(allocator, 10),
            .temp_kill = try std.DynamicBitSet.initEmpty(allocator, 10),
            .allocator = allocator,
        };
        self.entry = try self.addBlock();
        self.exit = try self.addBlock();

        return self;
    }

    pub fn addBlock(self: *Self) !usize {
        const idx = self.blocks.items.len;
        try self.blocks.append(Block.init(self.allocator));
        if (self.blocks.items.len >= self.buf_size) {
            self.buf_size *= 2;
            for (self.succ_ids.items) |*set| {
                try set.resize(self.buf_size, false);
            }
            for (self.pred_ids.items) |*set| {
                try set.resize(self.buf_size, false);
            }
        }
        try self.succ_ids.append(try std.DynamicBitSet.initEmpty(self.allocator, self.buf_size));
        try self.pred_ids.append(try std.DynamicBitSet.initEmpty(self.allocator, self.buf_size));

        try self.dfa_in_sets.append(try std.DynamicBitSet.initEmpty(self.allocator, 10));
        try self.dfa_out_sets.append(try std.DynamicBitSet.initEmpty(self.allocator, 10));
        try self.dfa_gen_sets.append(try std.DynamicBitSet.initEmpty(self.allocator, 10));
        try self.dfa_kill_sets.append(try std.DynamicBitSet.initEmpty(self.allocator, 10));

        return idx;
    }
    pub inline fn addCommand(self: *Self, cmd: Command, block: usize) !void {
        try self.blocks.items[block].commands.append(cmd);
    }

    pub inline fn connectBlocks(self: *Self, from: usize, to: usize) void {
        self.succ_ids.items[from].set(to);
        self.pred_ids.items[to].set(from);
    }
    pub inline fn setExit(self: *Self, block: usize) void {
        self.connectBlocks(block, self.exit);
    }
    pub inline fn setEntry(self: *Self, block: usize) void {
        self.connectBlocks(self.entry, block);
    }
};

fn union_meet(
    res: *std.DynamicBitSet,
    args: *const std.DynamicBitSet,
    sets: []std.DynamicBitSet,
) void {
    var it = args.iterator(.{});
    if (it.next()) |first| {
        Cfg.initSetTo(res, &sets[first]) catch unreachable;
        while (it.next()) |next| res.setUnion(sets[next]);
    } else {
        Cfg.initSetToEmpty(res, res.unmanaged.bit_length) catch unreachable;
    }
}
fn intersect_meet(
    res: *std.DynamicBitSet,
    args: *const std.DynamicBitSet,
    sets: []std.DynamicBitSet,
) void {
    var it = args.iterator(.{});
    if (it.next()) |first| {
        Cfg.initSetTo(res, &sets[first]) catch unreachable;
        while (it.next()) |next| res.setIntersection(sets[next]);
    } else {
        Cfg.initSetToEmpty(res, res.unmanaged.bit_length) catch unreachable;
    }
}

fn rd_transfer(
    block: *const Block,
    in: *const std.DynamicBitSet,
    gen: *std.DynamicBitSet,
    kill: *std.DynamicBitSet,
    extra: anytype,
) void {
    const cmds = @as([]Command, extra);

    for (block.commands.items) |cmd| {
        var in_it = in.iterator(.{});
        while (in_it.next()) |i| {
            if (cmds[i].lhs == cmd.lhs) kill.set(i);
        }
        var gen_it = gen.iterator(.{});
        while (gen_it.next()) |i| {
            if (cmds[i].lhs == cmd.lhs) gen.unset(i);
        }

        kill.unset(cmd.idx);
        gen.set(cmd.idx);
    }
}

const AeMaps = struct {
    const BinExpr = struct { u8, Op, u8 };
    exprs: []BinExpr,
    expr_ids: []usize,
};
fn ae_transfer(
    block: *const Block,
    in: *const std.DynamicBitSet,
    gen: *std.DynamicBitSet,
    kill: *std.DynamicBitSet,
    extra: anytype,
) void {
    const maps = @as(AeMaps, extra);

    for (block.commands.items) |cmd| {
        gen.set(maps.expr_ids[cmd.idx]);
        kill.unset(maps.expr_ids[cmd.idx]);

        var in_it = in.iterator(.{});
        while (in_it.next()) |i| {
            const expr = maps.exprs[i];
            if (expr[0] == cmd.lhs or expr[1] != .None and expr[2] == cmd.lhs) {
                kill.set(i);
            }
        }
        var gen_it = gen.iterator(.{});
        while (gen_it.next()) |i| {
            const expr = maps.exprs[i];
            if (expr[0] == cmd.lhs or expr[1] != .None and expr[2] == cmd.lhs) {
                gen.unset(i);
            }
        }
    }
}

// def: set of all vars defined without a previous usage in that block
// use: set of all vars used without a previous definition in that block
fn lv_transfer(
    block: *const Block,
    out: *const std.DynamicBitSet,
    use: *std.DynamicBitSet,
    def: *std.DynamicBitSet,
    extra: anytype,
) void {
    _ = out;
    _ = extra;
    const cnt = def.unmanaged.bit_length;
    Cfg.initSetToEmpty(def, cnt) catch unreachable;
    Cfg.initSetToEmpty(use, cnt) catch unreachable;
    for (block.commands.items, 0..) |_, i| {
        const cmd = block.commands.items[block.commands.items.len - i - 1];
        use.unset(cmd.lhs);
        def.unset(cmd.a);
        if (cmd.op != .None) def.unset(cmd.b);

        use.set(cmd.a);
        if (cmd.op != .None) use.set(cmd.b);
        def.set(cmd.lhs);
    }
}

fn normal_name(writer: std.io.AnyWriter, idx: usize, extra: anytype) anyerror!void {
    _ = extra;
    try writer.print("{}", .{idx});
}
fn char_name(writer: std.io.AnyWriter, idx: usize, extra: anytype) anyerror!void {
    _ = extra;
    try writer.print("{c}", .{@as(u8, @intCast(idx))});
}

fn expr_name(writer: std.io.AnyWriter, idx: usize, extra: anytype) anyerror!void {
    const maps = @as(AeMaps, extra);
    const expr = maps.exprs[idx];

    try writer.print("'{c}", .{expr[0]});
    switch (expr[1]) {
        .Add => try writer.print(" + {c}'", .{expr[2]}),
        .Sub => try writer.print(" - {c}'", .{expr[2]}),
        .Mul => try writer.print(" * {c}'", .{expr[2]}),
        .Div => try writer.print(" / {c}'", .{expr[2]}),
        .None => try writer.print("'", .{}),
    }
}

pub fn main() !void {
    var alloc = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.print("GPA: {}\n", .{alloc.deinit()});

    var cfg = try Cfg.init(alloc.allocator());
    defer cfg.deinit();

    var commands = try std.ArrayList(Command).initCapacity(alloc.allocator(), 12);
    defer commands.deinit();
    try commands.append(undefined);

    try commands.append(.{ .lhs = 'a', .a = '1', .op = .None, .b = 0, .idx = 1 });
    try commands.append(.{ .lhs = 'b', .a = '2', .op = .None, .b = 0, .idx = 2 });
    try commands.append(.{ .lhs = 'c', .a = 'a', .op = .Add, .b = 'b', .idx = 3 });
    try commands.append(.{ .lhs = 'd', .a = 'c', .op = .Sub, .b = 'a', .idx = 4 });
    try commands.append(.{ .lhs = 'd', .a = 'b', .op = .Add, .b = 'd', .idx = 5 });
    try commands.append(.{ .lhs = 'd', .a = 'a', .op = .Add, .b = 'b', .idx = 6 });
    try commands.append(.{ .lhs = 'e', .a = 'e', .op = .Add, .b = '1', .idx = 7 });
    try commands.append(.{ .lhs = 'b', .a = 'a', .op = .Add, .b = 'd', .idx = 8 });
    try commands.append(.{ .lhs = 'e', .a = 'c', .op = .Sub, .b = 'a', .idx = 9 });
    try commands.append(.{ .lhs = 'a', .a = 'b', .op = .Mul, .b = 'd', .idx = 10 });
    try commands.append(.{ .lhs = 'b', .a = 'a', .op = .Sub, .b = 'd', .idx = 11 });

    const b1 = try cfg.addBlock();
    try cfg.addCommand(commands.items[1], b1);
    try cfg.addCommand(commands.items[2], b1);
    const b2 = try cfg.addBlock();
    try cfg.addCommand(commands.items[3], b2);
    try cfg.addCommand(commands.items[4], b2);
    const b3 = try cfg.addBlock();
    try cfg.addCommand(commands.items[5], b3);
    const b4 = try cfg.addBlock();
    try cfg.addCommand(commands.items[6], b4);
    try cfg.addCommand(commands.items[7], b4);
    const b5 = try cfg.addBlock();
    try cfg.addCommand(commands.items[8], b5);
    try cfg.addCommand(commands.items[9], b5);
    const b6 = try cfg.addBlock();
    try cfg.addCommand(commands.items[10], b6);
    try cfg.addCommand(commands.items[11], b6);
    cfg.setEntry(b1);
    cfg.connectBlocks(b1, b2);
    cfg.connectBlocks(b2, b3);
    cfg.connectBlocks(b3, b4);
    cfg.connectBlocks(b4, b3);
    cfg.connectBlocks(b3, b5);
    cfg.connectBlocks(b5, b2);
    cfg.connectBlocks(b5, b6);
    cfg.setExit(b6);

    {
        var empty = try std.DynamicBitSet.initEmpty(alloc.allocator(), 12);
        defer empty.deinit();
        try cfg.dfa(empty, empty, .Forward, rd_transfer, commands.items, union_meet);
        var rd_file = try std.fs.cwd().createFile("rd.dot", .{});
        defer rd_file.close();
        try cfg.dump(rd_file.writer().any(), normal_name, .{});
    }
    {
        var empty = try std.DynamicBitSet.initEmpty(alloc.allocator(), 256);
        defer empty.deinit();
        try cfg.dfa(empty, empty, .Backward, lv_transfer, .{}, union_meet);
        var rd_file = try std.fs.cwd().createFile("lv.dot", .{});
        defer rd_file.close();
        try cfg.dump(rd_file.writer().any(), char_name, .{});
    }
    {
        var expr_ids = std.ArrayList(usize).init(alloc.allocator());
        defer expr_ids.deinit();
        var exprs = std.ArrayList(AeMaps.BinExpr).init(alloc.allocator());
        defer exprs.deinit();
        try expr_ids.resize(12);

        expr_ids.items[1] = 1;
        expr_ids.items[2] = 2;
        expr_ids.items[3] = 3;
        expr_ids.items[4] = 4;
        expr_ids.items[5] = 5;
        expr_ids.items[6] = 3;
        expr_ids.items[7] = 6;
        expr_ids.items[8] = 7;
        expr_ids.items[9] = 4;
        expr_ids.items[10] = 8;
        expr_ids.items[11] = 9;

        try exprs.resize(10);
        exprs.items[1] = .{ '1', .None, 0 };
        exprs.items[2] = .{ '2', .None, 0 };
        exprs.items[3] = .{ 'a', .Add, 'b' };
        exprs.items[4] = .{ 'c', .Sub, 'a' };
        exprs.items[5] = .{ 'b', .Add, 'd' };
        exprs.items[6] = .{ 'e', .Add, '1' };
        exprs.items[7] = .{ 'a', .Add, 'd' };
        exprs.items[8] = .{ 'b', .Mul, 'd' };
        exprs.items[9] = .{ 'a', .Sub, 'd' };

        const maps = AeMaps{
            .expr_ids = expr_ids.items,
            .exprs = exprs.items,
        };
        var full = try std.DynamicBitSet.initFull(alloc.allocator(), 10);
        defer full.deinit();
        full.unset(0);
        var empty = try std.DynamicBitSet.initEmpty(alloc.allocator(), 10);
        defer empty.deinit();
        try cfg.dfa(empty, full, .Forward, ae_transfer, maps, intersect_meet);
        var rd_file = try std.fs.cwd().createFile("ae.dot", .{});
        defer rd_file.close();
        try cfg.dump(rd_file.writer().any(), expr_name, maps);
    }
}
