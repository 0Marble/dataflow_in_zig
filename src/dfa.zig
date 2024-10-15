const std = @import("std");
const cfg = @import("cfg.zig");

pub const Dir = enum { Forward, Backward };

pub fn Transfer(comptime Command: type) type {
    return struct {
        data: *anyopaque,
        fptr: *const fn (
            data: *anyopaque,
            block_idx: usize,
            block: *const cfg.Cfg(Command).Block,
            res: *std.DynamicBitSet,
            arg: *const std.DynamicBitSet,
        ) void,

        pub fn transfer(
            self: @This(),
            block_idx: usize,
            block: *const cfg.Cfg(Command).Block,
            res: *std.DynamicBitSet,
            arg: *const std.DynamicBitSet,
        ) void {
            self.fptr(self.data, block_idx, block, res, arg);
        }
    };
}

pub const Meet = struct {
    data: *anyopaque,
    fptr: *const fn (
        data: *anyopaque,
        res: *std.DynamicBitSet,
        set_ids: []usize,
        sets: []std.DynamicBitSet,
    ) void,

    pub fn meet(
        self: *const @This(),
        res: *std.DynamicBitSet,
        set_ids: []usize,
        sets: []std.DynamicBitSet,
    ) void {
        self.fptr(self.data, res, set_ids, sets);
    }
};

pub fn initSetToEmpty(set: *std.DynamicBitSet, size: usize) !void {
    try set.resize(size, false);
    set.setRangeValue(.{ .start = 0, .end = size }, false);
}

pub fn copySet(res: *std.DynamicBitSet, set: *const std.DynamicBitSet) !void {
    try initSetToEmpty(res, set.unmanaged.bit_length);
    res.setUnion(set.*);
}

pub fn printSet(w: std.io.AnyWriter, set: *const std.DynamicBitSet) !void {
    var it = set.iterator(.{});
    try w.print("[", .{});
    while (it.next()) |x| {
        try w.print("{}, ", .{x});
    }
    try w.print("]", .{});
}

pub const Dfa = struct {
    ins: std.ArrayList(std.DynamicBitSet),
    outs: std.ArrayList(std.DynamicBitSet),
    temp: std.DynamicBitSet,

    const Self = @This();
    pub fn init(
        comptime Command: type,
        graph: *const cfg.Cfg(Command),
        dir: Dir,
        transfer: Transfer(Command),
        meet: Meet,
        edge: *const std.DynamicBitSet,
        initial: *const std.DynamicBitSet,
        allocator: std.mem.Allocator,
    ) !Self {
        const cnt = edge.unmanaged.bit_length;

        var self = Self{
            .ins = std.ArrayList(std.DynamicBitSet).init(allocator),
            .outs = std.ArrayList(std.DynamicBitSet).init(allocator),
            .temp = try std.DynamicBitSet.initEmpty(allocator, cnt),
        };

        for (graph.blocks.items) |_| {
            try self.ins.append(try std.DynamicBitSet.initEmpty(allocator, cnt));
            try self.outs.append(try std.DynamicBitSet.initEmpty(allocator, cnt));
        }

        switch (dir) {
            .Forward => try self.forward(Command, graph, transfer, meet, edge, initial),
            .Backward => try self.backward(Command, graph, transfer, meet, edge, initial),
        }

        return self;
    }

    fn forward(
        self: *Self,
        comptime Command: type,
        graph: *const cfg.Cfg(Command),
        transfer: Transfer(Command),
        meet: Meet,
        edge: *const std.DynamicBitSet,
        initial: *const std.DynamicBitSet,
    ) !void {
        const cnt = edge.unmanaged.bit_length;
        for (self.outs.items) |*set| try copySet(set, initial);
        try copySet(&self.outs.items[graph.entry], edge);

        var changed = true;
        while (changed) {
            changed = false;
            for (graph.blocks.items, 0..) |*block, cur| {
                if (cur == graph.entry) continue;
                const cur_in = &self.ins.items[cur];
                const cur_out = &self.outs.items[cur];
                try initSetToEmpty(&self.temp, cnt);

                meet.meet(cur_in, graph.preds.items[cur].items, self.outs.items);
                transfer.transfer(cur, block, &self.temp, cur_in);

                if (!self.temp.eql(cur_out.*)) {
                    changed = true;
                    try copySet(cur_out, &self.temp);
                }
            }
        }
    }
    fn backward(
        self: *Self,
        comptime Command: type,
        graph: *const cfg.Cfg(Command),
        transfer: Transfer(Command),
        meet: Meet,
        edge: *const std.DynamicBitSet,
        initial: *const std.DynamicBitSet,
    ) !void {
        const cnt = edge.unmanaged.bit_length;
        for (self.ins.items) |*set| try copySet(set, initial);
        try copySet(&self.ins.items[graph.exit], edge);

        var changed = true;
        while (changed) {
            changed = false;
            for (graph.blocks.items, 0..) |*block, cur| {
                if (cur == graph.exit) continue;
                const cur_in = &self.ins.items[cur];
                const cur_out = &self.outs.items[cur];
                try initSetToEmpty(&self.temp, cnt);

                meet.meet(cur_out, graph.succs.items[cur].items, self.ins.items);
                transfer.transfer(cur, block, &self.temp, cur_out);

                if (!self.temp.eql(cur_in.*)) {
                    changed = true;
                    try copySet(cur_in, &self.temp);
                }
            }
        }
    }

    pub fn deinit(self: *Self) void {
        for (self.ins.items) |*x| x.deinit();
        for (self.outs.items) |*x| x.deinit();
        self.ins.deinit();
        self.outs.deinit();
        self.temp.deinit();
    }
};

pub const UnionMeet = struct {
    var x: void = {};
    fn unionMeet(
        data: *anyopaque,
        res: *std.DynamicBitSet,
        set_ids: []usize,
        sets: []std.DynamicBitSet,
    ) void {
        _ = data;
        if (set_ids.len == 0) {
            initSetToEmpty(res, res.unmanaged.bit_length) catch unreachable;
        } else {
            copySet(res, &sets[set_ids[0]]) catch unreachable;
            for (set_ids[1..]) |i| res.setUnion(sets[i]);
        }
    }

    pub fn meet() Meet {
        return Meet{ .data = @ptrCast(&@This().x), .fptr = @This().unionMeet };
    }
};

pub const IntersectMeet = struct {
    var x: void = {};
    fn unionMeet(
        data: *anyopaque,
        res: *std.DynamicBitSet,
        set_ids: []usize,
        sets: []std.DynamicBitSet,
    ) void {
        _ = data;
        if (set_ids.len == 0) {
            initSetToEmpty(res, res.unmanaged.bit_length) catch unreachable;
        } else {
            copySet(res, &sets[set_ids[0]]) catch unreachable;
            for (set_ids[1..]) |i| res.setUnion(sets[i]);
        }
    }

    pub fn meet() Meet {
        return Meet{ .data = @ptrCast(&@This().x), .fptr = @This().unionMeet };
    }
};

pub fn GenKillTransfer(comptime Command: type) type {
    return struct {
        const GenKillTransferFn = fn (
            data: *anyopaque,
            block_idx: usize,
            block: *const cfg.Cfg(Command).Block,
            in: *const std.DynamicBitSet,
            gen: *std.DynamicBitSet,
            kill: *std.DynamicBitSet,
        ) void;

        gens: std.ArrayList(std.DynamicBitSet),
        kills: std.ArrayList(std.DynamicBitSet),
        temp_gen: std.DynamicBitSet,
        temp_kill: std.DynamicBitSet,
        data: *anyopaque,
        fptr: *const GenKillTransferFn,

        const Self = @This();
        pub fn init(
            allocator: std.mem.Allocator,
            block_cnt: usize,
            size: usize,
            data: *anyopaque,
            fptr: *const GenKillTransferFn,
        ) !Self {
            var self = Self{
                .gens = try std.ArrayList(std.DynamicBitSet).initCapacity(allocator, block_cnt),
                .kills = try std.ArrayList(std.DynamicBitSet).initCapacity(allocator, block_cnt),
                .data = data,
                .fptr = fptr,
                .temp_gen = try std.DynamicBitSet.initEmpty(allocator, size),
                .temp_kill = try std.DynamicBitSet.initEmpty(allocator, size),
            };
            for (0..block_cnt) |_| {
                self.gens.appendAssumeCapacity(try std.DynamicBitSet.initEmpty(allocator, size));
                self.kills.appendAssumeCapacity(try std.DynamicBitSet.initEmpty(allocator, size));
            }
            return self;
        }
        pub fn deinit(self: *Self) void {
            for (self.gens.items) |*x| x.deinit();
            for (self.kills.items) |*x| x.deinit();
            self.gens.deinit();
            self.kills.deinit();
            self.temp_gen.deinit();
            self.temp_kill.deinit();
        }

        pub fn transfer(self: *Self) Transfer(Command) {
            return Transfer(Command){
                .data = @ptrCast(self),
                .fptr = Self.typedErasedTransfer,
            };
        }

        fn typedErasedTransfer(
            data: *anyopaque,
            block_idx: usize,
            block: *const cfg.Cfg(Command).Block,
            res: *std.DynamicBitSet,
            arg: *const std.DynamicBitSet,
        ) void {
            const self: *Self = @alignCast(@ptrCast(data));
            const gen = &self.gens.items[block_idx];
            const kill = &self.kills.items[block_idx];
            initSetToEmpty(&self.temp_gen, res.unmanaged.bit_length) catch unreachable;
            initSetToEmpty(&self.temp_kill, res.unmanaged.bit_length) catch unreachable;

            self.fptr(self.data, block_idx, block, arg, &self.temp_gen, &self.temp_kill);
            copySet(gen, &self.temp_gen) catch unreachable;
            copySet(kill, &self.temp_kill) catch unreachable;
            copySet(res, arg) catch unreachable;
            self.temp_kill.toggleAll();
            res.setIntersection(self.temp_kill);
            res.setUnion(self.temp_gen);
        }
    };
}
