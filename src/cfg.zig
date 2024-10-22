const std = @import("std");

pub fn Cfg(comptime Command: type) type {
    return struct {
        blocks: std.ArrayList(Block),
        preds: std.ArrayList(std.ArrayList(usize)),
        succs: std.ArrayList(std.ArrayList(usize)),
        entry: usize = 0,
        exit: usize = 1,
        allocator: std.mem.Allocator,

        pub const CommandType: type = Command;

        pub const Block = struct {
            commands: std.ArrayList(Command),
            fn init(allocator: std.mem.Allocator) Block {
                return Block{ .commands = std.ArrayList(Command).init(allocator) };
            }
            fn deinit(self: *Block) void {
                self.commands.deinit();
            }
        };
        const Self = @This();

        pub fn init(allocator: std.mem.Allocator) !Self {
            var self = Self{
                .allocator = allocator,
                .blocks = std.ArrayList(Block).init(allocator),
                .preds = std.ArrayList(std.ArrayList(usize)).init(allocator),
                .succs = std.ArrayList(std.ArrayList(usize)).init(allocator),
            };
            self.entry = try self.addBlock();
            self.exit = try self.addBlock();

            return self;
        }

        pub fn deinit(self: *Self) void {
            for (self.blocks.items) |*x| x.deinit();
            for (self.preds.items) |*x| x.deinit();
            for (self.succs.items) |*x| x.deinit();
            self.blocks.deinit();
            self.preds.deinit();
            self.succs.deinit();
        }

        pub fn addBlock(self: *Self) !usize {
            const idx = self.blocks.items.len;
            try self.blocks.append(Block.init(self.allocator));
            try self.preds.append(std.ArrayList(usize).init(self.allocator));
            try self.succs.append(std.ArrayList(usize).init(self.allocator));
            return idx;
        }
        pub inline fn addCommand(self: *Self, cmd: Command, block: usize) !void {
            try self.blocks.items[block].commands.append(cmd);
        }

        pub inline fn connectBlocks(self: *Self, from: usize, to: usize) !void {
            try self.succs.items[from].append(to);
            try self.preds.items[to].append(from);
        }
        pub inline fn setExit(self: *Self, block: usize) !void {
            try self.connectBlocks(block, self.exit);
        }
        pub inline fn setEntry(self: *Self, block: usize) !void {
            try self.connectBlocks(self.entry, block);
        }

        pub fn dump(
            self: *const Self,
            writer: std.io.AnyWriter,
            print_block_meta: PrintBlockMetadata(Command),
        ) !void {
            try writer.print(
                \\digraph {{
                \\node [shape=record];
                \\{} [label="ENTRY"];
                \\{} [label="EXIT"];
                \\
            , .{ self.entry, self.exit });

            for (self.blocks.items, 0..) |*block, block_idx| {
                if (block_idx == self.exit or block_idx == self.entry) continue;

                try writer.print("{} [label=<{{Block {}|", .{ block_idx, block_idx });
                for (block.commands.items) |cmd| {
                    try writer.print("({}) {};", .{ cmd.idx, cmd });
                }
                try writer.print("|", .{});
                try print_block_meta.print(writer, block, block_idx);
                try writer.print("}}>];\n", .{});
            }
            for (self.blocks.items, 0..) |_, from| {
                for (self.succs.items[from].items) |to| {
                    try writer.print("{} -> {};\n", .{ from, to });
                }
            }

            try writer.print("}}\n", .{});
        }
    };
}

pub fn PrintBlockMetadata(comptime Command: type) type {
    return struct {
        data: *anyopaque,
        fptr: *const PrintBlockMetadataFn,

        const PrintBlockMetadataFn = fn (
            data: *anyopaque,
            w: std.io.AnyWriter,
            block: *const Cfg(Command).Block,
            block_idx: usize,
        ) anyerror!void;

        const Self = @This();
        fn print(
            self: *const Self,
            w: std.io.AnyWriter,
            block: *const Cfg(Command).Block,
            block_idx: usize,
        ) !void {
            try self.fptr(self.data, w, block, block_idx);
        }
    };
}

pub fn PrintNoMetadata(comptime Command: type) PrintBlockMetadata(Command) {
    const ResType = PrintBlockMetadata(Command);
    const T = struct {
        var x: void = {};
        fn print(
            data: *anyopaque,
            w: std.io.AnyWriter,
            block: *const Cfg(Command).Block,
            block_idx: usize,
        ) anyerror!void {
            _ = data;
            _ = w;
            _ = block;
            _ = block_idx;
        }

        fn printer() ResType {
            return ResType{ .data = @ptrCast(&@This().x), .fptr = @This().print };
        }
    };

    return T.printer();
}
