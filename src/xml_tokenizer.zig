const std = @import("std");

pub fn xml_tokenizer(comptime UnderlyingReader: type) type {
    return struct {
        buffered_reader: std.io.BufferedReader(8192, UnderlyingReader), // 8KB buffered reader
        allocator: std.mem.Allocator,
        buffer: std.ArrayList(u8),
        line: usize,
        column: usize,
        pos: usize,

        const Self = @This();

        pub const default_chunk_size = 8192;

        pub fn init(allocator: std.mem.Allocator, underlying_reader: UnderlyingReader) !Self {
            return Self{
                .buffered_reader = std.io.bufferedReaderSize(default_chunk_size, underlying_reader),
                .allocator = allocator,
                .buffer = try std.ArrayList(u8).initCapacity(allocator, default_chunk_size),
                .line = 1,
                .column = 1,
                .pos = 0,
            };
        }

        pub fn deinit(self: *Self) void {
            self.buffer.deinit();
        }

        pub fn reader(self: *Self) std.io.BufferedReader(default_chunk_size, UnderlyingReader).Reader {
            return self.buffered_reader.reader();
        }

        pub fn readNextChunk(self: *Self) !bool {
            if (self.pos >= self.buffer.items.len) {
                try self.buffer.resize(0);
            } else if (self.pos > 0) {
                // only if we have unprocessed data shift to the beginning
                const remaining = self.buffer.items[self.pos..];
                std.mem.copyForwards(u8, self.buffer.items[0..remaining.len], remaining);
                try self.buffer.resize(remaining.len);
                self.pos = 0;
            }
            // make sure we have capacity for at least default_chunk_size more bytes
            try self.buffer.ensureUnusedCapacity(default_chunk_size);
            const end_pos = self.buffer.items.len;
            const available_space = self.buffer.capacity - end_pos;
            const read_slice = self.buffer.allocatedSlice()[end_pos..][0..@min(available_space, default_chunk_size)];
            const bytes_read = try self.buffered_reader.reader().read(read_slice);
            if (bytes_read == 0) return false; // end of input reache
            try self.buffer.resize(end_pos + bytes_read);
            return true;
        }

        pub fn getCurrentBufferContents(self: *Self) []const u8 {
            return self.buffer.items;
        }
    };
}
