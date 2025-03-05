const std = @import("std");

pub fn xml_tokenizer(comptime UnderlyingReader: type) type {
    return struct {
        buffered_reader: std.io.BufferedReader(8192, UnderlyingReader), // 8KB buffered reader
        allocator: std.mem.Allocator,
        buffer: std.ArrayList(u8),
        line: usize,
        column: usize,
        pos: usize,
        last_detailed_error: ?DetailedError = null,

        pub const TokenizerError = error{
            UnexpectedEOF,
            InvalidToken,
            InvalidUTF8,
            InvalidCharacter,
            MalformedComment,
            MalformedProcessingInstruction,
            ContentTooLong,
            UnterminatedCDATASection,
            InvalidEntity,
            UnterminatedEntity,
        };

        pub const DetailedError = struct {
            kind: TokenizerError,
            line: usize,
            column: usize,
            message: []const u8,
            context: ?[]const u8,

            pub fn format(self: DetailedError) void {
                std.debug.print("XML error at line {d}, column {d}: {s}\n", .{ self.line, self.column, self.message });
                if (self.context) |ctx| {
                    std.debug.print("Context: \"{s}\"\n", .{ctx});
                }
            }
        };

        pub fn getErrorContext(self: *Self, window_size: usize) ?[]const u8 {
            if (self.buffer.items.len == 0) return null;

            const start = @max(0, self.pos - window_size);
            const end = @min(self.buffer.items.len, self.pos + window_size);

            return self.buffer.items[start..end];
        }

        pub fn emitError(self: *Self, kind: TokenizerError, message: []const u8) void {
            self.last_detailed_error = DetailedError{
                .kind = kind,
                .line = self.line,
                .column = self.column,
                .message = message,
                .context = self.getErrorContext(20),
            };
        }

        pub const Attribute = struct {
            name: []u8,
            value: []u8,
        };

        pub const Token = union(enum) {
            start_tag: struct { name: []u8, attributes: []Attribute },
            end_tag: []u8,
            self_closing_tag: struct { name: []u8, attributes: []Attribute },
            text: []u8,
            comment: []u8,
            pi: struct { target: []u8, data: []u8 },
            cdata: []u8,
            doctype: []u8,
            xml_declaration: struct { version: []u8, encoding: ?[]u8, standalone: ?[]u8 },
        };

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

        // get the next token from the XML stream or return null at EOF
        pub fn nextToken(self: *Self) !?Token {
            try self.skipWhitespace();
            if (self.pos >= self.buffer.items.len) {
                const has_more_data = try self.readNextChunk();
                if (!has_more_data)
                    return null; // end of input my guy just come back lol
            }

            const c = self.buffer.items[self.pos];
            if (c == '<') { // is start of a tag or special construct
                self.pos += 1;
                self.column += 1;
                if (self.pos >= self.buffer.items.len) {
                    const has_more_data = try self.readNextChunk();
                    if (!has_more_data) {
                        self.emitError(TokenizerError.UnexpectedEOF, "Unexpected end of file while parsing tag");
                        return TokenizerError.UnexpectedEOF; // legit EOF mid tag
                    }
                }
                if (self.pos >= self.buffer.items.len) {
                    self.emitError(TokenizerError.UnexpectedEOF, "Unexpected end of file after '<'");
                    return TokenizerError.UnexpectedEOF; // shouldnt happen after read
                }
                switch (self.buffer.items[self.pos]) {
                    '/' => { // end tag
                        self.pos += 1;
                        self.column += 1;
                        return try self.parseEndTag();
                    },
                    '!' => { // comment or other special token
                        self.pos += 1;
                        self.column += 1;
                        if (self.pos + 7 < self.buffer.items.len and std.mem.eql(u8, self.buffer.items[self.pos .. self.pos + 7], "[CDATA[")) {
                            self.pos += 7;
                            self.column += 7;
                            return try self.parseCdata();
                        } else if (self.pos + 7 < self.buffer.items.len and std.mem.eql(u8, self.buffer.items[self.pos .. self.pos + 7], "DOCTYPE")) {
                            self.pos += 7;
                            self.column += 7;
                            return try self.parseDoctype();
                        } else if (self.pos + 1 < self.buffer.items.len and self.buffer.items[self.pos] == '-' and self.buffer.items[self.pos + 1] == '-') {
                            self.pos += 2; // skip <!--
                            self.column += 2;
                            return try self.parseComment();
                        }
                        self.emitError(TokenizerError.InvalidToken, "Invalid character after '<'");
                        return TokenizerError.InvalidToken;
                    },
                    '?' => { // processing instruction
                        self.pos += 1;
                        self.column += 1;
                        return try self.parsePi();
                    },
                    else => return try self.parseStartTag(), // start or self closing tag
                }
            } else {
                return try self.parseText(); // text content
            }
        }

        pub fn skipWhitespace(self: *Self) !void {
            var pending_cr = false;
            while (true) {
                if (self.pos >= self.buffer.items.len) {
                    const has_more_data = try self.readNextChunk();
                    if (!has_more_data)
                        break;
                    // if the prev chunk ended with a \r and the new chunk starts with \n skip it
                    if (pending_cr and self.buffer.items[self.pos] == '\n') {
                        self.pos += 1;
                        pending_cr = false;
                    }
                    continue;
                }
                const c = self.buffer.items[self.pos];
                if (!isXmlWhitespace(c)) { // if this char is not whitespace finalize any pending cr adjustments and exit
                    if (pending_cr) {
                        self.column = 1;
                    }
                    return;
                }
                self.pos += 1;
                switch (c) {
                    ' ', '\t' => {
                        self.column += 1;
                        pending_cr = false;
                    },
                    '\n' => {
                        // if a pending CR then this \n is part of a crlf sequence
                        if (!pending_cr) {
                            self.line += 1;
                            self.column = 1;
                        } else {
                            pending_cr = false;
                        }
                    },
                    '\r' => {
                        self.line += 1;
                        self.column = 1;
                        pending_cr = true;
                        // check if the next char in the same chunk is \n
                        if (self.pos < self.buffer.items.len and self.buffer.items[self.pos] == '\n') {
                            self.pos += 1;
                            pending_cr = false;
                        }
                    },
                    else => unreachable,
                }
            }
        }

        pub fn isXmlWhitespace(char: u8) bool {
            return switch (char) {
                ' ', '\t', '\n', '\r' => true,
                else => false,
            };
        }
    };
}
