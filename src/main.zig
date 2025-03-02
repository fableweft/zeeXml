const std = @import("std");
const xml_tokenizer = @import("xml_tokenizer.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var xml_files_dir = try std.fs.cwd().openDir("xml-files", .{});
    defer xml_files_dir.close();

    const file = try xml_files_dir.openFile("simple.xml", .{});
    defer file.close();

    const reader = file.reader();
    var tokenizer = try xml_tokenizer.xml_tokenizer(std.fs.File.Reader).init(allocator, reader);
    defer tokenizer.deinit();

    // var chunk_count: usize = 0;
    // while (true) {
    //     const has_data = try tokenizer.readNextChunk();
    //     if (!has_data) break;

    //     chunk_count += 1;
    //     const current_buffer = tokenizer.getCurrentBufferContents();

    //     std.debug.print("\n--- Chunk {d} ---\n", .{chunk_count});
    //     std.debug.print("Buffer size: {d} bytes\n", .{current_buffer.len});
    //     std.debug.print("Content: \n{s}\n", .{current_buffer});
    //     std.debug.print("Current position: {d}\n", .{tokenizer.pos});

    //     tokenizer.pos = current_buffer.len;

    const contents = try tokenizer.reader().readAllAlloc(allocator, std.math.maxInt(usize));
    defer allocator.free(contents);
    std.debug.print("File content:\n{s}\n", .{contents});
}
