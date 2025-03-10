const std = @import("std");
const Tokenizer = @import("xml_tokenizer.zig").xml_tokenizer(std.fs.File.Reader);

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{ .enable_memory_limit = true }){};
    // defer {
    //     const leaks = gpa.deinit();
    //     if (leaks == .leak) {
    //         std.debug.print("Memory leaks detected!\n", .{});
    //     } else {
    //         std.debug.print("No memory leaks.\n", .{});
    //     }
    // }
    const allocator = gpa.allocator();

    var xml_files_dir = try std.fs.cwd().openDir("xml-files", .{});
    defer xml_files_dir.close();

    const file = try xml_files_dir.openFile("test_all.xml", .{});
    defer file.close();

    const reader = file.reader();

    var tokenizer = try Tokenizer.init(allocator, reader);
    defer tokenizer.deinit();

    while (true) {
        const maybe_token = tokenizer.nextToken() catch |err| {
            if (tokenizer.last_detailed_error) |detailed_err| {
                detailed_err.format();
            } else {
                std.debug.print("Error processing token: {}\n", .{err});
            }
            return err;
        };

        if (maybe_token) |token| {
            switch (token) {
                .start_tag => |st| {
                    std.debug.print("Start Tag: {s}\n", .{st.name});
                    for (st.attributes) |attr| {
                        std.debug.print("  Attribute: {s}=\"{s}\"\n", .{ attr.name, attr.value });
                        allocator.free(attr.name);
                        allocator.free(attr.value);
                    }
                    allocator.free(st.name);
                    allocator.free(st.attributes);
                },
                .end_tag => |name| {
                    std.debug.print("End Tag: {s}\n", .{name});
                    allocator.free(name);
                },
                .self_closing_tag => |sct| {
                    std.debug.print("Self-Closing Tag: {s}\n", .{sct.name});
                    for (sct.attributes) |attr| {
                        std.debug.print("  Attribute: {s}=\"{s}\"\n", .{ attr.name, attr.value });
                        allocator.free(attr.name);
                        allocator.free(attr.value);
                    }
                    allocator.free(sct.name);
                    allocator.free(sct.attributes);
                },
                .text => |text| {
                    std.debug.print("Text: {s}\n", .{text});
                    allocator.free(text);
                },
                .comment => |comment| {
                    std.debug.print("Comment: {s}\n", .{comment});
                    allocator.free(comment);
                },
                .pi => |pi| {
                    std.debug.print("Processing Instruction: target={s}, data={s}\n", .{ pi.target, pi.data });
                    allocator.free(pi.target);
                    allocator.free(pi.data);
                },
                .cdata => |cdata| {
                    std.debug.print("CDATA: {s}\n", .{cdata});
                    allocator.free(cdata);
                },
                .doctype => |doctype| {
                    std.debug.print("DOCTYPE: {s}\n", .{doctype});
                    allocator.free(doctype);
                },
                .xml_declaration => |xmldec| {
                    std.debug.print("XML Declaration: version={s}", .{xmldec.version});
                    if (xmldec.encoding) |enc| std.debug.print(", encoding={s}", .{enc});
                    if (xmldec.standalone) |sa| std.debug.print(", standalone={s}", .{sa});
                    std.debug.print("\n", .{});
                    allocator.free(xmldec.version);
                    if (xmldec.encoding) |enc| allocator.free(enc);
                    if (xmldec.standalone) |sa| allocator.free(sa);
                },
            }
        } else {
            break; // end of file
        }
    }

    std.debug.print("Finished processing XML file.\n", .{});
}
