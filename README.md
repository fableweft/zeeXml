# zeeXml

A lightweight, non-validating, stream-oriented XML parser written in Zig. I needed a simple stream-oriented, non-validating XML parser library written in Zig so I made one. UTF-8 support only for now. zeeXml uses an iterator pattern for explicit control flow, letting you step through XML events (start tags, end tags, text) at your own pace. Perfect for processing large files or streams with minimal memory use and maximum clarity.

## Features

- **Iterator-Based**: Pull XML events one-by-one with a simple next() interface
- **XML 1.0 Compliant**: Follows XML 1.0 Specification (Fifth Edition)
- **Non-Validating**: Focuses on parsing speed by skipping DTD validation
- **Stream-Friendly**: Designed for incremental processing of large XML data sources
- **Memory Efficient**: Process XML documents of any size with minimal memory overhead
- **Zig-Native**: Built with Zig's safety features and performance characteristics
- **Zero External Dependencies**
