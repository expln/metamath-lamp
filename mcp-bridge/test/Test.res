// Minimal bindings to the built-in Node.js test runner and assertions.

@module("node:test") external describe: (string, unit => unit) => unit = "describe"
@module("node:test") external it: (string, unit => unit) => unit = "it"

// Throws if the values are not deeply equal, showing a diff of them.
@module("node:assert/strict") external assertEq: ('a, 'a) => unit = "deepStrictEqual"
