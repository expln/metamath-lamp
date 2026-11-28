// Minimal bindings to the built-in Node.js test runner and assertions.

@module("node:test") external describe: (string, unit => unit) => unit = "describe"
@module("node:test") external it: (string, unit => unit) => unit = "it"
@module("node:test") external itAsync: (string, unit => promise<unit>) => unit = "it"

// Throws if the values are not deeply equal, showing a diff of them.
@module("node:assert/strict") external assertEq: ('a, 'a) => unit = "deepStrictEqual"

// Throws if the function doesn't throw an error with this message.
@module("node:assert/strict") external assertThrows: (unit => 'a, {"message": string}) => unit = "throws"
