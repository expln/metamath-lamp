open Expln_test
open Common
open Compiler_output_parser

describe_skip("parseCompilerOutput", _ => {
    it("parseCompilerOutput", _ => {
        parseCompilerOutput(
            ~compilerOutputFilePath=""
        )
    })
})