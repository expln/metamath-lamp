open Expln_test
open MM_j_comment

describe("parseJComment", _ => {
    it("empty string", _ => {
        assertEq( parseJComment(""), [] )
    })
    it("blank string", _ => {
        assertEq( parseJComment("   "), [] )
    })
    it("semicolon", _ => {
        assertEq( parseJComment(";"), [] )
    })
    it("two semicolons", _ => {
        assertEq( parseJComment("; ;"), [] )
    })
    it("single character command without arguments", _ => {
        assertEq( parseJComment("a ;"), [{command:"a", args:[]}] )
    })
    it("two character command without arguments", _ => {
        assertEqMsg( parseJComment(";a ;"), [{command:";a", args:[]}], "semicolon in command name")
        assertEqMsg( parseJComment("ab ;"), [{command:"ab", args:[]}], "no semicolon in command name")
    })
    it("command without agruments", _ => {
        assertEq( parseJComment("abc ;"), [{command:"abc", args:[]}] )
    })
    it("no semicolon at the end of a single command", _ => {
        assertEq( parseJComment("abc "), [] )
    })
    it("two commands without arguments", _ => {
        assertEq( parseJComment("abc ; def ;"), [{command:"abc", args:[]}, {command:"def", args:[]}] )
    })
    it("no semicolon after the second command", _ => {
        assertEq( parseJComment("abc ; def "), [{command:"abc", args:[]}] )
    })
    it("command with one unquoted argument", _ => {
        assertEq( parseJComment("abc def ;"), [{command:"abc", args:[Unquoted("def")]}] )
    })
    it("command with two unquoted arguments", _ => {
        assertEq( parseJComment("abc def ghi ;"), [{command:"abc", args:[Unquoted("def"), Unquoted("ghi")]}] )
    })
    it("command with one quoted argument", _ => {
        assertEq( parseJComment("abc 'd e f' ;"), [{command:"abc", args:[Quoted("d e f")]}] )
    })
    it("command with two quoted arguments", _ => {
        assertEq( parseJComment("abc 'd e f'    ' ghi ' ;"), [{command:"abc", args:[Quoted("d e f"), Quoted(" ghi ")]}])
    })
    it("command with multiple quoted and unquoted arguments", _ => {
        assertEq( 
            parseJComment("abc 'd e f'  XYZ  ' ghi ' 123 ;"), 
            [{command:"abc", args:[Quoted("d e f"), Unquoted("XYZ"), Quoted(" ghi "), Unquoted("123")]}]
        )
    })
    it("multiple commands on multiple lines", _ => {
        assertEq( 
            parseJComment("c1 a b ; \n c2 ; \n\t\f    \n\n c3 c ;"), 
            [
                {command:"c1", args:[Unquoted("a"), Unquoted("b")]},
                {command:"c2", args:[]},
                {command:"c3", args:[Unquoted("c")]},
            ]
        )
    })
    // it("commands from set.mm", _ => {
    //     assertEq( 
    //         parseJComment("c1 a b ; \n c2 ; \n\t\f    \n\n c3 c ;"), 
    //         [
    //             {command:"c1", args:[Unquoted("a"), Unquoted("b")]},
    //             {command:"c2", args:[]},
    //             {command:"c3", args:[Unquoted("c")]},
    //         ]
    //     )
    // })
})