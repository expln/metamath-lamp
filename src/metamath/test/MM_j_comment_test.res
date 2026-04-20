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
        assertEq( parseJComment("ab ;"), [{command:"ab", args:[]}])
    })
    it("command without agruments", _ => {
        assertEqMsg( parseJComment("abc ;"), [{command:"abc", args:[]}], "space before semicolon")
        assertEqMsg( parseJComment("abc;"), [{command:"abc", args:[]}], "no space before semicolon")
    })
    it("no semicolon at the end of a single command", _ => {
        assertEq( parseJComment("abc "), [] )
    })
    it("two commands without arguments", _ => {
        assertEq( parseJComment("abc; def ;"), [{command:"abc", args:[]}, {command:"def", args:[]}] )
    })
    it("no semicolon after the second command", _ => {
        assertEq( parseJComment("abc ; def "), [{command:"abc", args:[]}] )
    })
    it("command with one unquoted argument", _ => {
        assertEq( parseJComment("abc def ;"), [{command:"abc", args:[Unquoted("def")]}] )
    })
    it("command with two unquoted arguments", _ => {
        assertEq( parseJComment("abc def ghi;"), [{command:"abc", args:[Unquoted("def"), Unquoted("ghi")]}] )
    })
    it("command with one quoted argument", _ => {
        assertEq( parseJComment("abc 'd e f' ;"), [{command:"abc", args:[Quoted("d e f")]}] )
    })
    it("command with two quoted arguments", _ => {
        assertEq( parseJComment("abc 'd e f'    ' ghi ';"), [{command:"abc", args:[Quoted("d e f"), Quoted(" ghi ")]}])
    })
    it("command with multiple quoted and unquoted arguments", _ => {
        assertEq( 
            parseJComment("abc 'd e f'  XYZ  ' ghi ' 123 ;"), 
            [{command:"abc", args:[Quoted("d e f"), Unquoted("XYZ"), Quoted(" ghi "), Unquoted("123")]}]
        )
    })
    it("multiple commands on multiple lines", _ => {
        assertEq( 
            parseJComment("c1 a b; \n c2 ; \n\t\f    \n\n c3 c;"), 
            [
                {command:"c1", args:[Unquoted("a"), Unquoted("b")]},
                {command:"c2", args:[]},
                {command:"c3", args:[Unquoted("c")]},
            ]
        )
    })
    it("quoted single quote", _ => {
        assertEq( parseJComment(`cmd "'";`), [ {command:"cmd", args:[Quoted("'")]} ] )
    })
    it("quoted double quote", _ => {
        assertEq( parseJComment(`cmd '"';`), [ {command:"cmd", args:[Quoted("\"")]} ] )
    })
    it("single quoted empty arg", _ => {
        assertEq( parseJComment(`cmd '';`), [ {command:"cmd", args:[Quoted("")]} ] )
    })
    it("double quoted empty arg", _ => {
        assertEq( parseJComment(`cmd "";`), [ {command:"cmd", args:[Quoted("")]} ] )
    })
    it("commands from set.mm", _ => {
        assertEqMsg( 
            parseJComment(` usage 'cbvexvw' avoids 'ax-8' 'ax-9' 'ax-10' 'ax-11' 'ax-12'
       'ax-13'; `), 
            [
                {command:"usage", args:[
                    Quoted("cbvexvw"), Unquoted("avoids"), Quoted("ax-8"), Quoted("ax-9"), Quoted("ax-10"), 
                    Quoted("ax-11"), Quoted("ax-12"), Quoted("ax-13")
                ]},
            ],
            "case 1"
        )
        assertEqMsg( 
            parseJComment(`
    syntax 'wff';
    syntax '|-' as 'wff';
    unambiguous 'klr 5';
  `), 
            [
                {command:"syntax", args:[ Quoted("wff")]},
                {command:"syntax", args:[ Quoted("|-"), Unquoted("as"), Quoted("wff")]},
                {command:"unambiguous", args:[ Quoted("klr 5")]},
            ],
            "case 2"
        )
        assertEqMsg( 
            parseJComment(`
    varcolorcode "wff" as "0000FF";
    altvarcolorcode "wff" as "337DFF";
  `), 
            [
                {command:"varcolorcode", args:[ Quoted("wff"), Unquoted("as"), Quoted("0000FF")]},
                {command:"altvarcolorcode", args:[ Quoted("wff"), Unquoted("as"), Quoted("337DFF")]},
            ],
            "case 3"
        )
    })
})