open Expln_test
open Common

describe("strToSafeBase64", _ => {
    it("does not put plus and slash chars into the output", _ => {
        assertEq(strToBase64(" -> ("), "IC0+ICg=")
        assertEq(strToSafeBase64(" -> ("), "IC0-ICg=")

        assertEq(strToBase64(" -? ("), "IC0/ICg=")
        assertEq(strToSafeBase64(" -? ("), "IC0_ICg=")
    })

    it("correctly decodes base64 string containing minus and underscore signs", _ => {
        assertEq(safeBase64ToStr("IC0-ICg="), " -> (")
        assertEq(safeBase64ToStr("IC0_ICg="), " -? (")
    })
})

describe("getSpaceSeparatedValuesAsArray", _ => {
    it("splits a string into multiple strings as expected", _ => {
        assertEq( getSpaceSeparatedValuesAsArray(""), [] )
        assertEq( getSpaceSeparatedValuesAsArray("a"), ["a"] )
        assertEq( getSpaceSeparatedValuesAsArray("a b"), ["a", "b"] )
        assertEq( getSpaceSeparatedValuesAsArray("a     b"), ["a", "b"] )
        assertEq( getSpaceSeparatedValuesAsArray("a\tb"), ["a", "b"] )
        assertEq( getSpaceSeparatedValuesAsArray("a\t\tb"), ["a", "b"] )
        assertEq( getSpaceSeparatedValuesAsArray("a\nb"), ["a", "b"] )
        assertEq( getSpaceSeparatedValuesAsArray("a\n\nb"), ["a", "b"] )
        assertEq( getSpaceSeparatedValuesAsArray("a\rb"), ["a", "b"] )
        assertEq( getSpaceSeparatedValuesAsArray("a\r\rb"), ["a", "b"] )
        assertEq( getSpaceSeparatedValuesAsArray("a\fb"), ["a", "b"] )
        assertEq( getSpaceSeparatedValuesAsArray("a\f\fb"), ["a", "b"] )
        assertEq( getSpaceSeparatedValuesAsArray("a\f\tb"), ["a", "b"] )
        assertEq( getSpaceSeparatedValuesAsArray("a\t\fb"), ["a", "b"] )
        assertEq( getSpaceSeparatedValuesAsArray("a \f\t\n\rb"), ["a", "b"] )
    })
})