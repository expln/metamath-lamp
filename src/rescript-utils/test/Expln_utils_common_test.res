open Expln_utils_common
let {describe,it,assertEq,fail} = module(Expln_test)

describe("arrJoin", _ => {
    it("should produce results as expected", _ => {
        assertEq( []->arrJoin(0), [])
        assertEq( [1]->arrJoin(0), [1])
        assertEq( [1,2]->arrJoin(0), [1, 0, 2])
        assertEq( [1,2,3]->arrJoin(0), [1, 0, 2, 0, 3])
    })
})

describe("hash2", _ => {
    it("produces 0 for two zeroes", _ => {
        assertEq( hash2(0,0), 0)
    })
    it("produces non-zero for two non-zeroes", _ => {
        assertEq( hash2(1,2), 33)
    })
})

describe("hashStr", _ => {
    it("produces 0 for an empty string", _ => {
        assertEq( hashStr(""), 0)
    })
    it("produces non-zero for a non-empty string", _ => {
        assertEq( hashStr("a"), 97)
        assertEq( hashStr("ab"), 3105)
        assertEq( hashStr("abc"), 96354)
    })
})

describe("hashArrInt", _ => {
    it("produces 0 for an empty array", _ => {
        assertEq( hashArrInt([]), 0)
    })
    it("produces non-zero for a non-empty array", _ => {
        assertEq( hashArrInt([1]), 1)
        assertEq( hashArrInt([2,3]), 65)
        assertEq( hashArrInt([4,5,6]), 4005)
    })
})

describe("hashArrIntFromTo", _ => {
    it("produces 0 when 'from' is greater then 'to'", _ => {
        assertEq( hashArrIntFromTo([1,2,3,4,5],100,90), 0)
    })
    it("produces same result as hashArrInt", _ => {
        assertEq( hashArrIntFromTo([1,2,3,4,5,6],0,0), hashArrInt([1]))
        assertEq( hashArrIntFromTo([1,2,3,4,5,6],1,2), hashArrInt([2,3]))
        assertEq( hashArrIntFromTo([1,2,3,4,5,6],3,5), hashArrInt([4,5,6]))
    })
})

describe("comparatorBy", _ => {
    it("produces correct comparators", _ => {
        //given
        let cmp1 = comparatorBy(a => a->Array.getUnsafe(1))

        //when/then
        assertEq(-1.0, cmp1([1,30], [2,40]))
        assertEq(0.0, cmp1([1,40], [2,40]))
        assertEq(1.0, cmp1([1,50], [2,40]))
    })
})

describe("comparatorInverse", _ => {
    it("creates inverted comparator", _ => {
        //given
        let cmp1 = comparatorBy(a => a->Array.getUnsafe(1))->comparatorInverse

        //when/then
        assertEq(1.0, cmp1([1,30], [2,40]))
        assertEq(0.0, cmp1([1,40], [2,40]))
        assertEq(-1.0, cmp1([1,50], [2,40]))
    })
})

describe("comparatorAndThen", _ => {
    it("produces correct results when two comparators are combined", _ => {
        //given
        let cmp1 = (a,b) => (a->Array.getUnsafe(0) - b->Array.getUnsafe(0))->Belt_Float.fromInt
        let cmp2 = (a,b) => (a->Array.getUnsafe(1) - b->Array.getUnsafe(1))->Belt_Float.fromInt
        let cmp12 = cmp1->comparatorAndThen(cmp2)
        let cmp21 = cmp2->comparatorAndThen(cmp1)
        let arr = [
            [1,2],
            [3,1],
            [2,1],
            [3,2],
            [1,1],
            [2,2],
        ]

        //when
        let res12 = arr->Array.toSorted(cmp12)

        //then
        assertEq(
            res12,
            [
                [1,1],
                [1,2],
                [2,1],
                [2,2],
                [3,1],
                [3,2],
            ]
        )

        //when
        let res21 = arr->Array.toSorted(cmp21)

        //then
        assertEq(
            res21,
            [
                [1,1],
                [2,1],
                [3,1],
                [1,2],
                [2,2],
                [3,2],
            ]
        )
    })

    it("produces correct results when three comparators are combined", _ => {
        //given
        let cmp1 = (a,b) => (a->Array.getUnsafe(0) - b->Array.getUnsafe(0))->Belt_Float.fromInt
        let cmp2 = (a,b) => (a->Array.getUnsafe(1) - b->Array.getUnsafe(1))->Belt_Float.fromInt
        let cmp3 = (a,b) => (a->Array.getUnsafe(2) - b->Array.getUnsafe(2))->Belt_Float.fromInt
        let cmp123 = cmp1->comparatorAndThen(cmp2)->comparatorAndThen(cmp3)
        let cmp321 = cmp3->comparatorAndThen(cmp2)->comparatorAndThen(cmp1)
        let arr = [
            [2,2,1],
            [1,2,1],
            [1,2,2],
            [2,1,1],
            [1,1,2],
            [3,1,2],
            [3,2,2],
            [2,1,2],
            [2,2,2],
            [3,1,1],
            [1,1,1],
            [3,2,1],
        ]

        //when
        let res123 = arr->Array.toSorted(cmp123)

        //then
        assertEq(
            res123,
            [
                [1,1,1],
                [1,1,2],
                [1,2,1],
                [1,2,2],
                [2,1,1],
                [2,1,2],
                [2,2,1],
                [2,2,2],
                [3,1,1],
                [3,1,2],
                [3,2,1],
                [3,2,2],
            ]
        )

        //when
        let res321 = arr->Array.toSorted(cmp321)

        //then
        assertEq(
            res321,
            [
                [1,1,1],
                [2,1,1],
                [3,1,1],
                [1,2,1],
                [2,2,1],
                [3,2,1],
                [1,1,2],
                [2,1,2],
                [3,1,2],
                [1,2,2],
                [2,2,2],
                [3,2,2],
            ]
        )
    })
})
