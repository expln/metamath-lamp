open Expln_test
open MM_editor_snapshot
open MM_wrk_editor

describe("findDiff", _ => {
    mm_editor_snapshot__test_findDiff()
})

describe("applyDiff", _ => {
    mm_editor_snapshot__test_applyDiff()
})

describe("mergeDiff", _ => {
    mm_editor_snapshot__test_mergeDiff()
})