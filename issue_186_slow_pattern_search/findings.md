# Slow search by pattern (version 2): findings

Investigation of the performance of `src/metamath/worker/MM_wrk_pattern_search_v2.res`.
The code has not been changed yet; this document records the analysis only.

## Symptom

This pattern stalls the search, and the results never appear:

```text
( A + ( B + C ) ) = ( ( A + B ) + C )
```

The stall was reported at 20% on another database.
On the repo's `set._mm` the first expensive frame is at 30.2%, and the search slows down from there.
The mechanism is the same, but the frames and the percentages depend on the database.

## Summary

The variable phase is a backtracking search with no effective pruning,
so its cost grows combinatorially with the length of a frame.
On `set._mm` this pattern needs about 2×10¹² recursive calls, which is roughly 6 days of CPU time.
The search would finish eventually, but only long after the user has given up.

## Root cause

1. Without `$+`, the pattern becomes an `Ordered` group of 19 one-symbol children (`astToSeqGrp`, line 655).
   Each child may match anywhere after the previous one.
2. The concatenated hypotheses and assertion of a large theorem have hundreds of symbols,
   mostly `(`, `)`, `+`, `=` and class variables.
   Each of the 178 frames that pass the constant check has many valid alignments,
   up to 6.5×10³⁹ for `quartfull`.
3. `exprIncludesVarOrderedSeq` (line 458) tries every position for each child and recurses.
   It stops only at a full match with consistent bindings of `A`, `B` and `C`.
4. Most alignments fail late, on a variable conflict, so the same suffix is searched again and again.

## Why the existing optimizations do not help

- **The constant pre-check passes too easily.**
  `exprIncludesConstSeq` matches greedily and treats variables as their type only.
  Any large frame with enough of these symbols passes, and those are the expensive frames.
- **`minConstMismatchIdx` stays empty.**
  It is written only when a child fails in the constant phase (line 237).
  If that phase succeeds on a flat pattern, nothing fails, so the variable phase has nothing to prune with.
  Confirmed on `sqsumi`: 0 of 20 sequences held a value.
  `Unordered` and `OneOf` groups can record failures, but flat patterns cannot.
- **Failures in the variable phase are never remembered.**
  The same `(child, position, bindings)` state is explored again each time a different path reaches it.

## Measurements

The numbers come from `set._mm`, which gave the same results as `set-no-proofs._mm`.
Times are estimates at about 3.8M calls/s, a rough rate measured on this machine.

| Frame | Progress position | Matches? | Calls | Est. time | Distinct states |
|---|---|---|---|---|---|
| `seqf1olem2` | 30.2% | yes | 1.5×10⁸ | ~40 s | 9,806 |
| `4sqlem15` | 36.9% | yes | 4.8×10⁹ | ~21 min | 32,843 |
| `pntlemj` | 59.2% | yes | 1.4×10¹¹ | ~10 h | 130,647 |
| `fourierdlem100` | 95.0% | no | 1.35×10¹² | ~4 days | 135,646 |
| **All frames** | | | **2×10¹²** | **~6 days** | **1.9M** |

- Only 178 of 43,352 frames pass the constant check, so the cost is concentrated in a few frames.
- Of those 178 frames, 115 match and 63 do not.
- 68 frames exceeded a budget of 5M calls, and 38 of them do match.
  Matching frames are slow too, because the search reaches a valid alignment very late.

### Method

- A copy of the compiled `MM_wrk_pattern_search_v2.res.js` was loaded in memory
  with a call counter and a per-frame budget added to `exprIncludesVarOrderedSeq`.
- A separate script replayed the same depth-first search with failed states memoized,
  which gives exact call counts for frames too expensive to run.
- That replay matched the real code on all 106 frames cheap enough to compare, in both result and call count.
  This covers the flat pattern above only.

## Possible solutions

### 1. Remember failed states (main fix)

Cache failures by `(child index, start position, current variable bindings)`.

- Total work drops from 2×10¹² calls to 1.9M state evaluations, about a million times fewer.
- An unoptimized prototype with string keys took about 15 s for all frames, including its other checks.
- Found matches stay intact, because only failed states are cached.

Implementation notes:

- The prototype handles flat patterns only.
- `Unordered` needs `passedSeqIdxs` in the key.
- Nested patterns need a position in the pattern tree rather than a flat index.
- The memo must be reset for each frame, as `minConstMismatchIdx` is in `frameMatchesPattern`.

### 2. Include only the relevant bindings in the key

The key needs only the variables that the remaining pattern still uses, which improves the hit rate.
Variables not used later still matter through the rule that different pattern variables
must capture different frame variables (`capVars`), so that rule has to be accounted for.
This was not prototyped.

### 3. Latest-start pruning (optional)

A right-to-left scan can compute the last position where each child can still fit.
It is cheap, but it cut non-matching frames only about 80×, from 1.5×10¹² to 1.8×10¹⁰ calls.
It can complement the cache, but it cannot replace it.

## Other observations

- `exprIncludesVarOrderedSeq` reports the search start as `matchBegin` (line 483), not the real match start.
  This was suspected of causing repeated work in nested groups, but it did not show up in measurements.
  The only comparison that finished, on `sqsumi`, used 749 calls flat and 750 with grouping.
  One frame is weak evidence, so this is worth rechecking if nested patterns turn out to be slow.
- Long-running frames give no progress updates, because progress is reported per frame in steps of 10%.
  A single expensive frame therefore looks like a hang.
