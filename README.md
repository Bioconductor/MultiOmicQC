# MultiOmicQC

## Genomic annotation matching
Given two or more datasets on genomic intervals or with genomic build information:

### Simple sanity checks
- [x] Check seqlevels are compatible (and report mismatches) — `checkSeqlevels()`
- [x] Check seqlevelsStyle are compatible (and report mismatches) — `checkSeqlevelsStyle()`
- [x] Check seqlengths (or max seqlengths) — `checkSeqlengths()`
- [x] Check genome builds match (e.g., hg19 vs hg38) — `checkGenome()`

## Sample level checks
- [x] Sex check against marker gene expression — `checkSex()`
- [ ] Population prediction (genetic data)

## Dataset-level checks
- [ ] Sample correlations
- [ ] General outlier discovery in matrix-like data
