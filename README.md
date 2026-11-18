# MultiOmicQC

## Genomic annotation matching
Given two or more datasets on genomic intervals or with genomic build information:

### Simple sanity checks
- [x] Check seqlevels match (and report mismatches) — `checkSeqlevels()`
- [x] Check seqlevelsStyle match (and report mismatches) — `checkSeqlevelsStyle()`
- [x] Check seqlengths (or max seqlengths) — `checkSeqlengths()`
- [ ] Check genome builds match (e.g., hg19 vs hg38)

## Sample level checks
- [ ] Sex prediction
- [ ] Population prediction (genetic data)

## Dataset-level checks
- [ ] Sample correlations
- [ ] General outlier discovery in matrix-like data
