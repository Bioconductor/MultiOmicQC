source("helpers.R")

## non-MultiAssayExperiment input is rejected
expect_error(checkSeqlevels(list()), "MultiAssayExperiment")
expect_error(checkSeqlengths(1:3), "MultiAssayExperiment")
expect_error(checkSeqlevelsStyle("a"), "MultiAssayExperiment")

## miniACC (no ranged experiments) is trivially consistent
expect_false(any(hasRowRanges(miniACC)))
expect_true(checkSeqlevels(miniACC))
expect_true(checkSeqlengths(miniACC))
expect_true(checkSeqlevelsStyle(miniACC))

## a single ranged experiment is consistent
mae <- addRanged(cnA = makeRanged(c("chr1", "chr2")))
expect_identical(sum(hasRowRanges(mae)), 1L)
expect_true(checkSeqlevels(mae))
expect_true(checkSeqlengths(mae))
expect_true(checkSeqlevelsStyle(mae))

## identical ranged experiments pass without warnings
lens <- c(chr1 = 248956422L, chr2 = 242193529L)
mae <- addRanged(
    cnA = makeRanged(c("chr1", "chr2"), lens),
    cnB = makeRanged(c("chr1", "chr2"), lens),
    cnC = makeRanged(c("chr1", "chr2"), lens)
)
expect_silent(checkSeqlevels(mae))
expect_silent(checkSeqlengths(mae))
expect_silent(checkSeqlevelsStyle(mae))
expect_true(checkSeqlevels(mae))
expect_true(checkSeqlengths(mae))
expect_true(checkSeqlevelsStyle(mae))

## checkSeqlevels flags differing seqlevels
mae <- addRanged(
    cnA = makeRanged(c("chr1", "chr2")),
    cnB = makeRanged(c("chr1", "chr3"))
)
expect_warning(checkSeqlevels(mae), "Inconsistent 'seqlevels'")
expect_false(suppressWarnings(checkSeqlevels(mae)))

## inconsistency is detected even when it is not the first pair
mae <- addRanged(
    cnA = makeRanged(c("chr1", "chr2")),
    cnB = makeRanged(c("chr1", "chr2")),
    cnC = makeRanged(c("chr1", "chr3"))
)
expect_warning(checkSeqlevels(mae), "cnC: chr1, chr3")
expect_false(suppressWarnings(checkSeqlevels(mae)))

## checkSeqlengths flags differing seqlengths
mae <- addRanged(
    cnA = makeRanged(c("chr1", "chr2"), c(chr1 = 100L, chr2 = 200L)),
    cnB = makeRanged(c("chr1", "chr2"), c(chr1 = 100L, chr2 = 300L))
)
expect_warning(checkSeqlengths(mae), "Inconsistent 'seqlengths'")
expect_false(suppressWarnings(checkSeqlengths(mae)))

## checkSeqlevelsStyle flags UCSC vs NCBI
mae <- addRanged(
    cnA = makeRanged(c("chr1", "chr2")),
    cnB = makeRanged(c("1", "2"))
)
expect_warning(checkSeqlevelsStyle(mae), "cnA: UCSC")
expect_false(suppressWarnings(checkSeqlevelsStyle(mae)))

## long seqlevels are abbreviated in the warning
mae <- addRanged(
    cnA = makeRanged(paste0("chr", 1:10)),
    cnB = makeRanged(paste0("chr", 11:20))
)
expect_warning(
    checkSeqlevels(mae), "cnA: chr1, chr2, ..., chr9, chr10", fixed = TRUE
)
