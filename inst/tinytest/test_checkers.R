source("helpers.R")

## warning text of a call, or NA when it does not warn
warningText <- function(expr) {
    tryCatch({ expr; NA_character_ },
        warning = function(w) conditionMessage(w))
}

## non-MultiAssayExperiment input is rejected
expect_error(checkSeqlevels(list()), "MultiAssayExperiment")
expect_error(checkSeqlengths(1:3), "MultiAssayExperiment")
expect_error(checkSeqlevelsStyle("a"), "MultiAssayExperiment")
expect_error(checkGenome(NULL), "MultiAssayExperiment")
expect_error(checkSeqlevels(miniACC, exact = NA), "'exact'")

## miniACC (no ranged experiments) is trivially compatible
expect_false(any(hasRowRanges(miniACC)))
expect_true(checkSeqlevels(miniACC))
expect_true(checkSeqlevels(miniACC, exact = TRUE))
expect_true(checkSeqlengths(miniACC))
expect_true(checkSeqlevelsStyle(miniACC))
expect_true(checkGenome(miniACC))

## a single ranged experiment is compatible
mae <- addRanged(cnA = makeRanged(c("chr1", "chr2"), genome = "hg19"))
expect_identical(sum(hasRowRanges(mae)), 1L)
expect_true(checkSeqlevels(mae, exact = TRUE))
expect_true(checkSeqlengths(mae))
expect_true(checkSeqlevelsStyle(mae))
expect_true(checkGenome(mae))

## identical ranged experiments pass without warnings
lens <- c(chr1 = 248956422L, chr2 = 242193529L)
mae <- addRanged(
    cnA = makeRanged(c("chr1", "chr2"), lens, "hg38"),
    cnB = makeRanged(c("chr1", "chr2"), lens, "hg38"),
    cnC = makeRanged(c("chr1", "chr2"), lens, "hg38")
)
expect_silent(checkSeqlevels(mae))
expect_silent(checkSeqlevels(mae, exact = TRUE))
expect_silent(checkSeqlengths(mae))
expect_silent(checkSeqlevelsStyle(mae))
expect_silent(checkGenome(mae))
expect_true(checkSeqlevels(mae, exact = TRUE))
expect_true(checkGenome(mae))

## checkSeqlevels ------------------------------------------------------------

## order does not matter, in either mode
mae <- addRanged(
    cnA = makeRanged(c("chr1", "chr2")),
    cnB = makeRanged(c("chr2", "chr1"))
)
expect_silent(checkSeqlevels(mae))
expect_true(checkSeqlevels(mae))
expect_true(checkSeqlevels(mae, exact = TRUE))

## subsets are compatible by default but not with exact = TRUE
mae <- addRanged(
    cnA = makeRanged(c("chr1", "chr2", "chrX")),
    cnB = makeRanged(c("chr1", "chr2"))
)
expect_true(checkSeqlevels(mae))
expect_warning(checkSeqlevels(mae, exact = TRUE), "missing from")
expect_false(suppressWarnings(checkSeqlevels(mae, exact = TRUE)))
expect_identical(
    warningText(checkSeqlevels(mae, exact = TRUE)),
    "Inconsistent 'seqlevels' found; missing from:\n    cnB: chrX"
)

## partial overlap is compatible by default
mae <- addRanged(
    cnA = makeRanged(c("chr1", "chr2")),
    cnB = makeRanged(c("chr2", "chr3"))
)
expect_true(checkSeqlevels(mae))
expect_false(suppressWarnings(checkSeqlevels(mae, exact = TRUE)))

## no seqlevels in common fails, naming each disjoint pair
mae <- addRanged(
    cnA = makeRanged(c("chr1", "chr2")),
    cnB = makeRanged(c("chr1", "chr2")),
    cnC = makeRanged(c("1", "2"))
)
expect_false(suppressWarnings(checkSeqlevels(mae)))
expect_identical(
    warningText(checkSeqlevels(mae)),
    "Ranged experiments with no 'seqlevels' in common:\n    cnA & cnC\n    cnB & cnC"
)

## long lists of missing seqlevels are abbreviated
mae <- addRanged(
    cnA = makeRanged(paste0("chr", 1:10)),
    cnB = makeRanged(paste0("chr", 11:20))
)
expect_warning(
    checkSeqlevels(mae, exact = TRUE),
    "cnA: chr11, chr12, ..., chr19, chr20", fixed = TRUE
)

## checkSeqlengths -----------------------------------------------------------

## matched by name, so order does not matter
mae <- addRanged(
    cnA = makeRanged(c("chr1", "chr2"), c(chr1 = 100L, chr2 = 200L)),
    cnB = makeRanged(c("chr2", "chr1"), c(chr2 = 200L, chr1 = 100L))
)
expect_silent(checkSeqlengths(mae))
expect_true(checkSeqlengths(mae))

## unknown (NA) lengths are ignored
mae <- addRanged(
    cnA = makeRanged(c("chr1", "chr2"), c(chr1 = 100L, chr2 = 200L)),
    cnB = makeRanged(c("chr1", "chr2")),
    cnC = makeRanged(c("chr1", "chr2"), c(chr1 = 100L, chr2 = NA))
)
expect_silent(checkSeqlengths(mae))
expect_true(checkSeqlengths(mae))

## only seqlevels present in several experiments are compared
mae <- addRanged(
    cnA = makeRanged(c("chr1", "chr2"), c(chr1 = 100L, chr2 = 200L)),
    cnB = makeRanged(c("chr1", "chr3"), c(chr1 = 100L, chr3 = 300L))
)
expect_true(checkSeqlengths(mae))

## conflicting known lengths fail, listing each experiment's length
mae <- addRanged(
    cnA = makeRanged(c("chr1", "chr2"), c(chr1 = 100L, chr2 = 200L)),
    cnB = makeRanged(c("chr1", "chr2"), c(chr1 = 100L, chr2 = 300L)),
    cnC = makeRanged(c("chr1", "chr2"))
)
expect_false(suppressWarnings(checkSeqlengths(mae)))
expect_identical(
    warningText(checkSeqlengths(mae)),
    "Inconsistent 'seqlengths' found:\n    chr2: cnA=200, cnB=300"
)

## checkSeqlevelsStyle -------------------------------------------------------

## UCSC vs NCBI fails
mae <- addRanged(
    cnA = makeRanged(c("chr1", "chr2")),
    cnB = makeRanged(c("1", "2"))
)
expect_warning(checkSeqlevelsStyle(mae), "cnA: UCSC")
expect_false(suppressWarnings(checkSeqlevelsStyle(mae)))

## styles only need one match in common: 1,2 matches NCBI, Ensembl, MSU6,
## and AGPvF while 1,2,MT matches NCBI and Ensembl
mae <- addRanged(
    cnA = makeRanged(c("1", "2")),
    cnB = makeRanged(c("1", "2", "MT"))
)
expect_silent(checkSeqlevelsStyle(mae))
expect_true(checkSeqlevelsStyle(mae))

## experiments with an undetermined style are skipped
mae <- addRanged(
    cnA = makeRanged(c("chr1", "chr2")),
    cnB = makeRanged(c("contig1", "scaffold2"))
)
expect_silent(checkSeqlevelsStyle(mae))
expect_true(checkSeqlevelsStyle(mae))

## checkGenome ---------------------------------------------------------------

## hg19 vs hg38 fails
mae <- addRanged(
    cnA = makeRanged(c("chr1", "chr2"), genome = "hg19"),
    cnB = makeRanged(c("chr1", "chr2"), genome = "hg38")
)
expect_false(suppressWarnings(checkGenome(mae)))
expect_identical(
    warningText(checkGenome(mae)),
    "Inconsistent 'genome' found:\n    cnA: hg19\n    cnB: hg38"
)

## detected even without shared seqlevels (merge() would not catch this)
mae <- addRanged(
    cnA = makeRanged(c("chr1", "chr2"), genome = "hg19"),
    cnB = makeRanged(c("1", "2"), genome = "GRCh38")
)
expect_false(suppressWarnings(checkGenome(mae)))

## unknown (NA) genomes are ignored
mae <- addRanged(
    cnA = makeRanged(c("chr1", "chr2"), genome = "hg19"),
    cnB = makeRanged(c("chr1", "chr2")),
    cnC = makeRanged(c("chr1", "chr2"), genome = "hg19")
)
expect_silent(checkGenome(mae))
expect_true(checkGenome(mae))
