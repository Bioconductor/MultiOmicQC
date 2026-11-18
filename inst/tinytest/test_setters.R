source("helpers.R")

## seqlevelsStyle<- updates every ranged experiment
mae <- addRanged(
    cnA = makeRanged(c("chr1", "chr2")),
    cnB = makeRanged(c("1", "2"))
)
expect_warning(checkSeqlevelsStyle(mae))
## GenomeInfoDb warns that chr1/chr2 map to several NCBI renaming maps
suppressWarnings(seqlevelsStyle(mae) <- "NCBI")
expect_inherits(mae, "MultiAssayExperiment")
expect_identical(seqlevels(rowRanges(mae[["cnA"]])), c("1", "2"))
expect_identical(seqlevels(rowRanges(mae[["cnB"]])), c("1", "2"))
expect_silent(checkSeqlevelsStyle(mae))
expect_true(checkSeqlevelsStyle(mae))
expect_true(checkSeqlevels(mae))

## seqlevelsStyle<- leaves non-ranged experiments and colData untouched
mae <- addRanged(cnA = makeRanged(c("chr1", "chr2")))
before <- mae
suppressWarnings(seqlevelsStyle(mae) <- "NCBI")
expect_identical(names(mae), names(before))
for (nm in names(miniACC))
    expect_identical(mae[[nm]], miniACC[[nm]])
expect_identical(colData(mae), colData(before))
expect_identical(sampleMap(mae), sampleMap(before))

## genome<- sets the genome on every ranged experiment
mae <- addRanged(
    cnA = makeRanged(c("chr1", "chr2")),
    cnB = makeRanged(c("chr1", "chr3"))
)
genome(mae) <- "hg19"
for (nm in c("cnA", "cnB"))
    expect_true(all(Seqinfo::genome(rowRanges(mae[[nm]])) == "hg19"))
for (nm in names(miniACC))
    expect_identical(mae[[nm]], miniACC[[nm]])

## setters are no-ops when there are no ranged experiments
mae <- miniACC
seqlevelsStyle(mae) <- "NCBI"
genome(mae) <- "hg19"
expect_identical(experiments(mae), experiments(miniACC))
