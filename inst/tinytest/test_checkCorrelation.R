source("helpers.R")

exps <- c("RNASeq2GeneNorm", "RPPAArray")

## input validation ----------------------------------------------------------
expect_error(checkFeatureCorrelation(list(), exps), "MultiAssayExperiment")
expect_error(checkFeatureCorrelation(miniACC, "RPPAArray"), "two experiments")
expect_error(checkFeatureCorrelation(miniACC, c("RPPAArray", "RPPAArray")),
    "two experiments")
expect_error(checkFeatureCorrelation(miniACC, c("RPPAArray", "nope")),
    "two experiments")
expect_error(checkFeatureCorrelation(miniACC, exps, nperm = -1), "'nperm'")
expect_error(checkFeatureCorrelation(miniACC, exps, minObs = 2), "'minObs'")
expect_error(checkFeatureCorrelation(miniACC, exps, minObs = 47),
    "Fewer than 'minObs' patients")
expect_error(checkSampleIdentity(miniACC, exps, minZ = "a"), "'minZ'")

## no shared features
other <- makePlatform2()
rownames(other) <- paste0("feature", seq_len(nrow(other)))
noShared <- suppressWarnings(
    c(miniACC, other = other, mapFrom = "RNASeq2GeneNorm")
)
expect_error(checkFeatureCorrelation(noShared, c("RPPAArray", "other")),
    "share no patients or no features")

## patients with more than one sample in an experiment
dup <- makePlatform2()[, 1:3]
colnames(dup) <- c("s1", "s2", "s3")
smap <- S4Vectors::DataFrame(assay = "dup",
    primary = rownames(colData(miniACC))[c(1, 1, 2)],
    colname = c("s1", "s2", "s3"))
dupMae <- suppressWarnings(c(miniACC, dup = dup, sampleMap = smap))
expect_error(checkFeatureCorrelation(dupMae, c("RNASeq2GeneNorm", "dup")),
    "more than one sample")
expect_error(checkSampleIdentity(dupMae, c("RNASeq2GeneNorm", "dup")),
    "mergeReplicates")

## checkFeatureCorrelation ---------------------------------------------------

set.seed(1)
res <- checkFeatureCorrelation(miniACC, exps)
expect_inherits(res, "DataFrame")
expect_identical(names(res),
    c("feature", "n", "correlation", "pvalue", "padj"))
expect_identical(nrow(res), 33L)
expect_identical(metadata(res)$nPatients, 46L)
expect_identical(metadata(res)$experiments, exps)
expect_identical(metadata(res)$method, "spearman")
expect_identical(names(metadata(res)$null), c("q2.5", "median", "q97.5"))
expect_true(all(abs(res$correlation) <= 1, na.rm = TRUE))
expect_true(median(res$correlation, na.rm = TRUE) > metadata(res)$null[["q97.5"]])
expect_true(abs(metadata(res)$null[["median"]]) < 0.05)
expect_true(all(res$pvalue > 0 & res$pvalue <= 1))
expect_true(all(res$padj >= res$pvalue))

## matches cor() for each feature, using the patients observed in both
pairs <- MultiOmicQC:::.pairExperiments(miniACC, exps)
A <- pairs[[1]]$mat
B <- pairs[[2]]$mat
B[1, 1:5] <- NA
rc <- MultiOmicQC:::.rowCor(A, B, "spearman")
expect_equal(rc$r[1], cor(A[1, ], B[1, ], method = "spearman",
    use = "complete.obs"))
expect_identical(rc$n[1], 41)
expect_equal(MultiOmicQC:::.rowCor(A, B, "pearson")$r[2],
    cor(A[2, ], B[2, ]))

## reproducible with a seed; nperm = 0 skips the null
set.seed(2)
p1 <- checkFeatureCorrelation(miniACC, exps)$pvalue
set.seed(2)
expect_identical(checkFeatureCorrelation(miniACC, exps)$pvalue, p1)
res0 <- checkFeatureCorrelation(miniACC, exps, nperm = 0)
expect_true(all(is.na(res0$pvalue)))
expect_null(metadata(res0)$null)
expect_equal(res0$correlation, res$correlation)

## pearson
resP <- checkFeatureCorrelation(miniACC, exps, method = "pearson", nperm = 0)
expect_identical(metadata(resP)$method, "pearson")
expect_false(isTRUE(all.equal(resP$correlation, res0$correlation)))

## unrelated pairing gives a warning
shuffled <- addPlatform2(perm = c(2:79, 1))
set.seed(3)
expect_warning(
    checkFeatureCorrelation(shuffled, c("RNASeq2GeneNorm", "platform2")),
    "not above"
)

## checkSampleIdentity -------------------------------------------------------

p2exps <- c("RNASeq2GeneNorm", "platform2")
mae <- addPlatform2()
expect_silent(checkSampleIdentity(mae, p2exps))
res <- checkSampleIdentity(mae, p2exps)
expect_inherits(res, "DataFrame")
expect_identical(nrow(res), 79L)
expect_true(all(res$status == "match"))
expect_true(all(res$rank == 1 & res$reverseRank == 1))
expect_true(median(res$zscore) > 4)
expect_identical(res$bestMatch, res$primary)
expect_identical(res$colname1, res$colname2)
expect_identical(dim(metadata(res)$correlations), c(79L, 79L))
expect_identical(metadata(res)$nFeatures, 198L)

## two swapped samples are reported as a swap
mae <- addPlatform2(perm = c(2, 1, 3:79))
expect_warning(checkSampleIdentity(mae, p2exps), "swap \\(1\\)")
res <- suppressWarnings(checkSampleIdentity(mae, p2exps))
flagged <- res[res$status != "match", ]
expect_identical(flagged$status, c("swap", "swap"))
expect_identical(flagged$bestMatch, rev(flagged$primary))
expect_true(all(flagged$zscore < 2))

## three rotated samples are mismatches, not swaps
mae <- addPlatform2(perm = c(2, 3, 1, 4:79))
expect_warning(checkSampleIdentity(mae, p2exps), "mismatch \\(3\\)")
res <- suppressWarnings(checkSampleIdentity(mae, p2exps))
expect_identical(res$status[1:3], rep("mismatch", 3))
expect_true(all(res$status[-(1:3)] == "match"))

## too few informative features: warning instead of identity calls
expect_warning(checkSampleIdentity(miniACC, exps), "barely stand out")
