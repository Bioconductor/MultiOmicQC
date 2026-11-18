source("helpers.R")

genes <- c("CDKN1B", "ERBB2", "G6PD")
res <- quietly(
    clusterOn(miniACC, colDataCols = "gender", rownames = genes,
        experiments = "RNASeq2GeneNorm", seed = 42L)
)

## one row per patient with clusters and centers
expect_inherits(res, "DataFrame")
expect_identical(nrow(res), ncol(miniACC[["RNASeq2GeneNorm"]]))
expect_identical(anyDuplicated(res[["primary"]]), 0L)
expect_true(all(c("primary", "colname", "gender", genes) %in% names(res)))
expect_true(all(paste0("centers.", genes) %in% names(res)))
expect_true(all(res[["kcluster"]] %in% c(1, 2)))
expect_identical(length(unique(res[["kcluster"]])), 2L)

## expression values and colData are carried through
rna <- SummarizedExperiment::assay(miniACC[["RNASeq2GeneNorm"]])
expect_equal(res[["ERBB2"]], unname(rna["ERBB2", res[["colname"]]]))
expect_identical(
    res[["gender"]],
    miniACC$gender[match(res[["primary"]], rownames(colData(miniACC)))]
)

## cluster centers are constant within each cluster
centers <- as.data.frame(res[, paste0("centers.", genes)])
for (cl in split(centers, res[["kcluster"]]))
    expect_identical(nrow(unique(cl)), 1L)

## reproducible with a seed
run <- function() quietly(
    clusterOn(miniACC, colDataCols = "gender", rownames = genes,
        experiments = "RNASeq2GeneNorm", seed = 123L)
)
expect_identical(run()[["kcluster"]], run()[["kcluster"]])

## unknown features give an informative error
expect_error(
    quietly(
        clusterOn(miniACC, colDataCols = "gender",
            rownames = c("ERBB2", "NOTAGENE"),
            experiments = "RNASeq2GeneNorm")
    ),
    "NOTAGENE"
)
