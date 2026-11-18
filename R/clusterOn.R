#' Check expression of a given feature against clinical variable
#'
#' Function that outputs a \link[S4Vectors]{DataFrame} with participant ID,
#' sample ID, the select colData column, the expression values for select
#' rownames, and the center values for each gene by cluster.
#'
#' @param MultiAssayExperiment A MultiAssayExperiment object
#' @param colDataCols Select columns from the MultiAssayExperiment colData
#' DataFrame
#' @param rownames Features to be used for clustering
#' (e.g., a set of gene names)
#' @param experiments A \code{character} vector indicating assays of interest
#' in the \code{ExperimentList}
#' @param seed A single integer value passed to \link{set.seed} (default NULL)
#'
#' @return A DataFrame with appended cluster and center values
#' @examples
#' data(miniACC, package = "MultiAssayExperiment")
#' clusterOn(miniACC, colDataCols = "gender",
#'     rownames = c("CDKN1B", "ERBB2", "G6PD"),
#'     experiments = "RNASeq2GeneNorm", seed = 42L)
#'
#' @export clusterOn
clusterOn <- function(MultiAssayExperiment, colDataCols, rownames,
    experiments, seed = NULL) {
    MultiAssayExperiment <- MultiAssayExperiment[rownames, , experiments]
    longMulti <- longForm(MultiAssayExperiment, colDataCols = colDataCols)
    longMulti <- as.data.frame(longMulti)[, names(longMulti) != "assay"]

    missingRows <- setdiff(rownames, longMulti[["rowname"]])
    if (length(missingRows))
        stop("'rownames' not found in 'experiments': ",
            paste(missingRows, collapse = ", "))

    wideMulti <- reshape(longMulti, direction = "wide", timevar = "rowname",
        idvar = "primary", v.names = "value")
    names(wideMulti) <- gsub("value\\.", "", names(wideMulti))

    expSubset <- wideMulti[, rownames]
    scaledExp <- apply(expSubset, 2, scale)
    if (!is.null(seed))
        set.seed(seed)
    kma <- kmeans(scaledExp, centers = 2)
    kcenters <- kma[["centers"]][kma[["cluster"]], ]
    rownames(kcenters) <- NULL
    colnames(kcenters) <- paste0("centers.", colnames(kcenters))
    kcenters <- cbind(kcluster = kma[["cluster"]], kcenters)

    DataFrame(wideMulti, kcenters)
}
