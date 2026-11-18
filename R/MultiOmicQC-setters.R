.setter <- function(x, FUN, value) {
    hasRR <- hasRowRanges(x)
    exps <- experiments(x)

    exps[hasRR] <- lapply(exps[hasRR], function(y) {
        FUN(y, value)
    })

    experiments(x) <- exps
    x
}

#' @name MultiOmicQC-setters
#'
#' @title Utility functions for setting metadata in MultiAssayExperiment
#'
#' @description Replacement methods that apply \code{seqlevelsStyle<-} and
#' \code{genome<-} to every ranged experiment (see
#' \code{\link[MultiAssayExperiment]{hasRowRanges}}) in a
#' \linkS4class{MultiAssayExperiment}. Experiments without row ranges are
#' left unchanged.
#'
#' @param x A \linkS4class{MultiAssayExperiment}
#'
#' @param value For \code{seqlevelsStyle<-}, a single string naming the
#' style (e.g., "UCSC" or "NCBI"). For \code{genome<-}, a single string
#' with the genome build (e.g., "hg19").
#'
#' @return A \linkS4class{MultiAssayExperiment} with updated ranged
#' experiments
#'
#' @examples
#' library(GenomicRanges)
#' data(miniACC, package = "MultiAssayExperiment")
#' cn <- colnames(miniACC)[["RNASeq2GeneNorm"]]
#' rse <- SummarizedExperiment::SummarizedExperiment(
#'     matrix(0, nrow = 2, ncol = length(cn), dimnames = list(NULL, cn)),
#'     rowRanges = GRanges(c("chr1", "chr2"), IRanges(1:2, width = 1))
#' )
#' mae <- c(miniACC, ranged = rse, mapFrom = "RNASeq2GeneNorm")
#' seqlevelsStyle(mae) <- "NCBI"
#' genome(mae) <- "hg19"
#' rowRanges(mae[["ranged"]])
#'
#' @aliases seqlevelsStyle<-,MultiAssayExperiment-method
#' @export
setReplaceMethod("seqlevelsStyle", "MultiAssayExperiment", function(x, value) {
    .setter(x, `seqlevelsStyle<-`, value)
})

#' @rdname MultiOmicQC-setters
#' @aliases genome<-,MultiAssayExperiment-method
#' @export
setReplaceMethod("genome", "MultiAssayExperiment", function(x, value) {
    .setter(x, `genome<-`, value)
})
