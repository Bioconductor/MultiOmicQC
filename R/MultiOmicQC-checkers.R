.printInconsistent <- function(charList) {
    paste(
        vapply(seq_along(charList), function(i, string) {
            paste0(names(string)[[i]], ": ",
                paste(selectSome(string[[i]], 5L), collapse = ", ")
            )
        }, character(1L), string = charList)
    , collapse = "\n    ")
}

.consistencyChecker <- function(x, name, FUN) {
    if (!is(x, "MultiAssayExperiment"))
        stop("Provide a 'MultiAssayExperiment' object")
    hasRR <- hasRowRanges(x)
    exps <- experiments(x)

    funList <- lapply(exps[hasRR], function(y) {
        FUN(rowRanges(y))
    })
    if (length(funList) == 1L) { return(TRUE) }
    allConsistent <- all(duplicated(funList)[-1L])
    if (!allConsistent) {
        inconList <- funList[!duplicated(funList)]
        warning(
            "Inconsistent '", name, "' found:\n", "    ",
            .printInconsistent(inconList)
        )
    }
    allConsistent
}

#' @name MultiOmicQC-checkers
#'
#' @title Check seqinfo consistency across ranged experiments
#'
#' @description A set of functions to check several aspects of ranged
#' experiments including \code{seqlengths}, \code{seqlevels} and
#' \code{seqlevelsStyle}. These group of functions return a logical scalar
#' value when the names, styles, etc. are consistent accross all experiments.
#'
#' @section checkSeqlengths:
#' Runs and checks \code{seqlengths} on all ranged experiments.
#'
#' @section checkSeqlevels:
#' Checks \code{seqnames} levels of all ranged experiments for
#' consistency. Inconsistent seqlevels can be problematic when ranged
#' experiments differ by annotations.
#'
#' @section checkSeqlevelsStyle:
#' Checks all \code{seqlevelsStyle} of all ranged experiments by comparing
#' the first style with the rest using the \code{duplicated} function.
#'
#' @param x A \linkS4class{MultiAssayExperiment}
#'
#' @return A single logical: \code{TRUE} when all ranged experiments agree
#' (or when there are fewer than two ranged experiments), \code{FALSE}
#' otherwise. When \code{FALSE}, a warning lists the inconsistent values.
#'
#' @examples
#' library(GenomicRanges)
#' data(miniACC, package = "MultiAssayExperiment")
#' cn <- colnames(miniACC)[["RNASeq2GeneNorm"]]
#' makeRanged <- function(chrs) SummarizedExperiment::SummarizedExperiment(
#'     matrix(0, nrow = length(chrs), ncol = length(cn),
#'         dimnames = list(NULL, cn)),
#'     rowRanges = GRanges(chrs, IRanges(seq_along(chrs), width = 1))
#' )
#' mae <- c(miniACC, cnA = makeRanged(c("chr1", "chr2")),
#'     cnB = makeRanged(c("1", "2")), mapFrom = c(1L, 1L))
#' checkSeqlevels(mae)
#' checkSeqlevelsStyle(mae)
#'
#' @export
checkSeqlengths <- function(x) {
    .consistencyChecker(x, "seqlengths", seqlengths)
}

#' @rdname MultiOmicQC-checkers
#'
#' @export
checkSeqlevels <- function(x) {
    .consistencyChecker(x, "seqlevels", seqlevels)
}

#' @rdname MultiOmicQC-checkers
#'
#' @export
checkSeqlevelsStyle <- function(x) {
    .consistencyChecker(x, "seqlevelsStyle", seqlevelsStyle)
}
