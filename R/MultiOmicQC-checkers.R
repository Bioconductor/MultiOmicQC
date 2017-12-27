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
        warning("Inconsistent '", name, "' found:\n",
            "    ", Biobase::selectSome(paste(
                unlist(funList[!duplicated(funList)]), collapse = ", ")))
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
