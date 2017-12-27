#' @rdname MultiOmicQC-checkers
#'
#' @description A set of functions to check several aspects of ranged
#' experiments including \code{seqlengths}, \code{seqlevels} and
#' \code{seqlevelsStyle}.
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
