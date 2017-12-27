#' Ensure that all seqlevel styles match across experiments
#'
#' @param x A \linkS4class{MultiAssayExperiment}
#' @export
checkSeqlevelsStyle <- function(x) {
    .consistencyChecker(x, "seqlevelsStyle", seqlevelsStyle)
}
