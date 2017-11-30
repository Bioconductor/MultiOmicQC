.hasRanges <- function(object) {
is(object, "RangedSummarizedExperiment") ||
    is(object, "RaggedExperiment")
}

#' Ensure that all seqlevel styles match across experiments
#'
#' @param x A \linkS4class{MultiAssayExperiment}
#' @export
checkSeqlevels <- function(x) {
    exps <- experiments(x)
    rangedExp <- vapply(exps, .hasRanges, logical(1L))

    seqlvls <- lapply(exps[rangedExp], function(y) {
        seqlevelsStyle(rowRanges(y))
    })
    allStyles <- Reduce(identical, seqlvls)
    if (!allStyles) {
    warning("Inconsistent 'seqlevelsStyle' formats found")
    ## Report mismatches here
    }
    allStyles
}
