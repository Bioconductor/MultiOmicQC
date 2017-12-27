#' Ensure uniform seqnames across experiments
#'
#' This function checks seqnames of all ranged experiments for
#' consistency. This can become problematic when ranged experiments are
#' not using identical seqnames.
#'
#' @param x A \linkS4class{MultiAssayExperiment}
#'
#' @export
checkSeqlevels <- function(x) {
    .consistencyChecker(x, "seqlevels", seqlevels)
}


.consistencyChecker <- function(x, name, FUN) {
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
