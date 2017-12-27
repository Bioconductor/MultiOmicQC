#' @name MultiOmicQC-checkers
#'
#' @export
checkSeqlevels <- function(x) {
    .consistencyChecker(x, "seqlevels", seqlevels)
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
        warning("Inconsistent '", name, "' found:\n",
            "    ", Biobase::selectSome(paste(
                unlist(funList[!duplicated(funList)]), collapse = ", ")))
    }
    allConsistent
}
