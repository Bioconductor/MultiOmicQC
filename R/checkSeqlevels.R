#' Ensure that all seqlevel styles match across experiments
#'
#' @importFrom MultiAssayExperiment hasRowRanges
#' @importFrom IRanges CharacterList
#' @importFrom DelayedArray rowRanges
#' @importFrom Biobase selectSome
#' @importFrom GenomeInfoDb seqlevelsStyle
#'
#' @param x A \linkS4class{MultiAssayExperiment}
#' @export
checkSeqlevels <- function(x) {
    hasRR <- hasRowRanges(x)

    seqlvls <- lapply(exps[hasRR], function(y) {
        GenomeInfoDb::seqlevelsStyle(rowRanges(y))
    })
    if (length(seqlvls) == 1L) { return(TRUE) }
    allConsistent <- sum(duplicated(seqlvls)) == length(seqlvls)-1L
    if (!allConsistent) {
    warning("Inconsistent 'seqlevelsStyle' formats found:\n",
    "    ", Biobase::selectSome(paste(unlist(seqlvls[!duplicated(seqlvls)]),
        collapse = ", ")))
    }
    allConsistent
}
