.setter <- function(x, name, FUN, value) {
    if (!is(x, "MultiAssayExperiment"))
        stop("Provide a 'MultiAssayExperiment' object")
    hasRR <- hasRowRanges(x)
    exps <- experiments(x)

    exps[hasRR] <- lapply(exps[hasRR], function(y) {
        FUN(y, value)
    })

    BiocGenerics:::replaceSlots(x, ExperimentList = exps, check = FALSE)
}

#' @name MultiOmicQC-setters
#'
#' @title Utility functions for setting metadata in MultiAssayExperiment
#'
#' @param x A \linkS4class{MultiAssayExperiment}
#' @param style A character vector with a single element to specify the style
#'
#' @export
setReplaceMethod("seqlevelsStyle", "MultiAssayExperiment", function(x, value) {
    .setter(x, "seqlevelsStyle", GenomeInfoDb::`seqlevelsStyle<-`, value)
})


#' @rdname MultiOmicQC-setters
#' @aliases `genome<-`
#' @export
setReplaceMethod("genome", "MultiAssayExperiment", function(x, value) {
    .setter(x, "genome", GenomeInfoDb::`genome<-`, value)
})
