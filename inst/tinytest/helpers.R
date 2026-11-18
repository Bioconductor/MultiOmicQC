## Shared fixtures; sourced at the top of each test file.
## miniACC ships with no ranged experiments, so these helpers add small
## RangedSummarizedExperiment objects that share the RNASeq2GeneNorm samples.

data("miniACC", package = "MultiAssayExperiment", envir = environment())

makeRanged <- function(chrs, seqlengths = NULL) {
    cn <- colnames(miniACC)[["RNASeq2GeneNorm"]]
    gr <- GenomicRanges::GRanges(
        chrs, IRanges::IRanges(seq_along(chrs), width = 1L)
    )
    if (!is.null(seqlengths))
        GenomeInfoDb::seqlengths(gr) <- seqlengths
    SummarizedExperiment::SummarizedExperiment(
        matrix(0, nrow = length(chrs), ncol = length(cn),
            dimnames = list(NULL, cn)),
        rowRanges = gr
    )
}

## Append named ranged experiments to miniACC
addRanged <- function(...) {
    newExps <- list(...)
    suppressWarnings(
        do.call(
            c,
            c(list(miniACC), newExps,
                list(mapFrom = rep("RNASeq2GeneNorm", length(newExps))))
        )
    )
}

quietly <- function(expr) suppressMessages(suppressWarnings(expr))
