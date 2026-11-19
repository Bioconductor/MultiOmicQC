## Shared fixtures; sourced at the top of each test file.
## miniACC ships with no ranged experiments, so these helpers add small
## RangedSummarizedExperiment objects that share the RNASeq2GeneNorm samples.

data("miniACC", package = "MultiAssayExperiment", envir = environment())

makeRanged <- function(chrs, seqlengths = NULL, genome = NULL) {
    cn <- colnames(miniACC)[["RNASeq2GeneNorm"]]
    gr <- GenomicRanges::GRanges(
        chrs, IRanges::IRanges(seq_along(chrs), width = 1L)
    )
    if (!is.null(seqlengths))
        Seqinfo::seqlengths(gr) <- seqlengths
    if (!is.null(genome))
        Seqinfo::genome(gr) <- genome
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

## Simulated sex marker expression (counts) for the RNASeq2GeneNorm samples.
## 'sexes' gives the true sex of each sample (defaults to the reported
## gender); 'lossOfY' names samples with low Y-gene expression.
makeSexGenes <- function(sexes = NULL, lossOfY = character()) {
    samples <- colnames(miniACC)[["RNASeq2GeneNorm"]]
    smap <- sampleMap(miniACC)
    primary <- smap$primary[match(samples, smap$colname)]
    if (is.null(sexes))
        sexes <- miniACC$gender[match(primary, rownames(colData(miniACC)))]
    isMale <- sexes == "male"
    jitter <- 0.3 * sin(seq_along(samples))
    expr <- rbind(
        XIST = ifelse(isMale, 1, 10) + jitter,
        RPS4Y1 = ifelse(isMale, 11, 1) + jitter,
        DDX3Y = ifelse(isMale, 9, 0.5) - jitter,
        KDM5D = ifelse(isMale, 8, 0.5) + jitter / 2,
        ACTB = 12 + jitter
    )
    colnames(expr) <- samples
    yGenes <- c("RPS4Y1", "DDX3Y", "KDM5D")
    expr[yGenes, lossOfY] <- expr[yGenes, lossOfY] - 8
    2^expr - 1
}

## Add 'data' (by default simulated counts) as the 'sexGenes' experiment
addSexGenes <- function(..., data = makeSexGenes(...)) {
    suppressWarnings(
        c(miniACC, sexGenes = data, mapFrom = "RNASeq2GeneNorm")
    )
}

## A second expression platform simulated from the RNA-seq data (log2 scale
## plus noise), optionally with sample labels reordered by 'perm'
makePlatform2 <- function(perm = NULL, sd = 1, seed = 123L) {
    rna <- log2(SummarizedExperiment::assay(miniACC[["RNASeq2GeneNorm"]]) + 1)
    set.seed(seed)
    p2 <- rna + matrix(rnorm(length(rna), sd = sd), nrow(rna))
    if (!is.null(perm))
        p2 <- p2[, perm]
    colnames(p2) <- colnames(rna)
    p2
}

addPlatform2 <- function(...) {
    suppressWarnings(
        c(miniACC, platform2 = makePlatform2(...), mapFrom = "RNASeq2GeneNorm")
    )
}
