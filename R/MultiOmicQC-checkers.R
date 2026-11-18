.printInconsistent <- function(charList) {
    paste(
        vapply(seq_along(charList), function(i, string) {
            paste0(names(string)[[i]], ": ",
                paste(selectSome(string[[i]], 5L), collapse = ", ")
            )
        }, character(1L), string = charList)
    , collapse = "\n    ")
}

.warnInconsistent <- function(header, charList) {
    warning(header, "\n    ", .printInconsistent(charList), call. = FALSE)
}

## Seqinfo of each ranged experiment, named by experiment
.rangedSeqinfo <- function(x) {
    if (!is(x, "MultiAssayExperiment"))
        stop("Provide a 'MultiAssayExperiment' object")
    exps <- experiments(x)[hasRowRanges(x)]
    lapply(exps, function(y) seqinfo(rowRanges(y)))
}

## NA when the style cannot be determined (empty or unrecognized seqlevels)
.safeStyle <- function(si) {
    tryCatch(seqlevelsStyle(si), error = function(e) NA_character_)
}

#' @name MultiOmicQC-checkers
#'
#' @title Check seqinfo compatibility across ranged experiments
#'
#' @description A set of functions that check whether the genomic
#' annotations (\code{seqinfo}) of the ranged experiments in a
#' \linkS4class{MultiAssayExperiment} are compatible with each other.
#' Only experiments with row ranges (see
#' \code{\link[MultiAssayExperiment]{hasRowRanges}}) are checked.
#'
#' The checks follow the rules of \code{merge()} for \code{Seqinfo}
#' objects: differences in seqlevel order, experiments covering only some of
#' the seqlevels, and unknown (\code{NA}) seqlengths or genomes are
#' compatible. Conflicting known values are not.
#'
#' @section checkSeqlevels:
#' By default (\code{exact = FALSE}), fails when two ranged experiments
#' have no seqlevels in common, which usually points to a naming style or
#' genome build mismatch. The order of seqlevels and experiments that
#' cover a subset of the seqlevels (e.g., no \code{chrY}) are accepted.
#' With \code{exact = TRUE}, fails unless all experiments have the same set
#' of seqlevels (in any order); the warning lists the seqlevels missing from
#' each experiment.
#'
#' @section checkSeqlengths:
#' Fails when a seqlevel shared by two or more experiments has different
#' known lengths. Unknown (\code{NA}) lengths are ignored.
#'
#' @section checkSeqlevelsStyle:
#' Fails when the ranged experiments have no naming style (e.g., "UCSC" or
#' "NCBI") in common. An experiment can match several styles (e.g.,
#' \code{c("1", "2")} matches both "NCBI" and "Ensembl"). Experiments whose
#' style cannot be determined are skipped.
#'
#' @section checkGenome:
#' Fails when the ranged experiments report more than one genome build
#' (e.g., "hg19" and "hg38"). Unknown (\code{NA}) genomes are ignored.
#' Unlike \code{merge()}, genomes are compared even when the experiments
#' share no seqlevels.
#'
#' @param x A \linkS4class{MultiAssayExperiment}
#'
#' @param exact \code{logical(1)} Whether all ranged experiments must have
#' the same set of seqlevels (default \code{FALSE}).
#'
#' @return A single logical: \code{TRUE} when the ranged experiments are
#' compatible (or when there are fewer than two ranged experiments to
#' compare), \code{FALSE} otherwise. When \code{FALSE}, a warning describes
#' the conflicts.
#'
#' @examples
#' library(GenomicRanges)
#' data(miniACC, package = "MultiAssayExperiment")
#' cn <- colnames(miniACC)[["RNASeq2GeneNorm"]]
#' makeRanged <- function(chrs, genome = NA) {
#'     gr <- GRanges(chrs, IRanges(seq_along(chrs), width = 1))
#'     genome(gr) <- genome
#'     SummarizedExperiment::SummarizedExperiment(
#'         matrix(0, nrow = length(chrs), ncol = length(cn),
#'             dimnames = list(NULL, cn)),
#'         rowRanges = gr
#'     )
#' }
#' mae <- c(miniACC,
#'     cnA = makeRanged(c("chr1", "chr2", "chrX"), "hg19"),
#'     cnB = makeRanged(c("chr2", "chr1"), "hg38"),
#'     mapFrom = c(1L, 1L))
#' ## compatible: different order and cnB lacks chrX
#' checkSeqlevels(mae)
#' checkSeqlevelsStyle(mae)
#' ## not identical sets
#' checkSeqlevels(mae, exact = TRUE)
#' ## hg19 vs hg38
#' checkGenome(mae)
#'
#' @export
checkSeqlevels <- function(x, exact = FALSE) {
    if (!isTRUEorFALSE(exact))
        stop("'exact' must be TRUE or FALSE")
    levs <- lapply(.rangedSeqinfo(x), seqlevels)
    if (length(levs) < 2L)
        return(TRUE)

    if (exact) {
        allLevs <- unique(unlist(levs, use.names = FALSE))
        missingLevs <- Filter(length,
            lapply(levs, function(lv) setdiff(allLevs, lv)))
        if (!length(missingLevs))
            return(TRUE)
        .warnInconsistent(
            "Inconsistent 'seqlevels' found; missing from:", missingLevs
        )
        return(FALSE)
    }

    pairs <- combn(names(levs), 2L, simplify = FALSE)
    disjoint <- Filter(
        function(p) !length(intersect(levs[[p[1L]]], levs[[p[2L]]])), pairs
    )
    if (!length(disjoint))
        return(TRUE)
    warning(
        "Ranged experiments with no 'seqlevels' in common:\n    ",
        paste(vapply(disjoint, paste, character(1L), collapse = " & "),
            collapse = "\n    "),
        call. = FALSE
    )
    FALSE
}

#' @rdname MultiOmicQC-checkers
#'
#' @export
checkSeqlengths <- function(x) {
    lens <- lapply(.rangedSeqinfo(x), seqlengths)
    if (length(lens) < 2L)
        return(TRUE)

    lensDF <- data.frame(
        experiment = rep(names(lens), lengths(lens)),
        seqlevel = unlist(lapply(lens, names), use.names = FALSE),
        seqlength = unlist(lens, use.names = FALSE)
    )
    lensDF <- lensDF[!is.na(lensDF[["seqlength"]]), , drop = FALSE]
    bySeqlevel <- split(lensDF,
        factor(lensDF[["seqlevel"]], levels = unique(lensDF[["seqlevel"]])))
    conflicts <- Filter(
        function(d) length(unique(d[["seqlength"]])) > 1L, bySeqlevel
    )
    if (!length(conflicts))
        return(TRUE)
    .warnInconsistent(
        "Inconsistent 'seqlengths' found:",
        lapply(conflicts, function(d)
            paste0(d[["experiment"]], "=", d[["seqlength"]]))
    )
    FALSE
}

#' @rdname MultiOmicQC-checkers
#'
#' @export
checkSeqlevelsStyle <- function(x) {
    styles <- lapply(.rangedSeqinfo(x), .safeStyle)
    styles <- Filter(function(s) !anyNA(s), styles)
    if (length(styles) < 2L)
        return(TRUE)
    if (length(Reduce(intersect, styles)))
        return(TRUE)
    .warnInconsistent("Inconsistent 'seqlevelsStyle' found:", styles)
    FALSE
}

#' @rdname MultiOmicQC-checkers
#'
#' @export
checkGenome <- function(x) {
    gens <- lapply(.rangedSeqinfo(x), function(si) {
        g <- unname(genome(si))
        unique(g[!is.na(g)])
    })
    gens <- Filter(length, gens)
    if (length(unique(unlist(gens, use.names = FALSE))) <= 1L)
        return(TRUE)
    .warnInconsistent("Inconsistent 'genome' found:", gens)
    FALSE
}
