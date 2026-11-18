.defaultFemaleGenes <- "XIST"
.defaultMaleGenes <- c("RPS4Y1", "DDX3Y", "KDM5D", "UTY", "EIF1AY")

## Map common codings of sex to "female" / "male"; anything else is NA
.normalizeSex <- function(values) {
    lowered <- tolower(trimws(as.character(values)))
    out <- rep(NA_character_, length(lowered))
    out[lowered %in% c("f", "female")] <- "female"
    out[lowered %in% c("m", "male")] <- "male"
    unrecognized <- unique(as.character(values)[!is.na(lowered) & is.na(out)])
    if (length(unrecognized))
        warning(
            "Values in the 'sex' column not recognized as female or male ",
            "were treated as missing: ",
            paste(selectSome(unrecognized, 5L), collapse = ", "),
            call. = FALSE
        )
    out
}

## Mean of per-gene standardized expression; constant genes are dropped
.markerScore <- function(mat, genes) {
    sub <- mat[genes, , drop = FALSE]
    sds <- apply(sub, 1L, sd, na.rm = TRUE)
    sub <- sub[!is.na(sds) & sds > 0, , drop = FALSE]
    if (!nrow(sub))
        return(rep(NA_real_, ncol(mat)))
    unname(colMeans(t(scale(t(sub))), na.rm = TRUE))
}

.expressionMatrix <- function(expt) {
    mat <- if (is(expt, "SummarizedExperiment"))
        assay(expt, 1L, withDimnames = TRUE)
    else
        expt
    mat <- as.matrix(mat)
    if (!is.numeric(mat))
        stop("The expression data in 'experiment' must be numeric")
    mat
}

## Marker gene rows of 'experiment', log-transformed when requested
.markerMatrix <- function(x, experiment, femaleGenes, maleGenes, log) {
    mat <- .expressionMatrix(experiments(x)[[experiment]])
    fGenes <- intersect(femaleGenes, rownames(mat))
    mGenes <- intersect(maleGenes, rownames(mat))
    if (!length(fGenes) || !length(mGenes))
        stop(
            "'experiment' must contain at least one gene from ",
            "'femaleGenes' and one from 'maleGenes'"
        )
    mat <- mat[c(fGenes, mGenes), , drop = FALSE]
    if (log) {
        if (any(mat < 0, na.rm = TRUE))
            stop(
                "Negative expression values found; use 'log = FALSE' for ",
                "data already on a log scale"
            )
        mat <- log2(mat + 1)
    }
    list(mat = mat, femaleGenes = fGenes, maleGenes = mGenes)
}

## Thresholds halfway between the medians of the reported groups, or NULL
## (with a warning) when they cannot be calibrated
.sexThresholds <- function(femaleScore, maleScore, reported) {
    isF <- reported %in% "female"
    isM <- reported %in% "male"
    if (!any(isF) || !any(isM)) {
        warning(
            "Both sexes must be reported to check sex; no predictions made",
            call. = FALSE
        )
        return(NULL)
    }
    med <- function(score, group) median(score[group], na.rm = TRUE)
    fMeds <- c(female = med(femaleScore, isF), male = med(femaleScore, isM))
    mMeds <- c(female = med(maleScore, isF), male = med(maleScore, isM))
    if (anyNA(c(fMeds, mMeds)) || fMeds[["female"]] <= fMeds[["male"]] ||
        mMeds[["male"]] <= mMeds[["female"]]) {
        warning(
            "Marker expression does not separate reported females and ",
            "males; check 'femaleGenes', 'maleGenes', and 'sex'",
            call. = FALSE
        )
        return(NULL)
    }
    c(female = mean(fMeds), male = mean(mMeds))
}

.warnFlagged <- function(res) {
    flagged <- lapply(
        c(mismatch = "mismatch", ambiguous = "ambiguous"),
        function(s) res[["colname"]][res[["status"]] %in% s]
    )
    flagged <- Filter(length, flagged)
    if (!length(flagged))
        return(invisible(NULL))
    warning(
        "Samples whose expression disagrees with the reported sex:\n    ",
        paste0(names(flagged), " (", lengths(flagged), "): ",
            vapply(flagged, function(f)
                paste(selectSome(f, 4L), collapse = ", "), character(1L)),
            collapse = "\n    "),
        call. = FALSE
    )
}

#' Check reported sex against sex-specific gene expression
#'
#' Compares the sex recorded in the \code{colData} of a
#' \linkS4class{MultiAssayExperiment} with the expression of sex-specific
#' marker genes in one experiment, and flags samples whose expression
#' disagrees with the reported sex.
#'
#' @details
#' For each sample, a female score is computed as the mean standardized
#' (z-scored across samples) expression of \code{femaleGenes} (by default
#' \emph{XIST}), and a male score from \code{maleGenes} (by default
#' Y-chromosome genes). Genes that are missing from the experiment or have
#' constant expression are skipped.
#'
#' Each score is compared with a threshold halfway between its median in
#' samples reported as female and its median in samples reported as male.
#' Using the medians of the reported groups means the thresholds remain
#' valid when up to half of each group is mislabeled, and when one sex is
#' much rarer than the other. A sample is predicted female when its female
#' score is above the female threshold and its male score is below the male
#' threshold, and male in the opposite case. Samples where the two scores
#' disagree (e.g., low \emph{XIST} and low Y-gene expression, as seen in
#' male tumors that have lost chromosome Y) are called ambiguous.
#'
#' Predictions require both sexes to be reported, and the marker genes to
#' separate the two groups in the expected direction (higher female score
#' in reported females and higher male score in reported males). Otherwise,
#' a warning is issued and no predictions are made. Datasets with a single
#' reported sex, such as ovarian cancer cohorts, cannot be checked.
#'
#' Reported sex values are matched case-insensitively: "female" and "f" are
#' female, "male" and "m" are male. Other values are treated as missing.
#'
#' @seealso The \href{https://bioconductor.org/packages/massiR}{massiR}
#' package infers sex from Y-chromosome probe expression without reported
#' sex labels, which can help with microarray datasets that lack sex
#' annotations. Unlike \code{checkSex}, it does not use \emph{XIST}, so
#' male tumors that have lost chromosome Y expression are called female.
#'
#' @param x A \linkS4class{MultiAssayExperiment}
#'
#' @param experiment \code{character(1)} The name of a gene expression
#' experiment in \code{x}, with gene symbols as row names. For a
#' \code{SummarizedExperiment}, the first assay is used.
#'
#' @param sex \code{character(1)} The name of the \code{colData} column with
#' the reported sex (default \code{"sex"}).
#'
#' @param femaleGenes,maleGenes \code{character()} Marker genes expressed in
#' females and males, respectively. At least one gene from each set must be
#' present in \code{experiment}.
#'
#' @param log \code{logical(1)} Whether to transform expression with
#' \code{log2(x + 1)} before scoring (default \code{TRUE}). Use
#' \code{FALSE} for data already on a log scale.
#'
#' @return A \linkS4class{DataFrame} with one row per sample in
#' \code{experiment} and columns:
#' \describe{
#'   \item{primary}{The participant identifier from the \code{sampleMap}}
#'   \item{colname}{The sample identifier in \code{experiment}}
#'   \item{reported}{The reported sex, as "female", "male", or \code{NA}}
#'   \item{predicted}{The sex predicted from expression, or \code{NA}}
#'   \item{femaleScore, maleScore}{The marker scores}
#'   \item{status}{One of "match", "mismatch", "ambiguous", "unreported"
#'   (no reported sex), or "undetermined" (no predictions could be made)}
#' }
#' The marker genes used and the thresholds are stored in its
#' \code{metadata}. A warning lists mismatched and ambiguous samples.
#'
#' @examples
#' data(miniACC, package = "MultiAssayExperiment")
#' ## miniACC has no sex marker genes, so simulate an expression experiment
#' ## from the reported gender, with the sex of two samples swapped
#' samples <- colnames(miniACC)[["RNASeq2GeneNorm"]]
#' gender <- miniACC$gender[match(
#'     substr(samples, 1, 12), rownames(colData(miniACC))
#' )]
#' gender[1:2] <- ifelse(gender[1:2] == "female", "male", "female")
#' isMale <- gender == "male"
#' jitter <- 0.3 * sin(seq_along(samples))
#' expr <- rbind(
#'     XIST = ifelse(isMale, 1, 10) + jitter,
#'     RPS4Y1 = ifelse(isMale, 11, 1) + jitter,
#'     KDM5D = ifelse(isMale, 8, 0.5) - jitter
#' )
#' colnames(expr) <- samples
#' mae <- c(miniACC, sexGenes = 2^expr, mapFrom = "RNASeq2GeneNorm")
#' res <- checkSex(mae, "sexGenes", sex = "gender")
#' res[res$status != "match", ]
#'
#' @export
checkSex <- function(x, experiment, sex = "sex",
    femaleGenes = .defaultFemaleGenes, maleGenes = .defaultMaleGenes,
    log = TRUE) {
    if (!is(x, "MultiAssayExperiment"))
        stop("Provide a 'MultiAssayExperiment' object")
    if (!isScalarCharacter(experiment) || !experiment %in% names(x))
        stop("'experiment' must be the name of one experiment in 'x'")
    if (!isScalarCharacter(sex) || !sex %in% names(colData(x)))
        stop("'sex' must be the name of a column in 'colData(x)'")
    if (!isTRUEorFALSE(log))
        stop("'log' must be TRUE or FALSE")

    markers <- .markerMatrix(x, experiment, femaleGenes, maleGenes, log)
    mat <- markers[["mat"]]
    smap <- sampleMap(x)
    smap <- smap[smap[["assay"]] == experiment, , drop = FALSE]
    primary <- smap[["primary"]][match(colnames(mat), smap[["colname"]])]
    reported <- .normalizeSex(colData(x)[primary, sex])

    femaleScore <- .markerScore(mat, markers[["femaleGenes"]])
    maleScore <- .markerScore(mat, markers[["maleGenes"]])
    res <- DataFrame(
        primary = primary, colname = colnames(mat), reported = reported,
        predicted = NA_character_, femaleScore = femaleScore,
        maleScore = maleScore,
        status = ifelse(is.na(reported), "unreported", "undetermined")
    )
    metadata(res) <- markers[c("femaleGenes", "maleGenes")]

    thresholds <- .sexThresholds(femaleScore, maleScore, reported)
    if (is.null(thresholds))
        return(res)
    femaleLike <- femaleScore > thresholds[["female"]]
    maleLike <- maleScore > thresholds[["male"]]
    predicted <- ifelse(femaleLike & !maleLike, "female",
        ifelse(maleLike & !femaleLike, "male", NA_character_))
    res[["predicted"]] <- predicted
    res[["status"]] <- ifelse(is.na(reported), "unreported",
        ifelse(is.na(predicted), "ambiguous",
            ifelse(predicted == reported, "match", "mismatch")))
    metadata(res)[["thresholds"]] <- thresholds
    .warnFlagged(res)
    res
}
