## Matched matrices for two experiments: shared patients (columns, named by
## 'primary') and shared features (rows), via intersectColumns and
## intersectRows
.pairExperiments <- function(x, experiments) {
    if (!is(x, "MultiAssayExperiment"))
        stop("Provide a 'MultiAssayExperiment' object")
    if (!is.character(experiments) || length(experiments) != 2L ||
        anyNA(experiments) || anyDuplicated(experiments) ||
        !all(experiments %in% names(x)))
        stop("'experiments' must be the names of two experiments in 'x'")

    ## a two-experiment object; building it from matching parts avoids the
    ## messages that subsetting with '[' gives about dropped experiments
    smap <- sampleMap(x)
    smap <- smap[smap[["assay"]] %in% experiments, , drop = FALSE]
    smap[["assay"]] <- factor(smap[["assay"]], levels = experiments)
    sub <- MultiAssayExperiment(
        experiments = experiments(x)[experiments],
        colData = colData(x)[unique(smap[["primary"]]), , drop = FALSE],
        sampleMap = smap
    )
    sub <- intersectRows(intersectColumns(sub))
    if (any(anyReplicated(sub)))
        stop(
            "Some patients have more than one sample in 'experiments'; ",
            "keep one sample per patient first, e.g., with ",
            "'mergeReplicates()' or 'TCGAutils::TCGAprimaryTumors()'"
        )

    smap <- sampleMap(sub)
    mats <- lapply(experiments, function(e) {
        mat <- .expressionMatrix(experiments(sub)[[e]])
        mat <- mat[!duplicated(rownames(mat)), , drop = FALSE]
        emap <- smap[smap[["assay"]] == e, , drop = FALSE]
        primary <- emap[["primary"]][match(colnames(mat), emap[["colname"]])]
        list(mat = mat, primary = primary, colname = colnames(mat))
    })
    patients <- sort(
        intersect(mats[[1L]][["primary"]], mats[[2L]][["primary"]])
    )
    features <- intersect(rownames(mats[[1L]][["mat"]]),
        rownames(mats[[2L]][["mat"]]))
    if (!length(patients) || !length(features))
        stop("The two experiments share no patients or no features")
    out <- lapply(mats, function(m) {
        idx <- match(patients, m[["primary"]])
        mat <- m[["mat"]][features, idx, drop = FALSE]
        colnames(mat) <- patients
        list(mat = mat, colname = m[["colname"]][idx])
    })
    names(out) <- experiments
    out
}

## Row-wise correlation of matched matrices, using only the samples observed
## in both; Spearman correlations are Pearson correlations of the ranks
.rowCor <- function(A, B, method) {
    miss <- is.na(A) | is.na(B)
    A[miss] <- NA
    B[miss] <- NA
    if (identical(method, "spearman")) {
        A <- rowRanks(A, ties.method = "average")
        B <- rowRanks(B, ties.method = "average")
    }
    n <- rowSums(!miss)
    Ac <- A - rowMeans(A, na.rm = TRUE)
    Bc <- B - rowMeans(B, na.rm = TRUE)
    r <- rowSums(Ac * Bc, na.rm = TRUE) /
        sqrt(rowSums(Ac^2, na.rm = TRUE) * rowSums(Bc^2, na.rm = TRUE))
    r[n < 3L | !is.finite(r)] <- NA_real_
    list(r = unname(r), n = unname(n))
}

#' Correlate matched features across two experiments
#'
#' For two experiments that measure the same features, such as gene
#' expression and protein abundance, computes the correlation of each
#' shared feature across the patients present in both, and compares it
#' with a permutation null distribution.
#'
#' @details
#' Patients and features shared by both experiments are found with
#' \code{\link[MultiAssayExperiment]{intersectColumns}} and
#' \code{\link[MultiAssayExperiment]{intersectRows}}; features are matched
#' by row name (e.g., gene symbols). Map feature identifiers to a common
#' type beforehand if needed, for example UniProt accessions to gene
#' symbols with \code{AnnotationDbi::mapIds()}. Each experiment must have
#' at most one sample per patient; see
#' \code{\link[MultiAssayExperiment]{mergeReplicates}} and
#' \code{TCGAutils::TCGAprimaryTumors()}.
#'
#' For each feature, the correlation uses the patients observed in both
#' experiments. The null distribution is built by shuffling which patient's
#' sample in the second experiment is paired with each sample in the first,
#' \code{nperm} times, and pooling the correlations of all features. The
#' one-sided empirical p-value of a feature is the proportion of null
#' correlations at least as large as its correlation. Shuffling uses the
#' random number generator; call \code{set.seed()} beforehand for
#' reproducible p-values.
#'
#' Related molecular layers usually correlate positively. A median feature
#' correlation that is not above the 97.5th percentile of the null suggests
#' that the experiments do not measure related features, or that many
#' samples are mislabeled, and gives a warning.
#'
#' @param x A \linkS4class{MultiAssayExperiment}
#'
#' @param experiments \code{character(2)} The names of the two experiments
#' to compare. For a \code{SummarizedExperiment}, the first assay is used.
#'
#' @param method \code{character(1)} The correlation method, either
#' \code{"spearman"} (default) or \code{"pearson"}.
#'
#' @param nperm \code{integer(1)} The number of permutations for the null
#' distribution (default 50). Use 0 to skip the permutations.
#'
#' @param minObs \code{integer(1)} The minimum number of patients observed
#' in both experiments for a feature's correlation to be computed
#' (default 10).
#'
#' @return A \linkS4class{DataFrame} with one row per shared feature and
#' columns \code{feature}, \code{n} (patients observed in both
#' experiments), \code{correlation}, \code{pvalue}, and \code{padj}
#' (Benjamini-Hochberg adjusted). Its \code{metadata} holds the
#' experiments, the method, the number of patients, the number of
#' permutations, and quantiles of the null distribution.
#'
#' @seealso \code{\link{checkSampleIdentity}} to check that each patient's
#' samples match across the two experiments.
#'
#' @examples
#' data(miniACC, package = "MultiAssayExperiment")
#' ## gene expression vs reverse-phase protein array
#' set.seed(1)
#' res <- checkFeatureCorrelation(miniACC, c("RNASeq2GeneNorm", "RPPAArray"))
#' res[order(-res$correlation), ]
#' metadata(res)$null
#'
#' \donttest{
#' ## CPTAC mass-spectrometry proteomics of TCGA ovarian tumors, from the
#' ## CPTACdata package (requires an internet connection)
#' if (requireNamespace("CPTACdata", quietly = TRUE) &&
#'     requireNamespace("curatedTCGAData", quietly = TRUE) &&
#'     requireNamespace("TCGAutils", quietly = TRUE)) {
#'     ## the CPTAC data are provided under the PDC Data Use Agreement at
#'     ## https://proteomic.datacommons.cancer.gov/pdc/data-use-guidelines;
#'     ## read it before running the next line, which accepts it
#'     op <- options(CPTACdata.acceptDUA = TRUE)
#'     ov <- curatedTCGAData::curatedTCGAData("OV",
#'         "mRNAArray_TX_ht_hg_u133a", version = "2.1.1", dry.run = FALSE)
#'     ## add the JHU and PNNL proteomes from the Proteomic Data Commons
#'     ov <- CPTACdata::appendCPTAC(ov)
#'     options(op)
#'     ## keep one sample per patient
#'     ov <- TCGAutils::TCGAprimaryTumors(ov)
#'     array <- grep("u133a", names(ov), value = TRUE)
#'     set.seed(1)
#'     res <- checkFeatureCorrelation(ov, c(array, "CPTAC_JHU_Proteome"))
#'     summary(res$correlation)
#'     metadata(res)$null
#' }
#' }
#'
#' @export
checkFeatureCorrelation <- function(x, experiments,
    method = c("spearman", "pearson"), nperm = 50L, minObs = 10L) {
    method <- match.arg(method)
    if (!isScalarNumber(nperm) || nperm < 0)
        stop("'nperm' must be a single non-negative number")
    if (!isScalarNumber(minObs) || minObs < 3)
        stop("'minObs' must be a single number of at least 3")
    pairs <- .pairExperiments(x, experiments)
    A <- pairs[[1L]][["mat"]]
    B <- pairs[[2L]][["mat"]]
    if (ncol(A) < minObs)
        stop("Fewer than 'minObs' patients are shared by 'experiments'")

    obs <- .rowCor(A, B, method)
    r <- obs[["r"]]
    r[obs[["n"]] < minObs] <- NA_real_
    res <- DataFrame(feature = rownames(A), n = obs[["n"]],
        correlation = r, pvalue = NA_real_, padj = NA_real_)
    metadata(res) <- list(experiments = experiments, method = method,
        nPatients = ncol(A), nperm = as.integer(nperm))
    if (nperm == 0)
        return(res)

    null <- unlist(lapply(seq_len(nperm), function(i) {
        perm <- .rowCor(A, B[, sample.int(ncol(B)), drop = FALSE], method)
        perm[["r"]][perm[["n"]] >= minObs]
    }), use.names = FALSE)
    null <- sort(null[!is.na(null)])
    atLeast <- length(null) - findInterval(r, null, left.open = TRUE)
    res[["pvalue"]] <- ifelse(is.na(r), NA_real_,
        (1 + atLeast) / (1 + length(null)))
    res[["padj"]] <- p.adjust(res[["pvalue"]], method = "BH")
    nullQ <- quantile(null, c(0.025, 0.5, 0.975), names = FALSE)
    metadata(res)[["null"]] <- c(q2.5 = nullQ[1L], median = nullQ[2L],
        q97.5 = nullQ[3L])

    medianCor <- median(r, na.rm = TRUE)
    if (!is.na(medianCor) && medianCor <= nullQ[3L])
        warning(
            "The median feature correlation (", round(medianCor, 3),
            ") is not above the 97.5th percentile of the permutation null (",
            round(nullQ[3L], 3), "); check that the experiments measure ",
            "related features and that samples are labeled correctly",
            call. = FALSE
        )
    res
}

## Status of each patient from the sample correlation matrix C (rows:
## first experiment, columns: second, both ordered by patient) and the
## z-scores of the matched correlations
.identityStatus <- function(C, zscore, minZ) {
    patients <- rownames(C)
    idx <- seq_along(patients)
    bestRow <- apply(C, 1L, which.max)
    rankRow <- vapply(idx, function(i)
        rank(-C[i, ], na.last = "keep")[i], numeric(1L))
    rankCol <- vapply(idx, function(j)
        rank(-C[, j], na.last = "keep")[j], numeric(1L))
    low <- is.na(zscore) | zscore < minZ
    swapped <- low & bestRow != idx & bestRow[bestRow] == idx & low[bestRow]
    status <- ifelse(!low, "match", ifelse(swapped, "swap", "mismatch"))
    list(rank = unname(rankRow), reverseRank = unname(rankCol),
        bestMatch = unname(patients[bestRow]), status = unname(status))
}

#' Check that samples match across two experiments
#'
#' Verifies that each patient's samples in two experiments that measure the
#' same features, such as gene expression and protein abundance or two
#' expression platforms, look more like each other than like any other
#' patient's samples. Detects sample swaps and mislabeled samples.
#'
#' @details
#' Patients and features are matched as in
#' \code{\link{checkFeatureCorrelation}}. Each feature is standardized
#' (z-scored) across patients in each experiment, so that differences
#' between patients, rather than differences in abundance between
#' features, drive the comparison. Then every sample in the first
#' experiment is correlated with every sample in the second across
#' features.
#'
#' For each patient, the correlation between its two samples is compared
#' with the correlations between its sample in the first experiment and
#' all other patients' samples in the second, as a z-score. A correctly
#' labeled patient's samples stand out (\code{zscore} of at least
#' \code{minZ}) and are reported as a \code{"match"}. Two patients below
#' \code{minZ} whose samples each correlate best with the other patient's
#' are reported as a \code{"swap"}. Other patients below \code{minZ} are
#' reported as a \code{"mismatch"}; this includes samples of low quality
#' and samples from different tissue, as well as mislabeled samples.
#'
#' Identity calls need many informative shared features: in matched
#' gene expression and mass-spectrometry proteomics with thousands of
#' genes, correctly labeled patients typically have z-scores of 5 or more.
#' When the median z-score across patients is below 3, a warning notes
#' that the calls are unreliable, for example with a small protein array
#' panel.
#'
#' @inheritParams checkFeatureCorrelation
#'
#' @param minZ \code{numeric(1)} The minimum z-score of a patient's matched
#' correlation for its samples to be called a match (default 2).
#'
#' @return A \linkS4class{DataFrame} with one row per patient shared by
#' the two experiments and columns:
#' \describe{
#'   \item{primary}{The patient identifier}
#'   \item{colname1, colname2}{The sample identifiers in the first and
#'   second experiment}
#'   \item{correlation}{The correlation between the patient's two samples}
#'   \item{zscore}{\code{correlation} standardized against the correlations
#'   of the patient's sample in the first experiment with all other
#'   samples in the second}
#'   \item{rank}{The rank of the patient's own sample among all samples in
#'   the second experiment, by correlation with its sample in the first}
#'   \item{reverseRank}{The same, from the second experiment to the first}
#'   \item{bestMatch, bestCorrelation}{The patient whose sample in the
#'   second experiment correlates best with this patient's sample in the
#'   first, and that correlation}
#'   \item{status}{"match", "swap", or "mismatch"; see Details}
#' }
#' The full sample correlation matrix is stored as \code{correlations} in
#' its \code{metadata}. A warning lists swapped and mismatched patients.
#'
#' @examples
#' data(miniACC, package = "MultiAssayExperiment")
#' ## simulate a second expression platform from the RNA-seq data, with
#' ## the samples of two patients swapped
#' rna <- log2(SummarizedExperiment::assay(miniACC[["RNASeq2GeneNorm"]]) + 1)
#' set.seed(1)
#' platform2 <- rna + matrix(rnorm(length(rna), sd = 1), nrow(rna))
#' colnames(platform2)[1:2] <- colnames(platform2)[2:1]
#' mae <- c(miniACC, platform2 = platform2, mapFrom = "RNASeq2GeneNorm")
#' res <- checkSampleIdentity(mae, c("RNASeq2GeneNorm", "platform2"))
#' res[res$status != "match", ]
#'
#' ## 33 genes on a protein array are too few for reliable identity calls
#' res <- checkSampleIdentity(miniACC, c("RNASeq2GeneNorm", "RPPAArray"))
#' summary(res$zscore)
#'
#' \donttest{
#' ## CPTAC mass-spectrometry proteomics of TCGA ovarian tumors, from the
#' ## CPTACdata package (requires an internet connection)
#' if (requireNamespace("CPTACdata", quietly = TRUE) &&
#'     requireNamespace("curatedTCGAData", quietly = TRUE) &&
#'     requireNamespace("TCGAutils", quietly = TRUE)) {
#'     ## the CPTAC data are provided under the PDC Data Use Agreement at
#'     ## https://proteomic.datacommons.cancer.gov/pdc/data-use-guidelines;
#'     ## read it before running the next line, which accepts it
#'     op <- options(CPTACdata.acceptDUA = TRUE)
#'     ov <- curatedTCGAData::curatedTCGAData("OV",
#'         "mRNAArray_TX_ht_hg_u133a", version = "2.1.1", dry.run = FALSE)
#'     ## add the JHU and PNNL proteomes from the Proteomic Data Commons
#'     ov <- CPTACdata::appendCPTAC(ov)
#'     options(op)
#'     ## keep one sample per patient
#'     ov <- TCGAutils::TCGAprimaryTumors(ov)
#'     array <- grep("u133a", names(ov), value = TRUE)
#'     ## two samples appear swapped in the PNNL proteome
#'     res <- checkSampleIdentity(ov, c(array, "CPTAC_PNNL_Proteome"))
#'     res[res$status != "match", ]
#' }
#' }
#'
#' @export
checkSampleIdentity <- function(x, experiments,
    method = c("spearman", "pearson"), minZ = 2) {
    method <- match.arg(method)
    if (!isScalarNumber(minZ))
        stop("'minZ' must be a single number")
    pairs <- .pairExperiments(x, experiments)
    A <- pairs[[1L]][["mat"]]
    B <- pairs[[2L]][["mat"]]
    if (ncol(A) < 3L || nrow(A) < 3L)
        stop("At least 3 shared patients and 3 shared features are needed")

    ## constant features have no z-scores
    varies <- apply(A, 1L, sd, na.rm = TRUE) > 0 &
        apply(B, 1L, sd, na.rm = TRUE) > 0
    A <- A[which(varies), , drop = FALSE]
    B <- B[which(varies), , drop = FALSE]
    rowZ <- function(m) t(scale(t(m)))
    C <- cor(rowZ(A), rowZ(B), method = method, use = "pairwise.complete.obs")
    matched <- diag(C)
    offDiag <- C
    diag(offDiag) <- NA
    zscore <- (matched - rowMeans(offDiag, na.rm = TRUE)) /
        apply(offDiag, 1L, sd, na.rm = TRUE)
    st <- .identityStatus(C, zscore, minZ)

    res <- DataFrame(
        primary = colnames(A),
        colname1 = pairs[[1L]][["colname"]],
        colname2 = pairs[[2L]][["colname"]],
        correlation = unname(matched), zscore = unname(zscore),
        rank = st[["rank"]], reverseRank = st[["reverseRank"]],
        bestMatch = st[["bestMatch"]],
        bestCorrelation = unname(apply(C, 1L, max, na.rm = TRUE)),
        status = st[["status"]]
    )
    metadata(res) <- list(experiments = experiments, method = method,
        nFeatures = nrow(A), minZ = minZ, correlations = C)
    medianZ <- median(zscore, na.rm = TRUE)
    if (is.na(medianZ) || medianZ < 3)
        warning(
            "Matched samples barely stand out (median z-score ",
            round(medianZ, 2), " with ", nrow(A), " shared features); ",
            "identity calls are unreliable",
            call. = FALSE
        )
    else
        .warnIdentity(res)
    res
}

.warnIdentity <- function(res) {
    swaps <- res[res[["status"]] %in% "swap", , drop = FALSE]
    swapPairs <- unique(vapply(seq_len(nrow(swaps)), function(i)
        paste(sort(c(swaps[["primary"]][i], swaps[["bestMatch"]][i])),
            collapse = " <-> "), character(1L)))
    mismatched <- res[["primary"]][res[["status"]] %in% "mismatch"]
    msgs <- c(
        if (length(swapPairs))
            paste0("swap (", length(swapPairs), "): ",
                paste(selectSome(swapPairs, 4L), collapse = ", ")),
        if (length(mismatched))
            paste0("mismatch (", length(mismatched), "): ",
                paste(selectSome(mismatched, 4L), collapse = ", "))
    )
    if (length(msgs))
        warning(
            "Samples that do not match across experiments:\n    ",
            paste(msgs, collapse = "\n    "), call. = FALSE
        )
    invisible(NULL)
}
