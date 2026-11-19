#' Map the feature identifiers of an experiment
#'
#' Replaces the row names of one experiment, such as UniProt protein
#' accessions, with identifiers of another type, such as gene symbols,
#' using an annotation resource that supports
#' \code{\link[AnnotationDbi]{select}}. This lets the features of a
#' proteomics experiment be matched with those of a gene expression
#' experiment in \code{\link{checkFeatureCorrelation}} and
#' \code{\link{checkSampleIdentity}}.
#'
#' @details
#' \code{db} can be an \code{OrgDb} package such as \code{org.Hs.eg.db}
#' (offline; e.g., \code{keytype = "UNIPROT"}, \code{column = "SYMBOL"}) or
#' a \code{UniProt.ws} object (queries the UniProt web service for any
#' species; e.g., \code{keytype = "UniProtKB"},
#' \code{column = "gene_primary"}). See \code{keytypes(db)} and
#' \code{columns(db)} for the available identifier types. The keys are
#' taken from the first column of the \code{select()} result and the mapped
#' identifiers from the last.
#'
#' When a key maps to several identifiers, the first is used. Features
#' that do not map are dropped. When several features map to the same
#' identifier (e.g., protein isoforms of one gene), the feature with the
#' fewest missing values is kept. For a \code{SummarizedExperiment}, the
#' original identifiers are kept in a \code{rowData} column named after
#' \code{keytype}. A message reports how many features were mapped and
#' dropped.
#'
#' @param x A \linkS4class{MultiAssayExperiment}
#'
#' @param experiment \code{character(1)} The name of the experiment whose
#' row names are mapped.
#'
#' @param db An annotation resource with a \code{select()} method, such as
#' an \code{OrgDb} or a \code{UniProt.ws} object.
#'
#' @param keytype \code{character(1)} The type of the current row names, one
#' of \code{keytypes(db)} (default \code{"UNIPROT"}).
#'
#' @param column \code{character(1)} The type of the new row names, one of
#' \code{columns(db)} (default \code{"SYMBOL"}).
#'
#' @param stripIsoforms \code{logical(1)} Whether to remove UniProt isoform
#' suffixes (e.g., \code{"P04637-2"} to \code{"P04637"}) before mapping
#' (default \code{TRUE}). \code{OrgDb} packages do not include isoform
#' accessions.
#'
#' @return \code{x} with the row names of \code{experiment} mapped
#'
#' @examples
#' if (requireNamespace("org.Hs.eg.db", quietly = TRUE)) {
#'     data(miniACC, package = "MultiAssayExperiment")
#'     ## give the protein array UniProt accessions as row names
#'     rppa <- miniACC[["RPPAArray"]]
#'     rownames(rppa) <- AnnotationDbi::mapIds(org.Hs.eg.db::org.Hs.eg.db,
#'         rownames(rppa), column = "UNIPROT", keytype = "SYMBOL")
#'     miniACC[["RPPAArray"]] <- rppa[!is.na(rownames(rppa)), ]
#'     head(rownames(miniACC[["RPPAArray"]]))
#'
#'     ## map them back to gene symbols
#'     mapped <- mapFeatures(miniACC, "RPPAArray", org.Hs.eg.db::org.Hs.eg.db)
#'     head(rownames(mapped[["RPPAArray"]]))
#' }
#'
#' \donttest{
#' ## with the UniProt web service (requires an internet connection)
#' if (requireNamespace("UniProt.ws", quietly = TRUE) &&
#'     requireNamespace("org.Hs.eg.db", quietly = TRUE)) {
#'     up <- UniProt.ws::UniProt.ws(taxId = 9606)
#'     mapped <- mapFeatures(miniACC, "RPPAArray", up, keytype = "UniProtKB",
#'         column = "gene_primary")
#'     head(rownames(mapped[["RPPAArray"]]))
#' }
#' }
#'
#' @export
mapFeatures <- function(x, experiment, db, keytype = "UNIPROT",
    column = "SYMBOL", stripIsoforms = TRUE) {
    if (!is(x, "MultiAssayExperiment"))
        stop("Provide a 'MultiAssayExperiment' object")
    if (!isScalarCharacter(experiment) || !experiment %in% names(x))
        stop("'experiment' must be the name of one experiment in 'x'")
    if (!isScalarCharacter(keytype) || !isScalarCharacter(column))
        stop("'keytype' and 'column' must be single strings")
    if (!isTRUEorFALSE(stripIsoforms))
        stop("'stripIsoforms' must be TRUE or FALSE")

    expt <- experiments(x)[[experiment]]
    ids <- rownames(expt)
    if (is.null(ids))
        stop("'experiment' has no row names to map")
    keys <- if (stripIsoforms) sub("-[0-9]+$", "", ids) else ids
    uniqueKeys <- unique(keys[!is.na(keys)])
    ## 1:many mappings are resolved below, so select()'s notice is dropped
    map <- suppressMessages(
        select(db, keys = uniqueKeys, columns = column, keytype = keytype)
    )
    from <- as.character(map[[1L]])
    to <- as.character(map[[ncol(map)]])
    ok <- !is.na(to) & nzchar(to) & !duplicated(from)
    newIds <- to[ok][match(keys, from[ok])]

    ## among features mapping to the same identifier, keep the most complete
    mat <- .expressionMatrix(expt)
    ord <- order(rowSums(is.na(mat)))
    keep <- logical(length(ids))
    keep[ord] <- !is.na(newIds[ord]) & !duplicated(newIds[ord])
    unmapped <- sum(is.na(newIds))
    message(
        "Mapped ", sum(keep), " of ", length(ids), " features in '",
        experiment, "'; dropped ", unmapped, " unmapped and ",
        length(ids) - unmapped - sum(keep), " duplicated"
    )

    expt <- expt[keep, , drop = FALSE]
    if (is(expt, "SummarizedExperiment"))
        rowData(expt)[[keytype]] <- ids[keep]
    rownames(expt) <- newIds[keep]
    x[[experiment]] <- expt
    x
}
