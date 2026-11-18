#' @importFrom MultiAssayExperiment hasRowRanges experiments experiments<-
#'     sampleMap
#' @importFrom SummarizedExperiment rowRanges assay colData
#' @importFrom GenomeInfoDb seqlevelsStyle seqlevelsStyle<-
#' @importFrom Seqinfo seqinfo seqlevels seqlengths genome genome<-
#' @importFrom stats median sd
#' @importFrom S4Vectors DataFrame metadata metadata<-
#' @importFrom methods is
#' @importFrom BiocBaseUtils selectSome isTRUEorFALSE isScalarCharacter
#' @importFrom utils combn
NULL

#' MultiOmicQC: Helper functions for checking the integrity of Multi-omics
#' datasets
#'
#' MultiOmicQC allows the user to run common checks on MultiAssayExperiment
#' objects additional to the checks already established in
#' MultiAssayExperiment.
#'
#' @aliases NULL
"_PACKAGE"
