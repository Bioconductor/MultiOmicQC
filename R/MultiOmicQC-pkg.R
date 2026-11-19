#' @importFrom MultiAssayExperiment hasRowRanges experiments experiments<-
#'     sampleMap intersectColumns intersectRows anyReplicated
#'     MultiAssayExperiment
#' @importFrom SummarizedExperiment rowRanges assay colData rowData rowData<-
#' @importFrom AnnotationDbi select
#' @importFrom GenomeInfoDb seqlevelsStyle seqlevelsStyle<-
#' @importFrom Seqinfo seqinfo seqlevels seqlengths genome genome<-
#' @importFrom stats median sd cor p.adjust quantile
#' @importFrom MatrixGenerics rowRanks
#' @importFrom S4Vectors DataFrame metadata metadata<-
#' @importFrom methods is
#' @importFrom BiocBaseUtils selectSome isTRUEorFALSE isScalarCharacter
#'     isScalarNumber
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
