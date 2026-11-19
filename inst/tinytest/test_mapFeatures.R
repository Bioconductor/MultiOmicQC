source("helpers.R")

if (!requireNamespace("org.Hs.eg.db", quietly = TRUE))
    exit_file("org.Hs.eg.db not available")
orgdb <- org.Hs.eg.db::org.Hs.eg.db

## protein array with UniProt accessions as row names
rppa <- miniACC[["RPPAArray"]]
uniprot <- suppressMessages(AnnotationDbi::mapIds(orgdb, rownames(rppa),
    column = "UNIPROT", keytype = "SYMBOL"))
rppa <- rppa[!is.na(uniprot), ]
symbols <- rownames(rppa)
rownames(rppa) <- uniprot[!is.na(uniprot)]
withUniprot <- miniACC
withUniprot[["RPPAArray"]] <- rppa

## input validation
expect_error(mapFeatures(list(), "RPPAArray", orgdb), "MultiAssayExperiment")
expect_error(mapFeatures(withUniprot, "nope", orgdb), "'experiment'")
expect_error(mapFeatures(withUniprot, "RPPAArray", orgdb, keytype = 1),
    "'keytype'")
expect_error(mapFeatures(withUniprot, "RPPAArray", orgdb,
    stripIsoforms = NA), "'stripIsoforms'")

## maps back to the gene symbols and records the original accessions
expect_message(mapFeatures(withUniprot, "RPPAArray", orgdb), "Mapped")
mapped <- suppressMessages(mapFeatures(withUniprot, "RPPAArray", orgdb))
expect_inherits(mapped, "MultiAssayExperiment")
expect_identical(rownames(mapped[["RPPAArray"]]), symbols)
expect_identical(
    SummarizedExperiment::rowData(mapped[["RPPAArray"]])$UNIPROT,
    unname(rownames(rppa))
)
expect_identical(mapped[["RNASeq2GeneNorm"]], miniACC[["RNASeq2GeneNorm"]])
set.seed(1)
res <- checkFeatureCorrelation(mapped, c("RNASeq2GeneNorm", "RPPAArray"))
expect_identical(nrow(res), length(symbols))

## isoform suffixes are stripped; unmapped features are dropped
iso <- rppa
rownames(iso)[1] <- paste0(rownames(iso)[1], "-2")
rownames(iso)[2] <- "NOTANACCESSION"
isoMae <- withUniprot
isoMae[["RPPAArray"]] <- iso
expect_message(mapFeatures(isoMae, "RPPAArray", orgdb),
    "dropped 1 unmapped")
mapped <- suppressMessages(mapFeatures(isoMae, "RPPAArray", orgdb))
expect_identical(rownames(mapped[["RPPAArray"]]), symbols[-2])
mapped <- suppressMessages(
    mapFeatures(isoMae, "RPPAArray", orgdb, stripIsoforms = FALSE)
)
expect_identical(rownames(mapped[["RPPAArray"]]), symbols[-(1:2)])

## features mapping to the same gene: the most complete one is kept
dupe <- rppa[c(1, 1, 2:nrow(rppa)), ]
rownames(dupe)[2] <- paste0(rownames(dupe)[1], "-3")
SummarizedExperiment::assay(dupe)[1, 1:5] <- NA
dupeMae <- withUniprot
dupeMae[["RPPAArray"]] <- dupe
expect_message(mapFeatures(dupeMae, "RPPAArray", orgdb), "1 duplicated")
mapped <- suppressMessages(mapFeatures(dupeMae, "RPPAArray", orgdb))
expect_identical(rownames(mapped[["RPPAArray"]]), symbols)
expect_identical(
    SummarizedExperiment::rowData(mapped[["RPPAArray"]])$UNIPROT[1],
    rownames(dupe)[2]
)

## matrix experiments are supported
asMatrix <- withUniprot
asMatrix[["RPPAArray"]] <- SummarizedExperiment::assay(rppa)
mapped <- suppressMessages(mapFeatures(asMatrix, "RPPAArray", orgdb))
expect_identical(rownames(mapped[["RPPAArray"]]), symbols)
