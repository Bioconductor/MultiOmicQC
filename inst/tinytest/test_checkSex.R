source("helpers.R")

samples <- colnames(miniACC)[["RNASeq2GeneNorm"]]
smap <- sampleMap(miniACC)
primary <- smap$primary[match(samples, smap$colname)]
gender <- miniACC$gender[match(primary, rownames(colData(miniACC)))]

## input validation
mae <- addSexGenes()
expect_error(checkSex(list(), "sexGenes"), "MultiAssayExperiment")
expect_error(checkSex(mae, "nope", sex = "gender"), "'experiment'")
expect_error(checkSex(mae, c("sexGenes", "gistict"), sex = "gender"),
    "'experiment'")
expect_error(checkSex(mae, "sexGenes", sex = "nope"), "'sex'")
expect_error(checkSex(mae, "sexGenes", sex = "gender", log = NA), "'log'")
expect_error(
    checkSex(mae, "sexGenes", sex = "gender", maleGenes = "NOTAGENE"),
    "at least one gene"
)
expect_error(
    checkSex(miniACC, "RNASeq2GeneNorm", sex = "gender"), "at least one gene"
)
negative <- addSexGenes(data = log2(makeSexGenes() + 1) - 5)
expect_error(checkSex(negative, "sexGenes", sex = "gender"), "log = FALSE")

## all reported sexes agree with expression: no warning, all match
res <- checkSex(mae, "sexGenes", sex = "gender")
expect_silent(checkSex(mae, "sexGenes", sex = "gender"))
expect_inherits(res, "DataFrame")
expect_identical(nrow(res), length(samples))
expect_identical(res$colname, samples)
expect_identical(res$primary, primary)
expect_identical(res$reported, gender)
expect_identical(res$predicted, gender)
expect_true(all(res$status == "match"))
expect_identical(metadata(res)$femaleGenes, "XIST")
expect_identical(metadata(res)$maleGenes, c("RPS4Y1", "DDX3Y", "KDM5D"))
expect_identical(names(metadata(res)$thresholds), c("female", "male"))

## swapped samples are flagged as mismatches
truth <- gender
swapped <- samples[c(3, 10, 20)]
truth[c(3, 10, 20)] <- ifelse(truth[c(3, 10, 20)] == "female",
    "male", "female")
mae <- addSexGenes(sexes = truth)
expect_warning(checkSex(mae, "sexGenes", sex = "gender"), "mismatch \\(3\\)")
res <- suppressWarnings(checkSex(mae, "sexGenes", sex = "gender"))
expect_identical(res$colname[res$status == "mismatch"], swapped)
expect_identical(res$predicted, truth)

## loss of Y expression in a male is ambiguous, not a mismatch
male1 <- samples[gender == "male"][1L]
mae <- addSexGenes(lossOfY = male1)
res <- suppressWarnings(checkSex(mae, "sexGenes", sex = "gender"))
expect_identical(res$colname[res$status != "match"], male1)
expect_identical(res$status[res$colname == male1], "ambiguous")
expect_true(is.na(res$predicted[res$colname == male1]))
expect_warning(checkSex(mae, "sexGenes", sex = "gender"), "ambiguous \\(1\\)")

## a single reported male among females is enough to calibrate
keep <- rownames(colData(miniACC)) %in%
    c(primary[gender == "female"], primary[gender == "male"][2L])
oneMale <- suppressWarnings(addSexGenes()[, keep, ])
res <- checkSex(oneMale, "sexGenes", sex = "gender")
expect_identical(sum(res$reported == "male"), 1L)
expect_true(all(res$status == "match"))

## common codings of sex are recognized; others are treated as missing
coded <- addSexGenes()
coded$gender <- ifelse(coded$gender == "female", "F", "Male")
res <- checkSex(coded, "sexGenes", sex = "gender")
expect_true(all(res$status == "match"))
coded$gender[1:3] <- c("unknown", NA, "U")
expect_warning(checkSex(coded, "sexGenes", sex = "gender"), "unknown, U")
res <- suppressWarnings(checkSex(coded, "sexGenes", sex = "gender"))
unreported <- rownames(colData(coded))[1:3]
expect_true(all(res$status[res$primary %in% unreported] == "unreported"))
expect_true(all(res$status[!res$primary %in% unreported] == "match"))

## a single reported sex cannot be checked
females <- suppressWarnings(addSexGenes()[, miniACC$gender == "female", ])
expect_warning(checkSex(females, "sexGenes", sex = "gender"),
    "Both sexes must be reported")
res <- suppressWarnings(checkSex(females, "sexGenes", sex = "gender"))
expect_true(all(res$status == "undetermined"))
expect_true(all(is.na(res$predicted)))

## markers that do not separate the reported groups give no predictions
inverted <- addSexGenes(sexes = ifelse(gender == "female", "male", "female"))
expect_warning(checkSex(inverted, "sexGenes", sex = "gender"),
    "does not separate")
res <- suppressWarnings(checkSex(inverted, "sexGenes", sex = "gender"))
expect_true(all(res$status == "undetermined"))

## custom marker genes and log = FALSE
logged <- addSexGenes(data = log2(makeSexGenes() + 1))
res <- checkSex(logged, "sexGenes", sex = "gender", log = FALSE,
    maleGenes = "RPS4Y1")
expect_true(all(res$status == "match"))
expect_identical(metadata(res)$maleGenes, "RPS4Y1")

## constant marker genes are skipped
flat <- makeSexGenes()
flat["DDX3Y", ] <- 5
flat <- addSexGenes(data = flat)
res <- checkSex(flat, "sexGenes", sex = "gender")
expect_true(all(res$status == "match"))

## SummarizedExperiment input uses the first assay
se <- SummarizedExperiment::SummarizedExperiment(
    list(counts = makeSexGenes(), other = -makeSexGenes())
)
seMae <- addSexGenes(data = se)
res <- checkSex(seMae, "sexGenes", sex = "gender")
expect_true(all(res$status == "match"))
