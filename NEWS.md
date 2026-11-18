# Changes in version 1.0.0

## NEW FEATURES

- `checkSex()` compares the reported sex in `colData` with the expression
  of sex-specific marker genes (XIST and Y-chromosome genes) and flags
  mismatched and ambiguous samples, such as male tumors that have lost
  chromosome Y expression.
- `checkGenome()` reports ranged experiments with different genome builds
  (e.g., hg19 and hg38), even when they share no seqlevels.

## SIGNIFICANT USER-VISIBLE CHANGES

- `clusterOn()` is removed; use `checkSex()` to check reported sex against
  marker gene expression.
- The seqinfo checkers test compatibility rather than identity, following
  the rules of `merge()` for `Seqinfo` objects:
  - `checkSeqlevels()` ignores seqlevel order and accepts experiments that
    cover a subset of the seqlevels; it fails when two experiments have no
    seqlevels in common. Use `exact = TRUE` to require the same set of
    seqlevels.
  - `checkSeqlengths()` compares lengths by seqlevel name, ignores unknown
    (`NA`) lengths, and lists each experiment's length for conflicting
    seqlevels.
  - `checkSeqlevelsStyle()` passes when the experiments have at least one
    naming style in common and skips experiments whose style cannot be
    determined.

## BUG FIXES AND MINOR IMPROVEMENTS

- The package loads again: the `seqlevelsStyle<-` and `genome<-` generics
  are now imported (from GenomeInfoDb and Seqinfo, respectively).
- Fix the `Imports` field in `DESCRIPTION` (previously misspelled `Import`)
  and declare all dependencies.
- Replace Biobase with the lighter BiocBaseUtils for `selectSome()`.
- Setters update experiments with `experiments<-` instead of the internal
  `BiocGenerics:::replaceSlots()`.
- Add a tinytest test suite based on `miniACC`.
