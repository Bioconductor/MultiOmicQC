# Changes in version 1.0.0

## BUG FIXES AND MINOR IMPROVEMENTS

- The package loads again: the `seqlevelsStyle<-` and `genome<-` generics
  are now imported (from GenomeInfoDb and Seqinfo, respectively).
- `clusterOn()` uses `longForm()`, replacing the defunct `longFormat()`, and
  reports missing features with an informative error.
- Fix the `Imports` field in `DESCRIPTION` (previously misspelled `Import`)
  and declare all dependencies.
- Replace Biobase with the lighter BiocBaseUtils for `selectSome()`.
- Setters update experiments with `experiments<-` instead of the internal
  `BiocGenerics:::replaceSlots()`.
- Add a tinytest test suite based on `miniACC`.
