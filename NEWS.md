## miclust 1.3.0

### Minor changes:

* Dependences:
  - The package is now completely independent of the `irr` package, which was previously used to internally calculate Cohen's Kappa (`irr::kappa2`). Now, Cohen's Kappa is calculated using a native base implementation to improve stability and maintainability. In addition, the Cohen's Kappa calculation is now faster.

* Documentation:
  - Updated source code documentation to comply with the latest `roxygen2` (v8.0.0) standards.
  - Improved `README.Rmd` plot rendering.
  - Integrated documentation for `miclust()` and its related methods `print.miclust()` and `plot.miclust()` into a single help page for a more fluid usage.
  - Integrated documentation for methods `summary.miclust()` and `print.summary.miclust()`. 

* `miclust`:
  - The width of the verbose message has been reduced (now, breaks line each 20 imputations).

* `print.miclust`:
  - Fix erratum in labels of the output of `print.miclust`.

* `plot.miclust`:
  - New parameter `metric`, to select which metric is displayed. Defaults to `"all"`, maintaining compatibility with the output from versions 1.2.8 and earlier.  Other options are: `"nclfreq"` (percentage of times each number of clusters has been selected); `"critcf"` (critCF distribution for each number of clusters); `"nvarfreq"` (distribution of the number of selected variables), and `"varsel"` (percentage of appearance of the variables that remained in the final set of selected variables).
  - New parameters `col.nclfreq`, `col.critcf`, `col.nvarfreq`, and `col.varsel`, to customize the color used in the plots for metrics, `"nclfreq"`, `"critcf"`, `"nvarfreq"`, and `"varsel"`, respectively. Default values are consistent with results under versions 1.2.8 and earlier.
  - New parameter `col.all`. When specified, it overrides `col.nclfreq`, `col.critcf`, `col.nvarfreq`, and `col.varsel`, and applies that color across the entire panel. Default (`NULL`) is consistent with results under versions 1.2.8 and earlier.

* `summary.miclust`:
  - Fix output errors related to within-cluster missingness of the selected variables.
  - Fix an internal error in `summary.miclust()` (originally triggered during `kcca()` execution inside `assignprobandkappas()`) that occurred when a specific imputation or the final assigned clustering result contained an empty cluster. Specifically, add a validation check in `summary.miclust()` to detect empty clusters across all imputations and the assigned cluster for the given `k`. The function now prints the affected cases and halts execution with a detailed error message.
  - An internal error in `summary.miclust()` (originally triggered during `kcca()` execution inside `assignprobandkappas()`) occurs when the final clustering for a given `k` and the set of selected variables results in an empty cluster within specific imputed datasets. This indicates that such a combination of `k` and selected variables is unable to find as many clusters as requested. A validation check has been added in `summary.miclust()` to detect these scenarios and halt execution with a detailed error message.

* `print.summary.miclust`:
  - The `Within-cluster summary` section of the output has changed. To reduce the output width, the columns for missingness, mean, and standard deviation have been collapsed into a single column. The section label has been changed to `Within-cluster summary [%miss.;mean;sd]`. Separated columns (version <= 1.2.8 style) are still available in the `summarybycluster` element of the object.


<hr style="border:2px solid gray"> </hr>


## miclust 1.2.8

### Minor changes:

* Update deprecated code in `CITATION`.

<hr style="border:2px solid gray"> </hr>

## miclust 1.2.7

### Minor changes:

* Fix two missed figures in `README`.

<hr style="border:2px solid gray"> </hr>

## miclust 1.2.6

* First version on R CRAN

<hr style="border:2px solid gray"> </hr>
