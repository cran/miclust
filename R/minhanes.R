#' Multiple imputation for nhanes data.
#'
#' @description A list with 101 data sets. The first data set contains \code{nhanes}
#'   data from \code{mice} package. The remaining data sets were obtained by applying
#'   the multiple imputation function \code{mice} from package \code{mice}.
#'
#' @format A list of 101 data.frames each of them with 25 observations of the following
#'   4 variables:
#' \describe{
#' \item{age}{age group (1 = 20-39, 2 = 40-59, 3 = 60+). Treated as numerical.}
#' \item{bmi}{body mass index (kg/m\eqn{^2})}
#' \item{hyp}{hypertensive (1 = no, 2 = yes). Treated as numerical.}
#' \item{chl}{total serum cholesterol (mg/dL)}
#' }
#'
#' @source \url{https://CRAN.R-project.org/package=mice}
#'
#' @examples
#' data(minhanes)
#' ### raw data:
#' minhanes[[1]]
#' summary(minhanes[[1]])
#'
#' ### number of imputed data sets:
#' length(minhanes) - 1
#'
#' ### first imputed data set:
#' minhanes[[2]]
#' summary(minhanes[[2]])
"minhanes"
