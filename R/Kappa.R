

# ?vcd::Kappa does not keep any info of the input (eh..)



#' @title cut.Kappa
#' 
#' @param x a \link[vcd]{Kappa} object
#' 
#' @param breaks,labels,right,include.lowest,... parameters of function \link[base]{cut.default}
#' 
#' @examples
#' library(vcd)
#' (job1 = xtabs(Freq ~ supervisor + own, data = JobSatisfaction))
#' (job2 = xtabs(Freq ~ management + own, data = JobSatisfaction))
#' job1 |> Kappa() |> cut()
#' job2 |> Kappa() |> cut()
#' @keywords internal
#' @export cut.Kappa
#' @export
cut.Kappa <- function(
    x, 
    breaks = c(-Inf, 0, .2, .4, .6, .8, 1),
    labels = c('no', 'slight', 'fair', 'moderate', 'substantial', 'almost perfect'),
    right = TRUE, include.lowest = TRUE,
    ...
) {
  cut.default(
    x = coef.Kappa(x),
    breaks = breaks, labels = labels, 
    right = right, include.lowest = include.lowest,
    ...
  )
}


#' @importFrom stats coef
#' @export
coef.Kappa <- function(object, ...) {
  # packageDate('vcd') 2024-09-16
  # funny way of returned object (eh..)
  vapply(object[c('Unweighted', 'Weighted')], FUN = `[`, 'value', FUN.VALUE = NA_real_)
}


#' @importFrom stats nobs
#' @export
nobs.Kappa <- function(object, ...) NA_integer_ # impossible to retrieve..




#' @title Additional S3 Methods for \link[vcd]{Kappa}
#' 
#' @param x a \link[vcd]{Kappa} object
#' 
#' @keywords internal
#' @name S3_Kappa
#' @importFrom stats pnorm
#' @export
.pval.Kappa <- function(x) {
  # ?vcd:::print.Kappa; eh..
  # ?vcd:::summary.Kappa; this author is crazy..
  vapply(x[c('Unweighted', 'Weighted')], FUN = function(i) {
    2 * pnorm(abs(i[1L]/i[2L]), lower.tail = FALSE)
  }, FUN.VALUE = NA_real_)
}


#' @rdname S3_Kappa
#' @export
endpoint.Kappa <- function(x) quote(Agreement)







#' @rdname S3_Kappa
#' @export
Sprintf.Kappa <- function(x) {
  'Cohen\'s $\\kappa$ coefficient of agreement is provided by <u>**`R`**</u> package <u>**`vcd`**</u>.'
}



