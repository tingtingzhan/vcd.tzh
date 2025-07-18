
#' @title Kappa with input confusion matrix
#' 
#' @param x see function \link[vcd]{Kappa}
#' 
#' @param ... additional parameters of function \link[vcd]{Kappa}
#' 
#' @details
#' Function \link[vcd]{Kappa} does not keep any info of the input (eh..)
#' 
#' @examples
#' data('JobSatisfaction', package = 'vcd')
#' (job1 = xtabs(Freq ~ supervisor + own, data = JobSatisfaction))
#' (job2 = xtabs(Freq ~ management + own, data = JobSatisfaction))
#' 
#' library(rmd.tzh)
#' 
#' list(
#'   'supervisor vs. own' = job1 |> vcd::Kappa(),
#'   'management vs. own' = job2 |> vcd::Kappa()
#' ) |> render_(file = 'Kappa')
#' 
#' list(
#'   'supervisor vs. own' = job1 |> Kappa2(),
#'   'management vs. own' = job2 |> Kappa2()
#' ) |> render_(file = 'Kappa2')
#' 
#' list(
#'   'sec 1' = list(
#'     'abc',
#'     job1 |> Kappa2()
#'   )
#' ) |> render_(file = 'Kappa2_v2')
#' 
#' @keywords internal
#' @importFrom vcd Kappa
#' @export
Kappa2 <- function(x, ...) {
  ret <- Kappa(x, ...)
  attr(ret, which = 'x') <- x
  class(ret) <- c('Kappa2', class(ret)) |> unique.default()
  return(ret)
}



#' @title R Markdown Lines for \link[vcd]{Kappa} or [Kappa2]
#' 
#' @param x \link[vcd]{Kappa} or [Kappa2] object
#' 
#' @param xnm ..
#' 
#' @param ... ..
#' 
#' @keywords internal
#' @importFrom rmd.tzh md_
#' @export md_.Kappa
#' @export 
md_.Kappa <- function(x, xnm, ...) {

  return(list(
    
    Sprintf.Kappa(x), 
    
    if (inherits(x, what = 'Kappa2')) {
      c(
        '```{r}',
        sprintf(fmt = '%s |> attr(which = \'x\', exact = TRUE) |> as_flextable(include.row_percent = FALSE, include.column_percent = FALSE, include.table_percent = FALSE)', xnm),
        '```', 
        '<any-text>'
      )
    }, # else NULL
    
    '\n\n' # would never hurt !!
    
  ))
  
}




#' @title cut.Kappa
#' 
#' @param x a \link[vcd]{Kappa} object
#' 
#' @param ... trivial parameters of function \link[base]{cut.default}
#' 
#' @note
#' Hard-coded cut-off values from \url{https://en.wikipedia.org/wiki/Cohen%27s_kappa#Interpreting_magnitude}.
#' 
#' @keywords internal
#' @export cut.Kappa
#' @export
cut.Kappa <- function(x, ...) {
  x |>
    coef.Kappa() |>
    cut.default(
      breaks = c(-Inf, 0, .2, .4, .6, .8, 1), 
      labels = c('no ($\\kappa<0$)', 'slight ($0\\leq\\kappa\\leq.2$)', 'fair ($.2<\\kappa\\leq.4$)', 'moderate ($.4<\\kappa\\leq.6$)', 'substantial ($.6<\\kappa\\leq.8$)', 'almost perfect ($.8<\\kappa\\leq 1$)'), 
      right = TRUE, include.lowest = TRUE,
      ...
    )
}


#' @importFrom stats coef
#' @export
coef.Kappa <- function(object, ...) {
  # funny way of returned object (eh..)
  #object[c('Unweighted', 'Weighted')] |>
  object[c('Weighted')] |>
    vapply(FUN = `[`, 'value', FUN.VALUE = NA_real_)
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
  #x[c('Unweighted', 'Weighted')] |>
  x[c('Weighted')] |>
    vapply(FUN = \(i) {
      2 * pnorm(abs(i[1L]/i[2L]), lower.tail = FALSE)
    }, FUN.VALUE = NA_real_)
}


#' @rdname S3_Kappa
#' @export
endpoint.Kappa <- function(x) quote(Agreement)







#' @rdname S3_Kappa
#' @importFrom utils bibentry
#' @importFrom stats confint
#' @export
Sprintf.Kappa <- function(x) {
  
  ci <- confint(x)['Weighted', ] # ?vcd:::confint.Kappa
  
  ret <- sprintf(
    fmt = '[Cohen\'s $\\kappa$ coefficient of agreement](https://en.wikipedia.org/wiki/Cohen%%27s_kappa) [@Cohen60] $\\kappa=%.2f$, 95%% confidence interval (%.2f, %.2f), reflecting a %s agreement, is provided by <u>**`R`**</u> package <u>**`vcd`**</u>.',
    x |> coef.Kappa(),
    ci[1L], ci[2L],
    x |> cut.Kappa() |> as.character()
  )
  attr(ret, which = 'bibentry') <- bibentry(
    bibtype = 'article', 
    key = 'Cohen60',
    author = 'Jacob Cohen',
    title = 'A Coefficient of Agreement for Nominal Scales',
    journal = 'Educational and Psychological Measurement',
    volume = '20',
    number = '1',
    pages = '37--46',
    year = '1960',
    doi = '10.1177/001316446002000104',
  )
  return(ret)
}



