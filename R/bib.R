

#' @title bibs in vcd package
#' 
#' @param key,... additional parameters of function \link[utils]{bibentry}
#' 
#' @keywords internal
#' @importFrom utils bibentry person
#' @name vcd_bib
#' @export
.cohen60 <- \(key = 'Cohen60', ...) {
  bibentry(
    bibtype = 'article', key = key, ...,
    author = person(given = 'Jacob', family = 'Cohen'),
    title = 'A Coefficient of Agreement for Nominal Scales',
    journal = 'Educational and Psychological Measurement',
    volume = '20',
    number = '1',
    pages = '37--46',
    year = '1960',
    doi = '10.1177/001316446002000104',
  )
}