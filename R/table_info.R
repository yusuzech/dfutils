#' Capture formatted output of any variable, which can be used for logging
#'
#' @param x a R object
#'
#' @return A character vector with rows separated by \code{\\n}
#'
#' @examples
#' \dontrun{
#' cat(du_log_output(head(iris)))
#' cat(du_log_output(head(tibble::as_tibble(iris))))
#' cat(du_log_output(summary(lm(Sepal.Width ~ Sepal.Length,data = iris))))
#' }
#' @export
du_log_output <- function(x){
    paste0(capture.output(x),collapse = "\n")
}

