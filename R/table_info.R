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
    paste0(
        paste0(capture.output(x),collapse = "\n"),
        "\n"
    )
}

#' Add margins considering all numeric values in a data frame
#'
#' @description
#' Adding margins to column, row or both, non-numeric columns are ignored.Use
#' argument \code{ingnoreCols} to skip numeric columns.
#'
#' @param df data frame to add margin to.
#' @param margin valid values are "column", "row", "both". Append margins after
#' column, row or both.
#' @param verbose show or hide messages
#' @param ingnoreCols numeric columns to ignore when adding margin,
#' can use column names or column indexes.
#' @param enable_total_margin default to \code{TRUE}, whether to calcualte total
#' margin when \code{margin = "both"}
#' @param direction_total_margin, default to "column", direction for calculating
#' total margin, please read more in details.
#' @param  fun.aggregate function used to calculate margin, default to sum,
#' can be one of min, max, median, sd, sum, mean or any other custom
#' functions which can reduce a column/row
#' automatically enabled.
#' @param ... other arguments passed to  aggregate function
#' @return A data frame with extra an extra
#' column/row as the total for that column/row
#'
#' @details
#'
#' when \code{direction} is set to "both", total margin on bottom right of the
#' data frame may not make sense when using mean, sd or similar functions as
#' aggregating fucntion.
#'
#' To  specify direction of calculation:
#' \code{direction_total_margin = "column"} to calculate by columns(horizontally),
#' \code{direction_total_margin = "row"} to calculate by rows(vertically)
#'
#' @examples
#' du_add_margin(iris)
#' du_add_margin(iris,verbose = FALSE)
#' du_add_margin(iris,fun.aggregate=mean,na.rm = TRUE)
#' # disable calculation for total margin
#' du_add_margin(
#'   iris,
#'   enable_total_margin = FALSE,
#'   fun.aggregate=sd,
#'   na.rm = TRUE
#' )
#' @export
du_add_margin <- function(df,
                          margin = "both",
                          verbose = TRUE,
                          ignore_columns = NULL,
                          enable_total_margin = TRUE,
                          direction_total_margin = "column",
                          fun.aggregate = sum,
                          ...){
    if(!is.function(fun.aggregate)){
        stop("fun.aggregate must be a function.")
    }
    margin_col_name <- deparse(substitute(fun.aggregate))
    if (margin_col_name %in% colnames(df)) {
        margin_col_name <- paste0(margin_col_name,"_total")
    }
    user_fun <- purrr::partial({{fun.aggregate}},...)

    # get numeric df used to calculate margin
    add_margin_cols <- colnames(df)[purrr::map_lgl(df,is.numeric)]
    add_margin_cols <- add_margin_cols[!(add_margin_cols %in% ignore_columns)]
    column_index <- which(colnames(df) %in% add_margin_cols)
    selected_df <- dplyr::select_at(df,add_margin_cols)

    col_margin <- purrr::pmap_dbl(unname(as.list(selected_df)),function(...){
        input = unlist(list(...))
        user_fun(input)
    })

    row_margin <- purrr::map_dbl(unname(as.list(selected_df)),user_fun)

    # calculated margins
    if (margin == "column") {
        df[margin_col_name] <- col_margin
        if(verbose) message(paste(margin_col_name," appended.\n"))

    } else if (margin == "row") {
        full_columns <- 1:ncol(df)
        full_columns[which(!(full_columns %in% column_index))] <- NA
        full_columns[!is.na(full_columns)] <- row_margin
        # adding new row
        df[nrow(df)+1,] <- NA
        for (i in 1:ncol(df)){
            df[nrow(df),i] <- full_columns[i]
        }
        if(verbose) message(paste("row ", nrow(df)," appended.\n"))

    } else if (margin == "both") {
        # append col margin
        df[margin_col_name] <- col_margin
        # append row margin
        full_columns <- 1:ncol(df)
        full_columns[which(!(full_columns %in% column_index))] <- NA
        full_columns[!is.na(full_columns)] <- row_margin
        # adding new row
        df[nrow(df)+1,] <- NA
        for (i in 1:ncol(df)){
            df[nrow(df),i] <- full_columns[i]
        }

        #calculate total margin
        if(enable_total_margin){
            if (direction_total_margin == "row") {
                total_margin <- user_fun(col_margin)
            } else if (direction_total_margin == "column") {
                total_margin <- user_fun(row_margin)
            } else {
                stop(direction_total_margin,
                     " is not a valid direction. Use 'row' or 'column' instead")
            }
            df[nrow(df),ncol(df)] = total_margin
        }
        if(verbose) message(paste("row ", nrow(df)," appended."))
        if(verbose) message(paste("column", margin_col_name," appended."))

    } else {
        stop("Not a valid margin. Choose one of 'column', 'row' or 'both'")
    }

    if(verbose) message(paste("Columns used:\n",
                              paste0(add_margin_cols,collapse = ", ")))


    return(df)

}

