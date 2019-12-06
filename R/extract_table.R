#' get rle as a named vector
#'
#' @param v a vector
#' @return rle but with values as vector value and lengths as vector name
vec_rle <- function(v){
    temp <- rle(v)
    out <- temp$values
    names(out) <- temp$lengths
    return(out)
}


#' returns the consecutive row/column numbers that are not NA in a list
#' @description Only accept a boolean vector(only TRUE or FALSE). Otherwise, this
#' function doesn't make any sense.
#'
#' @param v a boolean vector(only TRUE/FALSE)
#'
#' @return A list of consecutive row/column numbers as different elements
make_df_index <- function(v){
    # doesn't make sense if v is not a logical vector or contains NA
    stopifnot(is.logical(v))
    stopifnot(any(is.na(v)))
    table_rle <- vec_rle(v)
    divide_points <- c(0,cumsum(names(table_rle)))
    table_index <- purrr::map2((divide_points + 1)[1:length(divide_points)-1],
                        divide_points[2:length(divide_points)],
                        ~.x:.y)
    return(table_index[table_rle])
}


#' split a large table in one direction if there are blank columns or rows
#'
#' @param df a data frame
#' @param direction "col" or "row", the direction to split the data frame
#' @param split_index whether to return the index where the split is made
#' @return
#' If split_index is \code{TRUE}, returns a list of data frame.
#' If split_index is \code{FALSE}, returns a list of two elements, the first element
#' \code{df} contains a list of split data frames, the second element \code{split_index}
#' is the index where the split is performed.
#' @export
du_split_direction <- function(df,direction = "col",split_index = FALSE){
    if(direction == "col"){
        direct_has_data <- unname(map_lgl(df,~!all(is.na(.x))))
        df_mapping <- make_df_index(direct_has_data)
        out <- map(df_mapping,~df[,.x])
    } else if(direction == "row"){
        direct_has_data <- df %>%
            dplyr::mutate_all(~!is.na(.x)) %>%
            purrr::pmap_lgl(any)
        df_mapping <- make_df_index(direct_has_data)
        out <- purrr::map(df_mapping,~df[.x,])
    }

    if (split_index) {
        return(
            list(
                df = out,
                split_index = setdiff(
                    1:length(direct_has_data),
                    purrr::reduce(df_mapping,c)
                )
            )
        )
    } else {
        return(out)
    }
}



#' split a large table into smaller tables if there are blank columns or rows
#'
#' @param df a data frame
#' @param complexity default to 1,number of times to split a data frame by row and column.
#' If number increase by one, one more split(by both row and column) will be performed
#' @details
#'
#' 1. if you still see entire rows or columns missing. Please increase complexity
#' 2. The list of data frames is roughly ordered from left to right and top to
#' bottom in the original data frame
#'
#' @return a list of data frames
#'
#' @examples
#' mydf <- as.data.frame(matrix(1:100,nrow = 10))
#' mydf[5,] <- NA
#' mydf[,7] <- NA
#' mydf[,8 ] <- NA
#' mydf[1,c(1,2,3)] <- NA
#' du_split_df(mydf)
#'
#' @export
du_split_df <- function(df,complexity = 1){
    out <- list(df)
    for(i in 1 :complexity){
        for (direction in c("row","col")){
            split_result <- out %>%
                map(~du_split_direction(.x,direction))
            out <- flatten(split_result)
        }
    }
    return(out)
}


#' display cells in a data frame that doesn't contain missing value
#' @param df a data frame
#'
#' @return a graph that display the shape of data
#'
#' @export
du_display_data_shape <- function(df){
    colnames(df) <- 1:ncol(df)
    df %>%
        map_df(~as.numeric(!is.na(.x))) %>%
        gather(key = "x",value = "value") %>%
        mutate(x = as.numeric(x)) %>%
        group_by(x) %>%
        mutate(y = -row_number()) %>%
        ungroup() %>%
        filter(value == 1) %>%
        ggplot(aes(x = x, y = y,fill = value)) +
        geom_tile(fill = "skyblue3") +
        scale_x_continuous(position = "top") +
        geom_rect(
            aes(xmin = 0,xmax = dim(df)[1],ymin = -dim(df)[2],ymax = 0),
            position = position_nudge(x = 0.5, y = -0.5),
            alpha = 0,
            color = "black",
            size = 1
        ) +
        coord_fixed() +
        theme_void() +
        guides(fill = FALSE)
}
