#' A warpper on \code{googleway::google_geocode()}: get geocode from google safely
#'
#' @param address address, vector length of one
#' @param apikey google's apikey
#'
#' @return a list of google's geocode service result
safely_get_geocode <- function(address,apikey){
    safe_func <- purrr::safely(function(){
        response <- googleway::google_geocode(address,key = apikey,simplify = TRUE)
        coords <- response[["results"]][["geometry"]][["location"]]
        return(
            list(
                response = response,
                coords = coords
            )
        )
    })
    return(safe_func())
}

#' get geocode with error handling built in,reasons are given for failed items
#'
#' @description getting geocode can be tedious sometimes since sometimes we get
#' errors, sometimes we multiple matched results. And doing the error/exception
#' handling has always been a headache. This functions tries do the error handling
#' for your api call won't be interrupted by any errors and you receive clean
#' results.
#'
#' Note that since this function won't be interrupted, please always test in smaller
#' scale to make sure everything is working as intended.
#'
#' @param address a character vector that contains address(es)
#' @param apikey google's apikey for using geocode service
#' @param result_type how should the results be returned, see more in details
#' @param multi_coord_handling how to handle results with multiple matching coordinates
#' @param progress whether to show a progress bar
#' @param show_summary whether to show a summary on job status
#'
#' @return a list of two elements \code{result} and \code{error}, return details
#' are dependent on \code{result_type} argument.
#'
#' @details
#'
#' \strong{result_type:}
#'
#' This function returns a list of two parts, \code{result} and \code{error}, and
#' result_type determines how \code{result} is structured. If a item fails or has
#' been processed by this function internally, a comment will show. Otherwise,
#' comment will be NA.
#'
#' \code{result}
#'
#' \describe{
#'     \item{list}{returns a list of 3 elements, lat,lng and comment}
#'     \item{df}{returns a data frame with 3 columns, lat, lng and comment}
#'     \item{raw}{returns the raw result from google's api as a list. Don't show
#'     any comments and this function don't process any data
#'     (\code{multi_coord_handling} won't be applied),you receive everything
#'     provided by geocode api}
#' }
#'
#' \code{error}
#'
#' a list that shows the error or special treatment for items in \code{result}
#'
#' \strong{multiple_coord_handling}
#'
#' Sometimes an address can be confusing to google's geocode API and multiple
#' matches are returned. For data analysis, It is undesirable in most cases. So
#' this functions tries handle this situation automatically. Regardless how this
#' functions handles this scenario, one entry is generated in \code{error}.
#'
#' \describe{
#'     \item{none}{no hanlding, the coordinates will be NA}
#'     \item{first}{use the first coordinates provided}
#'     \item{average}{average all coordinates and use it as result}
#' }
#'
#' \strong{show_summary}
#'
#' summary will be printed to console, it shows how many addresses are tried in
#' total, how many succeeded or failed
#'
#' \strong{comment}
#'
#' Below are explanations for comments:
#'
#' \itemize{
#'     \item API Error: something's wrong during API call
#'     \item Multiple Matches Error: Multiple paries of coordinates found for the
#'     same address.
#'     \item Multiple Matches Found, Using First Result: use the first result returned
#'     by geocode API
#'     \item Multiple Matches Found, Using Average Coordinates: use the average of
#'     coordinates returned by geocode API
#' }
#'
#' @examples
#' \dontrun{
#' apikey = your_apikey
#' du_google_geocode(
#'     c(
#'         "new york, usa",
#'         "#####", # contains error
#'         "canada, usa" # multiple matching results
#'     )
#' )
#' }
#' @export
du_google_geocode <- function(address,
                              apikey,
                              result_type = c("list","df","raw"),
                              multi_coord_handling = c("none","first","average"),
                              progress = TRUE,
                              show_summary = TRUE){
    # check whether package is loaded
    if(!("googleway" %in% (.packages()))){
        stop("du_google_geocode: This function must run with googleway packge loaded")
    }
    # whether to show progress
    if (progress) {
        pb <- dplyr::progress_estimated(length(address))
        safe_results <- purrr::map(address,function(address){
            pb$tick()$print()
            safely_get_geocode(address,apikey)
        })
    } else {
        safe_results <- purrr::map(address,function(address){
            safely_get_geocode(address,apikey)
        })
    }
    # first element will be applied
    result_type <- result_type[1]
    multi_coord_handling <- multi_coord_handling[1]
    # validate user input
    stopifnot(result_type %in% c("list","df","raw"))
    stopifnot(multi_coord_handling %in% c("none","first","average"))

    # error messages
    request_errors <- purrr::keep(
        setNames(
            purrr::map(safe_results, "error"),
            1:length(address)
        ),
        ~!is.null(.x)
    )
    # early return
    if(result_type == "raw"){
        return(
            list(
                result = setNames(
                    map(safe_results,~.x[["result"]][["response"]]),
                    1:length(address)
                ),
                error = request_errors
            )
        )
    } else {
        safe_df_list <- purrr::map(safe_results,~.x[["result"]][["coords"]])
        handling_info <- list()
        result_returned <- list()
        for (i in 1:length(safe_df_list)) {
            ele = safe_df_list[[i]]
            response_i = safe_results[[i]][["result"]][["response"]]
            # returns NA for missing values
            if(is.null(ele)){
                result_returned <- append(
                    result_returned,
                    setNames(
                        list(
                            data.frame(
                                lat = NA,
                                lng = NA,
                                comment = "API Error.",
                                stringsAsFactors = FALSE
                            )
                        ),
                        i
                    )
                )
                # no processing for df with one row
            } else if (is.data.frame(ele) & nrow(ele) == 1) {
                ele["comment"] = NA
                ele_m <- ele
                result_returned <- append(
                    result_returned,
                    setNames(list(ele_m),i)
                )
            } else if (is.data.frame(ele) & nrow(ele) > 1) {
                handling_info <-append(
                    handling_info,
                    setNames(list(response_i),i)
                )
                if (multi_coord_handling == "none") {
                    result_returned <- append(
                        result_returned,
                        setNames(
                            list(
                                data.frame(
                                    lat = NA,
                                    lng = NA,
                                    comment = "Multiple Matches Error.",
                                    stringsAsFactors = FALSE
                                )
                            ),
                            i
                        )
                    )
                } else if (multi_coord_handling == "first") {
                    ele["comment"] = "Multiple Matches Found, Using First Result"
                    ele_m <- ele[1,]
                    result_returned <- append(
                        result_returned,
                        setNames(list(ele_m),i)
                    )
                } else if (multi_coord_handling == "average") {
                    ele_m <- dplyr::summarise_all(ele,mean)
                    ele_m["comment"] = "Multiple Matches Found, Using Average Coordinates"
                    result_returned <- append(
                        result_returned,
                        setNames(list(ele_m),i)
                    )
                }
            }
        }
        error_element <- append(request_errors,handling_info)
        if(length(error_element) > 0) {
            error_element <- error_element[order(names(error_element))]
        }
        result_df <- bind_rows(result_returned)

        if(show_summary){

            cat(
                sprintf("\nTotal addresses: %s, Failed: %s",
                        length(address), length(error_element))
            )
        }
        if (result_type == "list") {
            return(
                list(
                    result = setNames(
                        purrr::map(result_returned,~as.list(.x)),
                        1:length(address)
                    ),
                    error = error_element
                )
            )
        }

        if (result_type == "df"){
            return(
                list(
                    result = result_df,
                    error = error_element
                )
            )
        }
    }
}
