#' write a list of data frames to excel with each data frame in a tab
#'
#' @description
#' Element names will be used as sheet name.
#'
#' @param x a list of data frames
#' @param path the path to write
#' @param add_index whether to add index number in sheet name, if \code{TRUE}, tabs
#' are named such as 1_sheetName1, 2_sheetName2 ...
#' @param overwrite whether to overwrite existing file
#'
#' @return NULL, files written to an excel file.
#'
#' @examples
#' \dontrun{
#' dflist <- list(iris = head(iris),mtcars = head(mtcars))
#' du_write_excel(dflist,"test.xlsx")
#' du_write_excel(dflist,"test.xlsx",add_index = TRUE, overwrite = TRUE)
#' }
#'
#' @export
du_write_excel <- function(x, path, add_index = FALSE, overwrite = FALSE){
    var_name <- rlang::as_string(rlang::enexpr(x))
    stopifnot(is.list(x))
    if(!("openxlsx" %in% (.packages()))){
        stop("du_write_excel: Must load 'openxlsx' to run this function.")
    }
    pb <- dplyr::progress_estimated(n = length(x))
    wb <- openxlsx::createWorkbook()
    for(i in 1:length(x)){
        if (add_index) {
            openxlsx::addWorksheet(wb, paste0(i,"_",names(x)[[i]]))
            openxlsx::writeData(wb = wb,
                                sheet = paste0(i,"_",names(x)[[i]]),
                                x = x[[i]])
        } else {
            openxlsx::addWorksheet(wb, names(x)[[i]])
            openxlsx::writeData(wb = wb,
                                sheet = names(x)[[i]],
                                x = x[[i]])
        }
        pb$tick()$print()
    }
    cat(paste("\nWriting",var_name,"to",path,"\n"))
    openxlsx::saveWorkbook(wb,path,overwrite = overwrite)
}

#' read multiple ranges from the same sheet in an excel file
#'
#' @param path file of excel file
#' @param sheet the sheet name or number to be read. If NULL, read the first sheet
#' @param range range(s) in the given sheet, can be a unnamed/named vector or list.
#' If named, names will be used as the name in the list of data frames
#' @param guess whether to guess column type, if FALSE, read all columns as text
#'
#' @return a list of data frames
#'
#' @examples
#' \dontrun{
#' file_path <- system.file("examples/excel.xlsx",package = "dfutils")
#' range1 <- c("A1:E5","H7:L11")
#' range2 <- list(tabl1="A1:E5",table2="H7:L11")
#' du_read_excel_ranges(file_path,range = range1)
#' du_read_excel_ranges(file_path,range = range2,guess = FALSE)
#' }
#'
#' @export
du_read_excel_ranges <- function(path,sheet = NULL,range,guess = TRUE){
    #default to read everything as string
    if(guess){
        df_list <- purrr::map(range,function(ran){
            readxl::read_excel(path,sheet,range = ran)
        })
    } else {
        df_list <- purrr::map(range,function(ran){
            readxl::read_excel(path,sheet,range = ran,col_types = "text")
        })
    }
    if(!is.null(names(range))){
        names(df_list) <- names(range)
    }


    return(df_list)
}

