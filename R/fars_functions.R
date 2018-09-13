#' @title Reads and Loads a CSV file
#'
#' @description
#' This function reads a CSV file with the name passed as an argument
#' in the argument <filename>. It returns a data frame with the data
#' read from the file.
#'
#' @param filename Name (with the path) (character) of the CSV file to be read
#'
#' @return A data.frame object with data from the read CSV file
#'
#' @note If the file does not exist it stops with an error
#'
#' @examples
#' \dontrun{
#' fars_read("accident_2013.csv.bz2")
#' fars_read("accident_2014.csv.bz2")
#' }
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}


#' @title Creates a filename
#'
#' @description
#' This function takes in an argument for a year in the format yyyy and
#' generates a file name in the format accident_<yyyy>.csv.bz2.
#'
#' @param year Year (integer/numeric) in the format yyyy
#'
#' @return A file name (character) in the format accident_<yyyy>.csv.bz2
#' where yyyy is the passed value in the argument year.
#'
#' @examples
#' \dontrun{
#' make_filename(2013)
#' make_filename(2014)
#' }
#'
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}


#' @title Produces a list of month,year from the read data
#'
#' @description
#' This function takes in an argument of a vector of years with each
#' year in the format yyyy and generates a list containing month,year
#' for each month year combinations in the data from the read CSv file.
#'
#' @param years An integer/numeric vector of years, with each year in the format yyyy
#'
#' @return A list of data.frame objects with columns month, year based on the years passed
#' in the argument years and the data contained in the associated files for those years
#'
#' @note The files need to be in the current working directory
#'
#' @examples
#' \dontrun{
#' fars_read_years(2013:2015)
#' fars_read_years(c(2013, 2015))
#' }
#'
#' @importFrom dplyr %>% mutate select
#'
#' @export
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>%
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}


#' @title Summarizes number of accidents by month and year
#'
#' @description This function takes in an argument of a vector of years with each
#' year in the format yyyy and generates a summary of accidents by month
#' and each year passed in the argument years.
#'
#' @param years An integer/numeric vector of years, with each year in the format yyyy
#'
#' @return A data.frame of columns <month>, <year1>, <year2>, <year3>, ... with each
#' year column having the number of accidents for each month,year combination
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(2013:2015)
#' fars_summarize_years(c(2013, 2015))
#' }
#'
#' @importFrom dplyr %>% bind_rows group_by summarize
#' @importFrom tidyr spread
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}


#' @title Generates a plot of all the accidents for a state and year combination
#'
#' @description
#' This function takes in an argument of a state number (1 to 12) and
#' year (yyyy) and generates a plot of all the accidents in the state,year
#' combinaiton.
#'
#' @param state.num State number (1 to 12) (integer/numeric) as used in the data files
#'
#' @param year Year (integer/numeric) in the format yyyy
#'
#' @return A NULL, however it generates a plot of all the accidents in the state for
#' the year
#'
#' @note If the state number does not exist in the CSV file for the year
#' then it stops with an error.
#'
#' @examples
#' \dontrun{
#' fars_map_state(1, 2013)
#' fars_map_state(10, 2014)
#' }
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @export
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}
