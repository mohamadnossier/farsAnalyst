#' @title Read data #
#'
#' @description This function reads comma separated value formatted dataset and returns it as an R dataframe table.
#'
#' @param filename A string of characters
#'
#' @return An R dataframe table
#'
#' @details This function throws an error message if the filename does not exist.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#' \dontrun{fars_read("filename.csv")}
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

#' @title Print a given year's filename
#'
#' @description This function prints a string of characters resulted from the merging of a prespecified text and a year given as parameter
#'
#' @param year Integer
#'
#' @return This function returns a string of characters resulted from the merging of a prespecified text and a year given as parameter
#'
#' @examples
#' \dontrun{
#' make_filename(2001)
#' make_filename(2020)
#' }
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' @title Create a months and years dataframe for a given number of years
#'
#' @description This function reads multiple comma separated values files for a given number of years, then create an aggregated view of months and years from the read files.
#'
#' @param years Integers vector
#'
#' @return This function returns an aggregated dataframe of months for each year from the read files.
#'
#' @details This function throws an error message if the file matching the given year does not exist.
#'
#' @importFrom dplyr mutate select %>%
#'
#' @examples
#' \dontrun{
#' fars_read_years(2020)
#' fars_read_years(c(2001, 2002))
#' }
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

#' @title Summarize Years
#'
#' @description This function generates a summary of the monthly number of accidents for each month in the given number of years.
#'
#' @param years Integers vector
#'
#' @return This function returns a dataframe summarizing the monthly number of accidents for each month in the given number of years.
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(2020)
#' fars_summarize_years(c(2001, 2002))
#' }
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' @title Map State
#'
#' @description Plot a map displaying all accidents previously recorded in the provided state number and year.
#'
#' @param state.num Integer
#' @param year Integer
#'
#' @return This function returns an object for graphical visualization for accidents' locations.
#'
#' @examples
#' \dontrun{
#' fars_map_state(16, 2020)
#' fars_map_state(70, 2014))
#' }
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
