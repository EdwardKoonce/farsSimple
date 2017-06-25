#'Read a csv File
#'
#'This function reads a csv file into R and returns a dataframe.
#'It is internal to this package.If the file name does not exist,
#'an error is generated and the function stopped.
#'
#'@param filename the name of the csv file to be read. File should be in the same
#'working directory as the R code or an error will be generated.
#'
#'@return function returns a dataframe or an error if the file does not exist.
#'
#'@examples
#'\dontrun{
#'dat <- fars_read("accident_2013.csv.bz2")
#'}
#'
#'@importFrom readr read_csv
#'@importFrom dplyr tbl_df

fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#'Make a filename
#'
#'This function generates a file namein the form "accident_%d.csv.bz2"
#'using the C wrapper sprintf where d = year (an integer). This
#'function is internal to this package. If year is not an integer,
#'then an error is generated.
#'
#'@param year year is an integer argument
#'
#'@return a string of the form "accident_year.csv.bz2"
#'
#'@examples
#'\dontrun{
#'file <- make_filename(2013)
#'}

make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#'Create new dataframe from csv file
#'
#'This function modifies the dataframe from a csv file read into R.
#'The csv file is of the form "accident_2013.csv.bz2", where 2013 is the year
#'of interest. The new dataframe has variables month and year. The single
#'argument is years, a list of years such as \code{list(2013,2014,2015)}.
#'The function uses lapply to loop through the list of years to create
#'the new dataframe. The function calls the internal function \code{make_filename(year)}
#'to get a file name and the internal function \code{fars_read(file)} to read 
#'the csv file into R. An error is generated if the year does not exist or
#'if the file does not exist
#'
#' @param years a list of years such as \code{list(2013,2014,2015)} year 
#' be an integer.
#' 
#'@return function returns a list of dataframes or NULL if the available
#'files do not include the year of interest.
#'
#'examples
#'\donotrun{
#'dat_list <- fars_read_years(list(2013,2014,2015))
#'}
#'
#'@importFrom dplyr mutate select
#'@importFrom magrittr %>%
#'
#'@export    

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

#'Summary of accidents
#'
#'This function takes as an argument a list composed of years(integers)
#'and returns a summary of accidents per month for a given year is a dataframe.
#'It calls the function \code{fars_read_years(years)}. If the year cannot be
#'found, then an error is generated.
#'
#'@param years a list of years(integers) such as \code{list(2013,2014,2015)}
#'
#'@return the function returns a dataframe with years as a variable and a 
#'summary of the accidents grouped by month.
#'
#'@examples 
#'\dontrum{
#'fars_summarize_years(list(2013,2014,2015))
#'}
#'
#'@importFrom dplyr find_rows group_by summarize
#'@importFrom tidyr spread
#'@importFrom magrittr %>%
#'
#'@export

fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>% 
                dplyr::group_by(year, MONTH) %>% 
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}
