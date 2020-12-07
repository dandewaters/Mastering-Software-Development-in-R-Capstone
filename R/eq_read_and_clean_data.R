#' @title Read in NOAA dataset
#'
#' @details throws an error if file path does not exist
#'
#' @param filename character string of path to raw NOAA data file
#'
#' @return Returns a dataframe of the raw NOAA data
#'
#' @examples \dontrun{raw_data <- eq_read_data("./inst/extdata/earthquakes.tsv")}
#'
#' @importFrom readr read_delim
#'
#' @export
eq_read_data <- function(filename){
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")

  suppressWarnings(data <- readr::read_delim(file=filename,
                                             delim="\t",
                                             col_names=TRUE,
                                             progress=FALSE,
                                             col_types=readr::cols()))
  data
}



#' @title Clean raw NOAA dataset
#'
#' @param raw_data data frame of raw NOAA data
#'
#' @return Returns a dataframe of the clean NOAA data
#'
#' @examples # Read in raw NOAA data
#'           \dontrun{raw_data <- eq_read_data("./inst/extdata/earthquakes.tsv")}
#'           \dontrun{clean_data <- eq_clean_data(raw_data)}
#'
#' @importFrom magrittr "%>%"
#' @importFrom lubridate ymd
#' @importFrom dplyr rename_all
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom tidyr separate
#'
#' @export
eq_clean_data <- function(raw_data){

  data <-
    raw_data %>%
    # Rename columns
    dplyr::rename_all(tolower) %>%
    dplyr::rename_all(~ gsub(" ", "_", .)) %>%
    dplyr::rename_all(~ gsub("\\(|\\)|\\$", "", .)) %>%
    dplyr::rename("month"="mo", "day"="dy", "hour"="hr", "minute"="mn", "second"="sec",
                  "tsunami"="tsu", "volcano"="vol", "magnitude"="mag") %>%
    # Remove Tsunami observations
    dplyr::filter(is.na(tsunami)) %>%
    dplyr::filter(!is.na(year) & !is.na(month) & !is.na(day)) %>%
    ## Create clean date column
    # Workaround for bce years
    dplyr::mutate(date = lubridate::ymd(paste("0000", month, day, sep="-"))) %>%
    dplyr::mutate(date = date + lubridate::years(year)) %>%
    # Cast lat and long to numeric
    dplyr::mutate(longitude = as.numeric(longitude)) %>%
    dplyr::mutate(latitude = as.numeric(latitude)) %>%
    # Add country column and clean location name column
    tidyr::separate(col=location_name, into=c("country"), remove=FALSE, extra="drop") %>%
    dplyr::mutate(country = eq_country_clean(country)) %>%
    dplyr::mutate(location_name = eq_location_clean(location_name)) %>%
    # remove unnecessary columns
    dplyr::select(-search_parameters, -hour, -minute, -second, -tsunami, -volcano)

  return(data)
}



#' @title Populate country column
#'
#' @description Returns "US" for any
#'
#' @param location The country column in the NOAA dataset
#'
#' @return string "US" if location is a American state
#'
#' @examples \dontrun{eq_country_clean("UTAH")}
#'
#' @importFrom dplyr if_else
#'
#' @export
eq_country_clean <- function(location){
  if_else(location %in% toupper(datasets::state.name),
          "US", location)
}



#' @title Clean NOAA Location Name Column
#'
#' @details Strips out the country name (and the colon) from the location name column and adds title casing
#'
#' @param location_name the location name column in the NOAA dataset
#'
#' @return the cleaned location name column for the NOAA dataset
#'
#' @examples \dontrun{eq_location_clean("GREECE:  MOUNT TAYGETUS")}
#'
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_to_title
#'
#' @export
eq_location_clean <- function(location_name){
  # Strip out country name
  location_name <- stringr::str_remove_all(location_name, "\\w+: +")
  # Convert to title case
  location_name <- stringr::str_to_title(location_name)

  return(location_name)
}
