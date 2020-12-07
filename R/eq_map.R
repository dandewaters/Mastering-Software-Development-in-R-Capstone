#' @title Make labels for each earthquake attribute
#'
#' @description Constructs individual labels for a given earthquake attribute
#'
#' @details Skips over labels for missing values
#'
#' @param label_name String of a label name for column
#' @param column Data frame column from earthquake data to be labeled
#'
#' @return A string in HTML style to be used as annotation text
#'
#' @examples \dontrun{make_label("Location", data$location_name)}
#'
#' @importFrom dplyr if_else
#'
#' @export
make_label <- function(label_name, column){
  dplyr::if_else(is.na(column),
                 "",
                 paste("<b>", label_name, ":</b>", as.character(column), "<br>"))
}



#' @title Make annotation text for eq_map()
#'
#' @description Constructs label column to be used as the annotation text in the leaflet map
#'
#' @details Skips over labels for missing values
#'
#' @param data Data frame of earthquake observations to be labeled
#'
#' @return A string in HTML style to be used as annotation text
#'
#' @examples \dontrun{eq_create_label(data)}
#'
#' @importFrom dplyr mutate
#' @importFrom magrittr "%>%"
#'
#' @export
eq_create_label <- function(data){
 labels <-
   data %>%
   dplyr::mutate(location_label = make_label("Location", location_name)) %>%
   dplyr::mutate(magnitude_label =  make_label("Magnitude", magnitude)) %>%
   dplyr::mutate(deaths_label =  make_label("Deaths", deaths)) %>%
   dplyr::mutate(label = paste0(location_label, magnitude_label, deaths_label))

 return(labels)
}


#' @title Map earthquake events from NOAA dataset
#'
#' @description Plots earthquake observations to an interactive where the points are the coordinates of the epicenters of the earthquakes.
#'
#' @param data Data frame of the cleaned NOAA earthquake dataset
#' @param annot_col String of name of column to be used for point annotations
#'
#' @return An interactive leaflet map with points of earthquake observations
#'
#' @examples # Read in raw NOAA data
#'           \dontrun{raw_data <- eq_read_data("./inst/extdata/earthquakes.tsv")}
#'           \dontrun{clean_data <- eq_clean_data(raw_data)}
#'           \dontrun{min_date <- ymd("2020-01-01")}
#'           \dontrun{max_date <- ymd("2020-01-31")}
#'           \dontrun{data <- filter(clean_data, date >= min_date & date <= max_date)}
#'           \dontrun{data %>%}
#'           \dontrun{eq_map(annot_col="date")}
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select
#' @importFrom leaflet leaflet
#' @importFrom leaflet addTiles
#' @importFrom leaflet addCircleMarkers
#'
#' @export
eq_map <- function(data, annot_col){

  data %>%
  dplyr::select(.data$latitude,
                .data$longitude,
                .data$magnitude,
                annot_col) %>%
  leaflet::leaflet() %>%
  leaflet::addTiles() %>%
  leaflet::addCircleMarkers(lat    = ~ latitude,
                            lng    = ~ longitude,
                            radius = ~ magnitude * 2,
                            popup  = ~ get(annot_col))
}
