context("Throwing Errors")

test_that("Throws errors", {
  throws_error(eq_read_data("ThisFileNameDoesn'tExist.tsv"))
})

context("Reading files are correct")

test_that("Reads files correctly", {
  filename <- system.file("extdata", "earthquakes.tsv", package = "msdirCapstone", mustWork=TRUE)
  df <- eq_read_data(filename)
  expect_that(df, is_a("data.frame"))
})



context("Cleaning Earthquake data is correct")

test_that("eq_clean_data() is correct", {
  filename <- system.file("extdata", "earthquakes.tsv", package = "msdirCapstone", mustWork=TRUE)

  clean_df <-
    eq_read_data(filename) %>%
    eq_clean_data()

  # Check that clean data frame has correct dimensions
  expect_equivalent(dim(clean_df), c(3891, 35))

  # Check that clean data has correct columns
  cols <- c("location_name", "country",
            "latitude", "longitude",
            "magnitude", "deaths", "date")

  contains_correct_cols <- all(cols %in% names(clean_df))
  expect_true(contains_correct_cols)
})



context("Data cleaning helper functions are correct")

test_that("eq_country_clean() is correct", {
  expect_equivalent(eq_country_clean("UTAH"), "US")

  expect_equivalent(eq_country_clean("Not a US State"), "Not a US State")
})

test_that("eq_location_clean() is correct", {
  expect_equivalent(eq_location_clean("GREECE:  MACEDONIA"), "Macedonia")

  expect_equivalent(eq_location_clean("GREECE:  CRETE:  KNOSSOS"), "Knossos")

  expect_equivalent(eq_location_clean("MARYLAND"), "Maryland")
})



context("Geoms are correct")

test_that("geom_timeline() is correct", {
  filename <- system.file("extdata", "earthquakes.tsv", package = "msdirCapstone", mustWork=TRUE)
  min_date <- ymd("2015-01-01")
  max_date <- ymd("2020-01-01")
  countries <- c("CHINA", "US")

  eq_plot <-
    eq_read_data(filename) %>%
    eq_clean_data() %>%
    filter(date >= min_date & date <= max_date & country %in% countries) %>%
    ggplot(aes(x=date, size=magnitude, fill=deaths, color=deaths)) +
    geom_timeline()

  expect_that(eq_plot, is_a("ggplot"))
})

test_that("geom_timeline_label() is correct", {
  filename <- system.file("extdata", "earthquakes.tsv", package = "msdirCapstone", mustWork=TRUE)
  min_date <- ymd("2015-01-01")
  max_date <- ymd("2020-01-01")
  countries <- c("CHINA", "US")

  eq_plot_label <-
    eq_read_data(filename) %>%
    eq_clean_data() %>%
    filter(date >= min_date & date <= max_date & country %in% countries) %>%
    ggplot(aes(x=date, size=magnitude, fill=deaths, color=deaths)) +
    geom_timeline()

  expect_that(eq_plot_label, is_a("ggplot"))
})



context("eq_map and helper functions are correct")

test_that("make_label is correct", {
  label_1 <- make_label("Location", "Ridgecrest")
  expect_equivalent(label_1, "<b> Location :</b> Ridgecrest <br>")

  label_2 <- make_label("Location", NA)
  expect_equivalent(label_2, "")
})

test_that("eq_create_label is correct", {
  filename <- system.file("extdata", "earthquakes.tsv", package = "msdirCapstone", mustWork=TRUE)
  min_date <- ymd("2015-01-01")
  max_date <- ymd("2020-01-01")
  countries <- c("CHINA", "US")

  eq_plot_label <-
    eq_read_data(filename) %>%
    eq_clean_data() %>%
    filter(date >= min_date & date <= max_date & country %in% countries) %>%
    eq_create_label()


  label_cols <- c("location_label", "magnitude_label", "deaths_label", "label")

  contains_correct_cols <- all(label_cols %in% names(eq_plot_label))
  expect_true(contains_correct_cols)
})


test_that("eq_map is correct", {
  filename <- system.file("extdata", "earthquakes.tsv", package = "msdirCapstone", mustWork=TRUE)
  min_date <- ymd("2015-01-01")
  max_date <- ymd("2020-01-01")
  countries <- c("CHINA", "US")

  eq_map_plot <-
    eq_read_data(filename) %>%
    eq_clean_data() %>%
    filter(date >= min_date & date <= max_date & country %in% countries) %>%
    eq_map(annot_col = "date")

  expect_that(eq_map_plot, is_a("leaflet"))
})
