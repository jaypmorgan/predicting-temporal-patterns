library(zoo)
library(here)
library(tidyverse)
library(clock)
library(lubridate)

#' Create a catalogue of keywords from the list of raw files.
#' @param path This is the path to the raw data (the long list of CSV
#'     files).
#' @return Return a tibble with 3 columns (filename, keyword,
#'     raw_filepath). The filename is the name of the CSV file, the
#'     keyword is a formatted version of the keyword. Finally, the
#'     raw_filepath is the path to the CSV file from the root
#'     directory.
create_catalogue <- function(path) {
  files <- list.files(here(path)) %>%
    tibble(filename = .)

  good_files <- files %>%
    filter(str_ends(filename, ".csv")) %>%  # only CSV files needed
    filter(filename != "where .csv") %>%  # remove duplicate `where` keyword
    filter(filename != "money.csv") %>%  # remove duplicate `money` keyword
    filter(filename != "sleep.csv")  # remove duplicate `sleep` keyword

  good_files <- good_files %>%
    mutate(
      keyword = str_replace_all(filename, "\\s?.csv", "") %>%
        str_to_title %>%
        str_replace_all("Where1", "Where") %>%
        str_replace_all("Mot", "MOT") %>%
        str_replace_all("Ssri", "SSRI") %>%
        str_replace_all("Hmrc", "HMRC")
    ) %>%  # make pretty keywords
    mutate(raw_filepath = paste0("data/raw/", filename))  # setup new path

  return(good_files)
}

list_keywords <- function(data) {
    return(data %>% pull(keyword) %>% unique)
}

#' Using a catalogue, read all the files and compose them in a way
#' that we can use them later on.
#' @param catalogue The tibble of CSV files to use. This can be
#'     created using the `create_catalogue` function.
#' @return A tibble containing the raw data for each CSV file.
load_from_catalogue <- function(catalogue) {
  # read into a big dataframe
  read_data <- function(filename, keyword) {
    return(read_csv(
      filename,
      show_col_types = FALSE,
      name_repair = "unique_quiet"
    ))
  }

  data <- catalogue %>%
    mutate(out = map2(raw_filepath, keyword, ~ read_data(.x, .y))) %>%
    unnest(out) %>%
    select(-c(...1, V169))

  return(data)
}


#' create_mean_curves
#'
#' Aggregate data for all keywords and compute the mean
#' curve for this keyword.
#'
#' @param data The raw data (all weeks for each keyword)
#'
#' @return A tibble of the mean curve for each keyword
#' @export
#'
#' @examples
create_mean_curves <- function(data, no.normalise = FALSE) {
  data %>%
    group_by(keyword, hour) %>%
    summarise(value = mean(search_index)) %>%
    mutate(value = ifelse(rep(no.normalise, n()), value,
                          (value-min(value))/(max(value)-min(value)))) %>%
    ungroup()
}


split_train_test <- function(classes,
                             test_frac = .2,
                             seed = NA) {
  if (!is.na(seed)) {
    set.seed(seed)
  }
  sampled_classes <-
    classes %>% group_by(class) %>% sample_frac(test_frac) %>% pull(keyword)
  classes %>%
    mutate(subset = if_else(keyword %in% sampled_classes, "test", "train"))
}

na_replacement <- function(d) {
  if (!anyNA(d)) {
    return(d)  # no need to do any interpolation
  }

  get_first_value <- function(start_index) {
      first_point <- NA
      first_index <- NA
      for (jdx in start_index:length(d)) {
          if(!is.na(d[jdx])) {
              first_point <- d[jdx]
              first_index <- jdx
              break
          }
      }
      return(c(first_point, first_index))
  }

  for (index in 1:length(d)) {
      di <- d[index]
      if (is.na(di) && index == 1) {
          ## copy first value
          d[index] = d[get_first_value(index+1)[2]]
          next
    }
    if (is.na(di) && index != length(d)) {
        ## interpolate between two points
        f <- get_first_value(index+1)
        first_point <- f[1]
        first_index <- f[2]

        if(is.na(first_point)) {
            d[index] <- d[index-1]
        } else {
            last_index <- index-1
            d[index] <- d[last_index] + (index - last_index) * ((first_point - d[last_index]) / (first_index - last_index))
        }
        next
    }
    if (is.na(di)) {
      ## last value
      d[index] <- d[index-1]
      next
    }
  }
  return(d)
}

prepare_data <- function(data) {
    keywords <- list_keywords(data)

    data %>%
        group_by(keyword) %>%
        mutate(week = row_number()) %>%
        ungroup() %>%
        pivot_longer(
            cols = starts_with("V"),
            names_to = "hour",
            values_to = "search_index") %>%
        mutate(hour = str_replace_all(hour, "V", "") %>% as.integer,
               day = ((hour-1) %/% 24)+1,
               hour = (hour %% 24),
               hour = ifelse(hour == 0, 24, hour)) %>%
        group_by(keyword, week, day) %>%
        mutate(search_index = na_replacement(search_index), uid = cur_group_id()) %>%
        ungroup() %>%
        mutate(keyword_id = match(keyword, keywords)) %>%
      group_by(keyword) %>%
      mutate(date = seq(from = as.POSIXct("2018/01/01 00:00:00"), length = n(), by = "hour")) %>%
      ungroup()
}

label_converter <- function(label) {
  if (is.numeric(label)) {
    switch (label,
      "1" = return('A'),
      "2" = return('B'),
      "3" = return('C'),
    )
  }
  if (is.character(label)) {
    switch (label,
      'A' = return(1),
      'B' = return(2),
      'C' = return(3),
    )
  }
}
