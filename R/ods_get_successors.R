#' ODS Successor Org Lookup
#'
#' Builds a lookup table for organisations that have merged over time
#'
#' @importFrom dplyr %>% mutate across case_when
#' @importFrom purrr map_dfr
#' @importFrom readr read_csv
#' @importFrom lubridate ymd
ods_get_successors <- function() {
  # create a temporary file
  tf <- tempfile(fileext = ".zip")
  td <- tempdir()

  # url of archives
  url <- "https://files.digital.nhs.uk/assets/ods/current/"

  map_dfr(c("succ", "succarc"), function(file) {
    paste0(url, file, ".zip") %>%
      download.file(tf, mode = "wb")
    unzip(tf, files = paste0(file, ".csv"), exdir = td) %>%
      read_csv(col_names = c("old_code",
                             "new_code",
                             "reason",
                             "effective_date",
                             "indicator"),
               col_types = "ccccc")
  }) %>%
    mutate(
      across(effective_date, ymd),
      across(reason, ~case_when(
        .x == "F" ~ "FMR code change",
        .x == "O" ~ "Org. type change",
        .x == "R" ~ "Reconfiguration"
      )),
      across(indicator, ~case_when(
        .x == "F" ~ "Further succession",
        .x == "X" ~ "Closed",
        TRUE ~ "No further succession"
      ))
    )
}
