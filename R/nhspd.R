#' NHS Postcode Directory
#'
#' The NHS Postcode Directory is a database of all of the Postcodes within the
#' United Kingdom, along with the codes for things like CCG, STP, Local Authority
#' for which that postcode belongs to. Unfortunately, the raw csv's do not have
#' any column headings, so it's pretty difficult to use.
#'
#' This script will handle loading of the csv by adding in the correct column
#' headings and datatypes for those columns.
#'
#' Instructions for use -----
#'
#' Download from https://geoportal.statistics.gov.uk the latest NHSPD file.
#' This is found under the "Postcodes"->"NHS Postcode Directory (NHSPD)" menu.
#'
#' Download the "NHS Postcode Directory UK Full" version, and not the extract
#' version.
#'
#' Unzip the contents of the download and update the "path" variable to point to
#' the correct location (alternatively, extract the zip contents to your current
#' R working directory: you can view this folder easily by clicking the "More"
#' button in the R-Studio Files pane and selecting "Show folder in new window").
#'
#' Inside the folder there are a number of directories. If you look at the word
#' document inside of the "Contents" folder this will explain the structure of
#' the NHSPD. The "User Guide" is also very useful as it explains what each of
#' the columns contains.
#'
#' NOTE: loading the complete csv will use up a large amount of RAM, (the Nov-19
#'       version used 2.3 GB of RAM on my machine). You may wish to use one of
#'       the regional files (see the Contents folder to figure out which to use),
#'       or you may wish to consider altering the column types to skip columns
#'       that you are not interested in. To do this, change the col type for that
#'       column to be col_skip().
#'
#' @param file the file to load, defaults to a file that begins with "nhg" in
#'             the folder "path"
#' @param path the path where the file is stored. Defaults to "Data" in the
#'             current directory. Change this to where you saved the downloaded
#'             files, or set "." for the current directory
#'
#' @importFrom readr read_csv cols col_character col_double
#' @export
nhspd_load <- function(file = dir(path, "^nhg.*\\.csv$", full.names = TRUE),
                       path = "./Data") {

  col_types = {cols(
    "pcd2"       = col_character(),
    "pcds"       = col_character(),
    "dointr"     = col_double(),
    "doterm"     = col_double(),
    "oseast100m" = col_double(),
    "osnrth100m" = col_double(),
    "oscty"      = col_character(),
    "odslaua"    = col_character(),
    "oslaua"     = col_character(),
    "osward"     = col_character(),
    "usertype"   = col_double(),
    "osgrdind"   = col_double(),
    "ctry"       = col_character(),
    "oshlthau"   = col_character(),
    "rgn"        = col_character(),
    "oldha"      = col_character(),
    "nhser"      = col_character(),
    "ccg"        = col_character(),
    "psed"       = col_character(),
    "cened"      = col_character(),
    "edind"      = col_character(),
    "ward98"     = col_character(),
    "oa01"       = col_character(),
    "nhsrlo"     = col_character(),
    "hro"        = col_character(),
    "lsoa01"     = col_character(),
    "ur01ind"    = col_character(),
    "ms0a01"     = col_character(),
    "cannet"     = col_character(),
    "scn"        = col_character(),
    "oshaprev"   = col_character(),
    "oldpct"     = col_character(),
    "oldhro"     = col_character(),
    "pcon"       = col_character(),
    "canreg"     = col_character(),
    "pct"        = col_character(),
    "oseast1m"   = col_double(),
    "osnrth1m"   = col_double(),
    "oa11"       = col_character(),
    "lsoa11"     = col_character(),
    "msoa11"     = col_character(),
    "calncv"     = col_character(),
    "stp"        = col_character()
  )}

  nhspd <- read_csv(file,
                    col_names = names(col_types$cols),
                    col_types = col_types)

  class(nhspd) <- c("nhspd", class(nhspd))

  nhspd
}

#' Convert NHSPD table to SF
#'
#' Converts NHSPD to a SF object by turning the OS easting and northing values to a st_point
#'
#' @param nhspd NHS Postcode Database object (loaded by `nhspd_load()`)
#'
#' @importFrom dplyr %>% mutate case_when nest_by
#' @importFrom purrr map2 map_dfr pmap compose
#' @importFrom sf st_point st_as_sf st_transform
#' @export
#' @md
nhspd_as_sfc <- function(nhspd) {
  if (!inherits(nhspd, "nhspd")) {
    stop("`nhspd` object is not a proper nhspd object. Was it loaded with `nhspd_load()`?")
  }
  nhspd %>%
    mutate(geometry = map2(oseast1m, osnrth1m, compose(st_point, c))) %>%
    # NI postcodes use Irish National Grid (29900). Otherwise, use British National Grid (27700)
    # let's convert NI postcodes to use 27700
    mutate(crs = case_when(ctry == "N92000002" ~ 29900, TRUE ~ 27700)) %>%
    nest_by(crs, .key = "x") %>%
    pmap(st_as_sf) %>%
    map_dfr(st_transform, 27700)
}
