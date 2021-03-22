#' ODS Organisations
#'
#' This function queries the \href{https://digital.nhs.uk/services/organisation-data-service/guidance-for-developers/search-endpoint}{ODS Search API}.
#'
#' One or more arguments should be specified, if not it will default (with a
#' warning to using the primary role of "NHS Trust")
#'
#' @param name a character: queries organisations that contain that word in the
#'   name of the organisation
#' @param post_code a character: queries organisations based on their postcode.
#'   You can search by either the full postcode, or a partial postcode. If you
#'   use a partial postcode then it must be at least the Outcode portion of the
#'   postcode (e.g. B1, WV10).
#' @param last_change_date a date: queries all organisations that have been
#'   changed after that date
#' @param status a character (either active or inactive): searches for
#'   organisations based on their status
#' @param primary_role_id a character: queries all organisations that have the
#'   specified primary role (see \code{\link{ods_get_roles}})
#' @param non_primary_role_id a character: queries all organisations that have
#'   the role as a secondary role (see \code{\link{ods_get_roles}})
#' @param org_record_class a character: queries all organisations based on their
#'   record class TODO: implement a function that queries this API
#'
#' @return a tibble of organisations
#' @export
#'
#' @importFrom assertthat assert_that is.scalar is.string is.date
#' @importFrom dplyr bind_rows coalesce
#' @importFrom httr GET status_code headers content
#' @importFrom janitor clean_names
#' @importFrom rlang enexpr
#' @importFrom stringr str_starts
#' @importFrom utils tail URLencode
#'
#' @examples
#' \dontrun{
#' # this will default to getting organisations with a primary role of "NHS
#' # TRUST", but it will throw a warning:
#' ods_get_organisations()
#'
#' # you could be more specific and specify that you want to use a primary role
#' # of "NHS TRUST" using the code from ods_get_roles()
#' primary_role_id <- ods_get_roles() %>%
#'   filter(displayName == "NHS TRUST") %>%
#'   pull(id)
#' ods_get_organisations(primary_role_id = primary_role_id)
#'
#' # or, you could use the specific functions for the primary role id's
#' ods_get_trusts()
#' ods_get_trust_sites()
#' ods_get_ccgs()
#'
#' # you can use as many of the arguments as you like
#' # get current active CCG's
#' ods_get_ccgs(status = "active")
#'
#' # get NHS Trust Sites with "Royal" in their name
#' ods_get_trust_sites(name = "Royal")
#' }
ods_get_organisations <- function(name = as.character(NA),
                                  post_code = as.character(NA),
                                  last_change_date = as.Date(NA),
                                  status = c(NA, "active", "inactive"),
                                  primary_role_id = as.character(NA),
                                  non_primary_role_id = as.character(NA),
                                  org_record_class = as.character(NA)) {
  # limit of number of rows we can get at any time
  LIMIT <- ods_api_results_limit()

  # url is built up of api and the parameters. We need to keep a copy of the
  # initial url to compare to later: the api will fail to run if no parameters
  # are passed
  url <- init_url <- paste0(ODS_API_ENDPOINT, "organisations?Limit=", LIMIT)

  # argument checking ==========================================================

  # helper to validate arguments and update the url variable
  arg_check <- function(arg, is.type = is.string) {
    # get the name of the argument
    arg_name <- as.character(substitute(arg))

    # extract the "type" from the is.type function. This function should be of
    # the like "is.character" or "assertthat::is.string".
    type_name <- gsub("^is\\.([^.]*)$",
                      "\\L\\1",
                      tail(as.character(substitute(is.type)), 1),
                      perl = TRUE)

    assert_that(is.scalar(arg),
                msg = paste(arg_name, "argument must be a single value"))
    assert_that(is.type(arg),
                msg = paste(arg_name, "argument must be of type", type_name))

    # if the argument values are NA's then return
    if (is.na(arg)) return (NULL)

    # if arg is a string, make sure it's not empty (or whitespace)
    if (type_name == "string") {
      assert_that(!grepl("^\\s*$", arg),
                  msg = paste(arg_name, "argument must not be an empty string"))
    }

    # if we are working with a date then make sure the value is formatted
    # correctly
    if (type_name == "date") {
      arg <- format(arg, "%Y-%m-%d")
    }
    # this will convert to title case from snake case, needed for the URL
    arg_name <- gsub("(?:^|_)(.)", "\\U\\1", arg_name, perl = TRUE)
    # update the url in the parent environment
    url <<- paste0(url, "&", arg_name, "=", utils::URLencode(arg))

    NULL
  }

  status <- match.arg(status)

  arg_check(name)
  arg_check(post_code)
  arg_check(last_change_date, is.date)
  arg_check(status) # only call arg_check on status to update url
  arg_check(primary_role_id)
  arg_check(non_primary_role_id)

  if (dplyr::coalesce(!stringr::str_starts(post_code, "\\w{1,2}\\d"), FALSE)) {
    stop ("post_code argument must be at least a district, e.g. AA1")
  }

  # checked all arguments ======================================================

  # if no arguments were passed, let's use NHS TRUST as primary role
  if (url == init_url) {
    warning("No arguments specified: defaulting to Primary Role == NHS TRUST")
    url <- paste0(url, "&PrimaryRoleId=RO197")
  }

  # get the initial page of data
  res <- httr::GET(url)

  # make sure the api call was successful
  if (httr::status_code(res) != 200) {
    stop("unable to load data from API")
  }

  # find out how many records there are in total to load
  total_count <- as.numeric(httr::headers(res)[["x-total-count"]])

  # convert the results into a tibble
  organisations <- dplyr::bind_rows(httr::content(res)[["Organisations"]])

  # if we have more results than we can load in one go
  offset <- 0
  while (total_count > nrow(organisations)) {
    # load the next page of results
    offset <- offset + LIMIT
    res <- httr::GET(paste0(url, "&Offset=", offset))

    # again, make sure call was successful
    if (httr::status_code(res) != 200) {
      stop("unable to load data from API")
    }

    # update the organisations data to include next page of data
    organisations <- dplyr::bind_rows(
      organisations,
      httr::content(res)[["Organisations"]]
    )
  }
  # return the organisations data
  janitor::clean_names(organisations)
}

#' @rdname ods_get_organisations
#' @export
ods_get_trusts <- function(name = as.character(NA),
                           post_code = as.character(NA),
                           last_change_date = as.Date(NA),
                           status = c(NA, "active", "inactive"),
                           non_primary_role_id = as.character(NA),
                           org_record_class = as.character(NA)) {
  ods_get_organisations(name,
                        post_code,
                        last_change_date,
                        status,
                        primary_role_id = "RO197",
                        non_primary_role_id,
                        org_record_class)
}

#' @rdname ods_get_organisations
#' @export
ods_get_trust_sites <- function(name = as.character(NA),
                                post_code = as.character(NA),
                                last_change_date = as.Date(NA),
                                status = c(NA, "active", "inactive"),
                                non_primary_role_id = as.character(NA),
                                org_record_class = as.character(NA)) {
  ods_get_organisations(name,
                        post_code,
                        last_change_date,
                        status,
                        primary_role_id = "RO198",
                        non_primary_role_id,
                        org_record_class)
}

#' @rdname ods_get_organisations
#' @export
ods_get_ccgs <- function(name = as.character(NA),
                         post_code = as.character(NA),
                         last_change_date = as.Date(NA),
                         status = c(NA, "active", "inactive"),
                         non_primary_role_id = as.character(NA),
                         org_record_class = as.character(NA)) {
  ods_get_organisations(name,
                        post_code,
                        last_change_date,
                        status,
                        primary_role_id = "RO98",
                        non_primary_role_id,
                        org_record_class)
}
