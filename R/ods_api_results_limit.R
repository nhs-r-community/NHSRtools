#' ODS API Results Limit
#'
#' Get's, or set's, the current limit of results to be returned by the ODS API.
#' If a value is provided for the limit argument then the value is updated. If a
#' value is not provided then the current limit value is returned.
#'
#' @param limit an integer between 1 and 1000
#'
#' @return the currently set limit, or the updated limit
#'
#' @export
#'
#' @examples
#' # get the current limit
#' ods_api_results_limit() # defaults to 1000
#'
#' # we can update this limit to a value between 1 and 1000
#' ods_api_results_limit(50) # should return 50
#' ods_api_results_limit() # should now be 50
ods_api_results_limit <- function(limit) {
  if (!missing(limit)) {
    options("ODS_API_results_limit" = limit)
  }
  getOption("ODS_API_results_limit", default = 1000)
}
