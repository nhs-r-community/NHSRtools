# helper function to mock the httr environment and run some
mock_httr <- function(..., status_code = c(200), headers = NULL, content, env) {
  mocks <- list(
    "GET" = mock("data", cycle = TRUE),
    # mock expects return values to be passed as ...
    # use lift to accept a vector instead
    "status_code" = purrr:::lift_dv(mock)(status_code, cycle = TRUE),
    "headers" = mock(headers, cycle = TRUE),
    "content" = mock(content, cycle = TRUE)
  )

  do.call(with_mock, c(mocks, .env = "httr", enexprs(...)), envir = env)

  mocks
}
