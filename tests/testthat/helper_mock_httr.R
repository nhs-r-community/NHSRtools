# helper function to mock the httr environment and run some
mock_httr <- function(..., status_code = c(200), headers = NULL, content, env) {
  mocks <- list(
    "GET" = mockery::mock("data", cycle = TRUE),
    # mock expects return values to be passed as ...
    # use lift to accept a vector instead
    "status_code" = purrr:::lift_dv(mockery::mock)(status_code, cycle = TRUE),
    "headers" = mockery::mock(headers, cycle = TRUE),
    "content" = mockery::mock(content, cycle = TRUE)
  )

  do.call(with_mock, c(mocks, .env = "httr", rlang::enexprs(...)), envir = env)

  mocks
}
