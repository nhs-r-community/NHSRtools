library(mockery)
library(rlang)
# helper function to mock the httr environment
mock_ods_get_roles <- function(..., status_code = c(200)) {
  env <- parent.frame()

  env$sample_data <- tibble::tribble(
    ~id, ~code, ~display_name, ~primary_role,
    "1", "A",  "AAA",         "true",
    "2", "B",  "BBB",         "false"
  )

  mock_httr(...,
            status_code = status_code,
            headers = list("x-total-count" = nrow(env$sample_data)),
            content = list(Roles = env$sample_data),
            env = env)
}

test_that("it returns the data from the API call", {
  mock_ods_get_roles({
    expect_equal(ods_get_roles(), sample_data)
  })
})

test_that("it calls dplyr::bind_rows", {
  m <- mock()
  stub(ods_get_roles, "janitor::clean_names", identity)
  with_mock("bind_rows" = m, .env = "dplyr",
            mock_ods_get_roles({
              ods_get_roles()
            }))

  expect_called(m, 1)
  expect_call(m, 1, dplyr::bind_rows(httr::content(res)[["Roles"]]))
  expect_args(m, 1, sample_data)
})

test_that("it calls janitor::clean_names", {
  m <- mock()
  stub(ods_get_roles, "dplyr::bind_rows", "binded_rows")
  with_mock("clean_names" = m, .env = "janitor",
            mock_ods_get_roles({
              ods_get_roles()
            }))

  expect_called(m, 1)
  expect_call(m, 1, janitor::clean_names(roles))
  expect_args(m, 1, "binded_rows")
})

test_that("it calls httr::GET and queries the correct API", {
  m <- mock_ods_get_roles(ods_get_roles())

  expected_url <- paste0(ODS_API_ENDPOINT, "roles")

  expect_called(m$GET, 1)
  expect_call(m$GET, 1, httr::GET(url))
  expect_args(m$GET, 1, expected_url)
})

test_that("it calls httr::content", {
  m <- mock_ods_get_roles(ods_get_roles())

  expect_called(m$content, 1)
  expect_call(m$content, 1, httr::content(res))
  expect_args(m$content, 1, "data")
})

test_that("it calls httr:status_code and it stops if the API call fails", {
  m <- mock_ods_get_roles(status_code = c(200, 201, 400), {
    ods_get_roles()
    expect_error(ods_get_roles())
    expect_error(ods_get_roles())
  })

  expect_called(m$status_code, 3)
  for(i in 1:3) {
    expect_call(m$status_code, i, httr::status_code(res))
    expect_args(m$status_code, i, "data")
  }
})
