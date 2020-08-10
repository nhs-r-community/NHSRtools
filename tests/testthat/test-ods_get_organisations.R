library(mockery)
library(rlang)
library(magrittr)

# helper function to mock the httr environment
mock_ods_get_organisations <- function(..., status_code = c(200)) {
  env <- parent.frame()

  env$sample_data <- tibble::tribble(
    ~name, ~org_id, ~status,
    "A",   1,       "active",
    "B",   2,       "inactive"
  )

  mock_httr(...,
            status_code = status_code,
            headers = list("x-total-count" = nrow(env$sample_data)),
            content = list(Organisations = env$sample_data),
            env = env)
}

# arguments ----
test_that("name input must be a single character or NA", {
  mock_ods_get_organisations({
    expect_error({ ods_get_organisations(name = 1) },
                 "^name argument must be of type character$")
    expect_error({ ods_get_organisations(name = TRUE) },
                 "^name argument must be of type character$")
    expect_error({ ods_get_organisations(name = c("1", "2")) },
                 "^name argument must be a single value$")
    expect_error({ ods_get_organisations(name = "") },
                 "^name argument must not be an empty string$")

    expect_equal(ods_get_organisations(name = "a"), sample_data)
  })
})

test_that("name argument gets appended to the URL", {
  m <- mock_ods_get_organisations({
    ods_get_organisations(name = "test name")
  })

  expect_called(m$GET, 1)
  expect_call(m$GET, 1, httr::GET(url))
  expect_args(m$GET, 1, paste0(ODS_API_ENDPOINT,
                               "organisations?Limit=1000&Name=test%20name"))
})

test_that("post_code argument must be a valid value", {
  mock_ods_get_organisations({
    expect_error({ ods_get_organisations(post_code = 1) },
                 "^post_code argument must be of type character$")
    expect_error({ ods_get_organisations(post_code = "AA") },
                 "^post_code argument must be at least a district, e.g. AA1$")
    expect_error({ ods_get_organisations(post_code = c("AA1", "AA2")) },
                 "^post_code argument must be a single value$")
    expect_error({ ods_get_organisations(post_code = "") },
                 "^post_code argument must not be an empty string$")

    expect_equal(ods_get_organisations(post_code = "AA1"), sample_data)
  })
})

test_that("post_code argument gets appended to the URL", {
  m <- mock_ods_get_organisations({
    ods_get_organisations(post_code = "AA1")
  })

  expect_called(m$GET, 1)
  expect_call(m$GET, 1, httr::GET(url))
  expect_args(m$GET, 1, paste0(ODS_API_ENDPOINT,
                               "organisations?Limit=1000&PostCode=AA1"))
})

test_that("last_change_date argument must be a valid value", {
  mock_ods_get_organisations({
    expect_error({ ods_get_organisations(last_change_date = "2020-01-01") },
                 "^last_change_date argument must be of type date$")
    expect_error({ ods_get_organisations(last_change_date = 20200101) },
                 "^last_change_date argument must be of type date$")
    expect_error({ ods_get_organisations(last_change_date = c(as.Date("2020-01-01"),
                                                              as.Date("2020-01-02"))) },
                 "^last_change_date argument must be a single value$")

    expect_equal(ods_get_organisations(last_change_date = as.Date("2020-01-01")),
                 sample_data)
  })
})

test_that("last_change_date argument gets appended to the URL", {
  m <- mock_ods_get_organisations({
    ods_get_organisations(last_change_date = as.Date("2020-01-01"))
  })

  expect_called(m$GET, 1)
  expect_call(m$GET, 1, httr::GET(url))
  expect_args(m$GET, 1, paste0(ODS_API_ENDPOINT,
                               "organisations?Limit=1000&LastChangeDate=2020-01-01"))
})

test_that("status argument must be a valid value", {
  mock_ods_get_organisations({
    expect_error({ ods_get_organisations(status = "") },
                 "^'arg' should be one of \"NA\", \"active\", \"inactive\"$")
    expect_error({ ods_get_organisations(status = "other") },
                 "^'arg' should be one of \"NA\", \"active\", \"inactive\"$")
    expect_error({ ods_get_organisations(status = 1) },
                 "^'arg' must be NULL or a character vector$")
    expect_error({ ods_get_organisations(status = c("active", "inactive")) },
                 "^'arg' must be of length 1$")

    expect_equal(ods_get_organisations(status = "active"), sample_data)
    expect_equal(ods_get_organisations(status = "inactive"), sample_data)
  })
})

test_that("status argument gets appended to the URL", {
  m <- mock_ods_get_organisations({
    ods_get_organisations(status = "active")
  })

  expect_called(m$GET, 1)
  expect_call(m$GET, 1, httr::GET(url))
  expect_args(m$GET, 1, paste0(ODS_API_ENDPOINT,
                               "organisations?Limit=1000&Status=active"))
})

test_that("primary_role_id argument must be a valid value", {
  mock_ods_get_organisations({
    expect_error({ ods_get_organisations(primary_role_id = 1) },
                 "^primary_role_id argument must be of type character$")
    expect_error({ ods_get_organisations(primary_role_id = c("a", "b")) },
                 "^primary_role_id argument must be a single value$")
    expect_error({ ods_get_organisations(primary_role_id = "") },
                 "^primary_role_id argument must not be an empty string$")

    expect_equal(ods_get_organisations(primary_role_id = "abc"), sample_data)
  })
})

test_that("primary_role_id argument gets appended to the URL", {
  m <- mock_ods_get_organisations({
    ods_get_organisations(primary_role_id = "abc")
  })

  expect_called(m$GET, 1)
  expect_call(m$GET, 1, httr::GET(url))
  expect_args(m$GET, 1, paste0(ODS_API_ENDPOINT,
                               "organisations?Limit=1000&PrimaryRoleId=abc"))
})

test_that("non_primary_role_id argument must be a valid value", {
  mock_ods_get_organisations({
    expect_error({ ods_get_organisations(non_primary_role_id = 1) },
                 "^non_primary_role_id argument must be of type character$")
    expect_error({ ods_get_organisations(non_primary_role_id = c("a", "b")) },
                 "^non_primary_role_id argument must be a single value$")
    expect_error({ ods_get_organisations(non_primary_role_id = "") },
                 "^non_primary_role_id argument must not be an empty string$")

    expect_equal(ods_get_organisations(non_primary_role_id = "abc"), sample_data)
  })
})

test_that("non_primary_role_id argument gets appended to the URL", {
  m <- mock_ods_get_organisations({
    ods_get_organisations(non_primary_role_id = "abc")
  })

  expect_called(m$GET, 1)
  expect_call(m$GET, 1, httr::GET(url))
  expect_args(m$GET, 1, paste0(ODS_API_ENDPOINT,
                               "organisations?Limit=1000&NonPrimaryRoleId=abc"))
})

test_that("it defaults to NHS trust if no arguments are passed", {
  m <- mock_ods_get_organisations({
    expect_warning({ ods_get_organisations() },
                   "^No arguments specified: defaulting to Primary Role == NHS TRUST$")
  })

  expect_called(m$GET, 1)
  expect_call(m$GET, 1, httr::GET(url))
  expect_args(m$GET, 1, paste0(ODS_API_ENDPOINT,
                               "organisations?Limit=1000&PrimaryRoleId=RO197"))
})

# test we call the rest of the httr stuff correctly ----
test_that("it calls httr::content", {
  m <- mock_ods_get_organisations(ods_get_organisations(name = "A"))

  expect_called(m$content, 1)
  expect_call(m$content, 1, httr::content(res))
  expect_args(m$content, 1, "data")
})

test_that("it calls httr:status_code and it stops if the API call fails", {
  m <- mock_ods_get_organisations(status_code = c(200, 201, 400), {
    ods_get_organisations(name = "A")
    expect_error(ods_get_organisations(name = "A"))
    expect_error(ods_get_organisations(name = "A"))
  })

  expect_called(m$status_code, 3)
  for(i in 1:3) {
    expect_call(m$status_code, i, httr::status_code(res))
    expect_args(m$status_code, i, "data")
  }
})

test_that("it pages if there are more than LIMIT results", {
  # LIMIT is set to 1000, the current max supported by the API
  sample_data <- tibble::tribble(
    ~name, ~org_id, ~status,
    "A",   1,       "active",
    "B",   2,       "inactive"
  ) %>%
    dplyr::mutate(rep = list(1:750)) %>%
    tidyr::unnest_longer(col = rep)

  m <- list(
    "GET" = mock("data", cycle = TRUE),
    "status_code" = mock(200, cycle = TRUE),
    "headers" = mock(
      list("x-total-count" = nrow(sample_data))
    ),
    "content" = mock(
      list("Organisations" = sample_data %>% dplyr::filter(rep <= 500)),
      list("Organisations" = sample_data %>% dplyr::filter(rep >  500))
    )
  )
  do.call(with_mock, c(m, .env = "httr", expr({
    ods_get_organisations(name = "A")
  })))

  expect_called(m$GET, 2)
  expect_called(m$status_code, 2)
  expect_called(m$headers, 1)
  expect_called(m$content, 2)
})

test_that("it calls janitor::clean_names", {
  m <- mock()
  with_mock("clean_names" = m, .env = "janitor",
            mock_ods_get_organisations({
              ods_get_organisations(name = "a")
            }))

  expect_called(m, 1)
  expect_call(m, 1, janitor::clean_names(organisations))
  expect_args(m, 1, sample_data)
})

# test helper functions ----

test_that("ods_get_trusts calls ods_get_organisations correctly", {
  expect_equal(1, 1)
  m <- mock()

  with_mock(ods_get_organisations = m, {
    ods_get_trusts()
  })

  expect_called(m, 1)
  expect_call(m, 1, ods_get_organisations(name,
                                          post_code,
                                          last_change_date,
                                          status,
                                          primary_role_id = "RO197",
                                          non_primary_role_id,
                                          org_record_class))
})

test_that("ods_get_trust_sites calls ods_get_organisations correctly", {
  m <- mock()

  with_mock(ods_get_organisations = m, {
    ods_get_trust_sites()
  })

  expect_called(m, 1)
  expect_call(m, 1, ods_get_organisations(name,
                                          post_code,
                                          last_change_date,
                                          status,
                                          primary_role_id = "RO198",
                                          non_primary_role_id,
                                          org_record_class))
})

test_that("ods_get_ccgs calls ods_get_organisations correctly", {
  m <- mock()

  with_mock(ods_get_organisations = m, {
    ods_get_ccgs()
  })

  expect_called(m, 1)
  expect_call(m,  1, ods_get_organisations(name,
                                           post_code,
                                           last_change_date,
                                           status,
                                           primary_role_id = "RO98",
                                           non_primary_role_id,
                                           org_record_class))
})

