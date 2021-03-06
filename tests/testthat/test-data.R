context("data")

test_that("trope_data", {
  urls <- c("http://tvtropes.org/pmwiki/pmwiki.php/Main/SenseiChan",
             "http://tvtropes.org/pmwiki/pmwiki.php/Main/YouAreBetterThanYouThinkYouAre")
  res <- trope_urls(urls)
  expect_true(length(res) == 2)

  urls <- c("http://tvtropes.org/pmwiki/pmwiki.php/Creator/GailSimone",
            "http://tvtropes.org/pmwiki/pmwiki.php/Main/YouAreBetterThanYouThinkYouAre")
  res <- trope_urls(urls, filter_pattern = "/Main/")
  expect_true(res[1] == urls[2])
})

test_that("trope_data", {
  .urls <- c("https://tvtropes.org/pmwiki/pmwiki.php/Main/SenseiChan",
             "https://tvtropes.org/pmwiki/pmwiki.php/Main/YouAreBetterThanYouThinkYouAre")
  res <- trope_data(.urls)
  expect_true(inherits(res, "data.frame"))
})

test_that("trope_redirect_to", {
  .urls <- c("http://tvtropes.org/pmwiki/pmwiki.php/Main/SenseiChan",
             "http://tvtropes.org/pmwiki/pmwiki.php/Main/YouAreBetterThanYouThinkYouAre")
  res <- trope_redirect_to(.urls)
  expect_true(inherits(res, "data.frame"))

  .urls <- c("http://tvtropes.org/pmwiki/pmwiki.php/Main/SenseiChan",
             "http://tvtropes.org/pmwiki/pmwiki.php/Main/ThisIsInvalidTroprTestURL")
  res <- trope_redirect_to(.urls)
  expect_true(inherits(res, "data.frame"))
})

test_that("trope_cache", {
  urls <- "https://tvtropes.org/pmwiki/pmwiki.php/Main/SenseiChan"
  res <- trope_cache(urls, depth = 1, verbose = F)
  expect_true(inherits(res, "data.frame"))

  # from here tricky cases will be tested
  urls <- "http://tvtropes.org/pmwiki/pmwiki.php/Main/Recursion"
  expect_error(trope_cache(urls, depth = 1, verbose = F))

  urls <- "http://tvtropes.org/pmwiki/pmwiki.php/Main/Contributors"
  res <- trope_cache(urls, depth = 1, verbose = F)
  expect_true(inherits(res, "data.frame"))
})
