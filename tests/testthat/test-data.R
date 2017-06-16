context("data")

test_that("trope_data", {
  urls <- c("http://tvtropes.org/pmwiki/pmwiki.php/Main/SenseiChan",
             "http://tvtropes.org/pmwiki/pmwiki.php/Main/YouAreBetterThanYouThinkYouAre")
  res <- trope_urls(urls)
  expect_true(inherits(res, "data.frame"))

  urls <- c("http://tvtropes.org/pmwiki/pmwiki.php/Creator/GailSimone",
            "http://tvtropes.org/pmwiki/pmwiki.php/Main/YouAreBetterThanYouThinkYouAre")
  res <- trope_urls(urls, filter_pattern = "/Main/")
  expect_true(res[1] == urls[2])
})


test_that("trope_data", {
  .urls <- c("http://tvtropes.org/pmwiki/pmwiki.php/Main/SenseiChan",
             "http://tvtropes.org/pmwiki/pmwiki.php/Main/YouAreBetterThanYouThinkYouAre")
  res <- trope_data(.urls)
  expect_true(inherits(res, "data.frame"))
})

test_that("trope_redirect_to", {
  .urls <- c("http://tvtropes.org/pmwiki/pmwiki.php/Main/SenseiChan",
             "http://tvtropes.org/pmwiki/pmwiki.php/Main/YouAreBetterThanYouThinkYouAre")
  res <- trope_redirect_to(.urls)
  expect_true(inherits(res, "data.frame"))
})

test_that("trope_cache", {
  urls <- "http://tvtropes.org/pmwiki/pmwiki.php/Main/SenseiChan"
  res <- trope_cache(urls, depth = 1, verbose = F)
  expect_true(inherits(res, "data.frame"))

  # tricky case
  urls <- "http://tvtropes.org/pmwiki/pmwiki.php/Main/SenseiChan"
  res <- trope_cache(urls, depth = 1, verbose = F)
  expect_true(inherits(res, "data.frame"))
})
