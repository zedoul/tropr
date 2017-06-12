context("data")

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


