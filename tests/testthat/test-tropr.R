context("tropr")

test_that("trope_content, as.data.frame", {
  .url <- "http://tvtropes.org/pmwiki/pmwiki.php/Main/SenseiChan"
  content <- trope_content(.url)
  expect_true(inherits(content, "tropr.content"))

  .df <- as.data.frame(content)
  expect_true(inherits(.df, "data.frame"))

})

test_that("More cases", {
  # anime page
  .url <- "http://tvtropes.org/pmwiki/pmwiki.php/Anime/GirlsUndPanzer"
  .df <- as.data.frame(trope_content(.url))
  expect_true(inherits(.df, "data.frame"))

  # character
  .url <- "http://tvtropes.org/pmwiki/pmwiki.php/Characters/GirlsUndPanzerOaraiAcademyAnglerfishTeam"
  .df <- as.data.frame(trope_content(.url))
  expect_true(inherits(.df, "data.frame"))

  # A bit tricky page
  .url <- "http://tvtropes.org/pmwiki/pmwiki.php/Main/Hikikomori"
  .df <- as.data.frame(trope_content(.url))
  expect_true(inherits(.df, "data.frame"))

  .url <- "http://tvtropes.org/pmwiki/pmwiki.php/Main/TabletopRPG"
  content <- trope_content(.url)
  .df <- as.data.frame(content)
  expect_true(inherits(.df, "data.frame"))
})
