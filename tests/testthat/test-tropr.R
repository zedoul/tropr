context("tropr")

test_that("trope_node, trope_content", {
  .url <- "http://tvtropes.org/pmwiki/pmwiki.php/Main/SenseiChan"
  node <- trope_node(.url)
  expect_true(inherits(node, "xml_nodeset"))

  content <- trope_content(node)
  expect_true(inherits(content, "data.frame"))

})

test_that("Some more cases", {
  # anime page
  .url <- "http://tvtropes.org/pmwiki/pmwiki.php/Anime/GirlsUndPanzer"
  content <- trope_content(trope_node(.url))
  expect_true(inherits(content, "data.frame"))

  # character
  .url <- "http://tvtropes.org/pmwiki/pmwiki.php/Characters/GirlsUndPanzerOaraiAcademyAnglerfishTeam"
  content <- trope_content(trope_node(.url))
  expect_true(inherits(content, "data.frame"))
})

test_that("tricky cases", {
  .url <- "http://tvtropes.org/pmwiki/pmwiki.php/Main/Hikikomori"
  node <- trope_node(.url)
  content <- trope_content(node)
  expect_true(inherits(content, "data.frame"))
})


