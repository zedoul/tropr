context("tropr")

test_that("trope_node, trope_content", {
  .url <- "http://tvtropes.org/pmwiki/pmwiki.php/Main/SenseiChan"
  node <- trope_node(.url)
  expect_true(inherits(node, "xml_nodeset"))

  content <- trope_content(node)
  expect_true(inherits(content, "data.frame"))
})

