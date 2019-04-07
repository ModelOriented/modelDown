context("Check parseExplainers() function")
library(useful)

createFakeExplainer <- function(name) {
  object <- list()
  object$name <- name
  class(object) <- "explainer"
  return(object)
}

old1 <- createFakeExplainer("old1")
new1 <- createFakeExplainer("new1")
old2 <- createFakeExplainer("old2")
new2 <- createFakeExplainer("new2")
old3 <- createFakeExplainer("old3")
new3 <- createFakeExplainer("new3")

test_that("Default arguments", {
  expect_true({

    explainers = list(old1, old2, old3)
    res <- parseExplainers(explainers)
    expected <- list(basic_explainers=list(old1, old2, old3), drifter_explainer_pairs=list())

    all(compare.list(res, expected))
  })
})

test_that("Mixed arguments", {
  expect_true({
    explainers = list(old1, list(old2, new2), old3)
    res <- parseExplainers(explainers)
    expected <- list(basic_explainers=list(old1, old2, old3), drifter_explainer_pairs=list(list(old2, new2)))

    all(compare.list(res, expected))
  })
})

test_that("To many explainers in vector", {
  expect_error({
    explainers = list(list(old2, new2, new3))
    parseExplainers(explainers)
  })
})
