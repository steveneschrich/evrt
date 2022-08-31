test_that("count works",{
  expect_equal(count_at_or_above_threshold(iris$Species, threshold = "setosa"), tibble::tibble(count=150,n=150))
  expect_equal(count_at_or_above_threshold(iris$Species, threshold = "versicolor"), tibble::tibble(count=100,n=150))
  expect_equal(count_at_or_above_threshold(iris$Species, threshold = "virginica"), tibble::tibble(count=50,n=150))
})

test_that("count works with reversed order of factors", {
  expect_equal(count_at_or_above_threshold(forcats::fct_rev(iris$Species), threshold = "setosa"), tibble::tibble(count=50,n=150))
  expect_equal(count_at_or_above_threshold(forcats::fct_rev(iris$Species), threshold = "versicolor"), tibble::tibble(count=100,n=150))
  expect_equal(count_at_or_above_threshold(forcats::fct_rev(iris$Species), threshold = "virginica"), tibble::tibble(count=150,n=150))
})


test_that("non factors fail", {
  expect_error(count_at_or_above_threshold(iris$Species, threshold = "NotInIris"))
})

test_that("count_yes works", {
  expect_equal(count_yes(factor(c("Yes","No","Yes","Yes"))),tibble::tibble(count=3,n=4))
})
