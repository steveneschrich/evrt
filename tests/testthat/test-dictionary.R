label_table <- tibble::tibble(Value=c(1,2,3), Label=c("One","Two","Three"))
labels <- "1:One; 2:Two; 3:Three"
revlabels <- "3:Three; 2:Two; 1:One"
test_that("Collapse levels works", {
  expect_equal(
    collapse_levels(label_table),
    labels
  )
})


test_that("format_levels works", {
  expect_equal(
    format_levels(list(label_table,dplyr::arrange(label_table, dplyr::desc(Value)))),
    c(labels, revlabels)
  )
})

test_that("levels_as_string works with no dictionary", {
  expect_equal(
    levels_as_string(iris, vars="Species"),
    stats::setNames(c("1:setosa; 2:versicolor; 3:virginica"),"Species")
  )
  expect_equal(
    levels_as_string(cbind(iris, foo=factor(c("A","B"))), vars=c("foo","Species")),
    stats::setNames(c("1:A; 2:B","1:setosa; 2:versicolor; 3:virginica"), c("foo","Species"))
  )
})
