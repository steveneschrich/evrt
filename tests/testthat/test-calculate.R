mydf <- tibble::tribble(
  ~var1, ~var2, ~var3, ~var4,~var5,
  "A1","B1","C1", "Yes","A3",
  "A2","B2","C2", "No","A2",
  "A3","B3","C3", "Yes","A1"
) |>
  dplyr::mutate(dplyr::across(dplyr::everything(),forcats::fct_inorder)) |>
  labelled::set_variable_labels(var1="Variable 1",var2="Variable 2",var3="Variable 3", var4="Variable 4",var5="Variable 5")

test_that("counts at threshold for one factor", {
  expect_equal(calculate_counts(mydf, vars="var1", f = count_at_or_above_threshold, threshold="A2"),
               tibble::tibble(variable="var1",label="Variable 1", count=2,n=3,percent=2/3))
  expect_equal(calculate_counts(mydf, vars="var1", f = count_at_or_above_threshold, threshold="A2"),
               calculate_counts_at_or_above_threshold(mydf, vars="var1", threshold = "A2"))
})

test_that("counts at threshold for multiple factors", {
  expect_equal(
    calculate_counts(mydf, vars=c("var1","var5"), f = count_at_or_above_threshold, threshold="A2"),
    tibble::tibble(variable=c("var1","var5"), label=c("Variable 1","Variable 5"), count=c(2,2), n=c(3,3),
                   percent=c(2/3,2/3))
  )
  expect_equal(
    dplyr::bind_rows(
      calculate_counts(mydf, vars="var1", f = count_at_or_above_threshold, threshold="A2"),
      calculate_counts(mydf, vars="var5", f=count_at_or_above_threshold, threshold = "A2")
    ),
    calculate_counts_at_or_above_threshold(mydf, vars=c("var1","var5"), threshold = "A2")
  )
  expect_equal(
    dplyr::bind_rows(
      calculate_counts(mydf, vars="var1", f = count_at_or_above_threshold, threshold="A1"),
      calculate_counts(mydf, vars="var5", f=count_at_or_above_threshold, threshold = "A1")
    ),
    calculate_counts_at_or_above_threshold(mydf, vars=c("var1","var5"), threshold = "A1")
  )
})

test_that("calculate_counts works", {
  expect_equal(calculate_counts(iris, "Sepal.Length"),
               tibble::tibble(variable="Sepal.Length", label="",count=150,n=150,percent=1))
})

test_that("calculate_counts works with group by", {
  expect_equal(calculate_counts(iris, "Sepal.Length", by = "Species")$count, c(50,50,50))
})

test_that("calculate_yes works", {
  expect_equal(calculate_counts(mydf, vars = "var4", f=count_yes)$count, 2)
  expect_equal(calculate_counts(mydf, vars = "var4", f=count_yes), calculate_counts_yesno(mydf, vars="var4"))

})
