mydf <- tibble::tribble(
  ~var1, ~var2, ~var3, ~var4,~var5,
  "A1","B1","C1", "Yes","A3",
  "A2","B2","C2", "No","A2",
  "A3","B3","C3", "Yes","A1"
) |>
  dplyr::mutate(dplyr::across(dplyr::everything(),factor)) |>
  labelled::set_variable_labels(var1="Variable 1",var2="Variable 2",var3="Variable 3", var4="Variable 4",var5="Variable 5")

my_labels <- tibble::tribble(
  ~variable, ~label,
  "var1","Variable 1",
  "var2","Variable 2",
  "var3","Variable 3",
  "var4","Variable 4",
  "var5","Variable 5"
)

test_that("all var labels can be extracted", {
  expect_equal(extract_var_labels(mydf), my_labels )
})

test_that("var labels tidy select works", {
  expect_equal(extract_var_labels(mydf, vars = dplyr::starts_with("var")), my_labels)
  expect_equal(extract_var_labels(mydf, vars = dplyr::ends_with(c("1","2"))), my_labels[1:2,])
})

test_that("var labels with explicit fields works", {
  expect_equal(extract_var_labels(mydf, vars = c("var3","var4")), my_labels[3:4,])
})

test_that("var labels work out of order", {
  expect_equal(extract_var_labels(mydf, vars = c("var4","var2")), my_labels[c(4,2),])
  expect_equal(extract_var_labels(mydf, vars = c("var5","var4","var3","var2","var1")), my_labels[5:1,])
})
