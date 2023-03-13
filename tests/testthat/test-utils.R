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



# fct_wrap


test_that("fct_wrap doesn't change basic factors",{
  expect_equal(fct_wrap(iris$Species, width=1),iris$Species)
})
f <- paste(iris$Species, "iris",sep=" ")
test_that("fct_wrap doesn't change things it shouldn't", {
  expect_equal(fct_wrap(f,width=100), as.factor(f))
  expect_equal(fct_wrap(as.character(iris$Species), width=1), iris$Species)
})
test_that("fct_wrap works with factors", {
  expect_equal(levels(fct_wrap(as.factor(f), width=1)), c("setosa\niris","versicolor\niris","virginica\niris"))

})

test_that("fct_wrap works with strings", {
  expect_equal(levels(fct_wrap(f, width = 1)), c("setosa\niris","versicolor\niris","virginica\niris"))
})

test_that("fct_wrap works with numbers", {
  expect_equal(levels(fct_wrap(rep(c(1,2,3),n=5), width=1)), c("1","2","3"))
})


# default_labels
label_text <- c("First","Second","Third","Fourth","Fifth")
test_that("default_labels works with labels", {
  expect_equal(
    default_labels(labelled::set_variable_labels(iris, .labels=label_text)),
    setNames(label_text,colnames(iris))
  )
  expect_equal(
    default_labels(labelled::set_variable_labels(iris, .labels=label_text), vars = "Sepal.Length"),
    setNames(label_text,colnames(iris))[1]
  )
})
test_that("empty labels works", {
  expect_equal(
    default_labels(iris), setNames(colnames(iris),colnames(iris))
  )
})
