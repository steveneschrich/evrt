test_that("get choice list works", {
  expect_equal(
    get_choice_list(setNames(iris, paste("test",1:5,sep="___")), "test"),
    paste0("test___",1:5)
  )
  expect_equal(
    get_choice_list(iris, "test"), character(0)
  )
})


