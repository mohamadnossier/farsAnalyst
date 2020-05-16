context("Just testing making filename functionality")

test_that("Whether the make filename functions gives the correct output", {

  set.seed(1)
  res = make_filename(2014)

  expect_equal(res, "accident_2014.csv.bz2")
})
