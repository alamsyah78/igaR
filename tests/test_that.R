test_that("read_regulation reads files correctly", {
  expect_is(read_regulation("C:/Users/ThinkPad P52s/Documents/igaR/example.txt"), "character")
})
