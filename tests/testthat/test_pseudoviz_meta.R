context("pseudoviz meta data")

test_that("All viz in meta have an image", {
  ids <- availablePseudoviz()
  
  d <- sampleData("Cat-Num")
  whichPseudoviz(d)
  
  printPseudoviz("bar_hor")
  printPseudoviz(c("bar_hor","treemap"))
  
})


