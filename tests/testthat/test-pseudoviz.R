test_that("Viz recommender", {
  
  library(homodatum)
  
  # Data frame in
  x <- iris %>% dplyr::select(Species, Sepal.Length)
  families_df <- viz_which(x)
  
  x <- sample_data("Cat-Num") 
  families <- viz_which(x)
  
  expect_equal(families, families_df)
  expect_true("bar" %in% families$family)
  
  # Fringe in
  f <- homodatum::fringe(x)
  families2 <- viz_which(f)
  expect_equal(families2, families)
  
  viz_which_family(x)
  viz_which_family(f)
  
  package <- "hgchmagic"
  viz_which_fun(x, package)
  vw <- viz_which_fun(x, package, family = c("pie","bar"))
  expect_equal(vw, c("hgch_bar_CatNum","hgch_pie_CatNum"))
  
  
  # Viz recommender funs
  
  library(hgchmagic)
  
  viz_fun_df("hgchmagic")
  
  funs <- viz_fun("hgchmagic", family = "pie", frtype = "Cat")
  expect_equal(funs, "hgch_pie_Cat")
  funs <- viz_fun("hgchmagic", family = "pie", frtype = "Cat-Num")
  expect_equal(funs, "hgch_pie_CatNum")
  funs <- viz_fun("hgchmagic", family = "pie", frtype = "Cat", with_package = T)
  expect_equal(funs, "hgchmagic::hgch_pie_Cat")
  
  funs <- viz_fun("hgchmagic", family = "xxx", with_package = T)
  expect_null(funs)
  
  
  
})
