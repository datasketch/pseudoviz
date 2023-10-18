
test_that("vizfuns from packages", {
  
  package <- "hgchmagic"
  df <- available_package_vizfuns(package)
  expect_true(all(grepl("^hgch_", df$vizfun)))
  expect_equal(unique(df$package), package)
  
  package <- "ggmagic"
  df <- available_package_vizfuns(package)
  expect_true(all(grepl("^gg_", df$vizfun)))
  expect_equal(unique(df$package), package)
  
})

test_that("Viz recommender", {
  
  
  # Data frame in
  x <- iris |>  dplyr::select(Species, Sepal.Length)
  
  ## Find hdtable_type
  hdtable_type <- hdtable_type_guess(x)
  expect_equal(hdtable_type, "Cat-Num")
  
  ## Use hdtable_type to match with pseudoviz_vizfun_families
  pseudoviz_which(hdtable_type)
  selected_families <- pseudoviz_which_family(hdtable_type)
  selected_families
  
  ## Rank families and get default params from hdtable stats
  # TODO
  
  
  
  ## Match families and params with package funs
  package <- "hgchmagic"
  
  package_vizfun_family(package, data = x)
  package_vizfun_family(package, hdtable_type = "Cat-Num")
  
  package_vizfun(package, data = x)
  package_vizfun(package, hdtable_type = "Cat-Num")
  package_vizfun(package, data = x, with_package = TRUE)
  


  families_bar_donut <- available_package_vizfuns(data = x, family = c("bar", "donut")) 
  expect_true(all(c("bar", "donut") %in% unique(families_bar_donut$vizfun_family)))
  
  package <- "hgchmagic"
  package_vizfun(data = x, package = package)
  vw <- package_vizfun(data = x, package = package, family = c("pie","bar"))
  expect_equal(vw, c("hgch_bar_CatNum","hgch_pie_CatNum"))
  
  
  # Viz recommender funs
  
  package_vizfun(package = "hgchmagic")
  
  funs <- package_vizfun(package = "hgchmagic", family = "pie", 
                         hdtable_type = "Cat")
  expect_equal(funs, "hgch_pie_Cat")
  funs <- package_vizfun(package = "hgchmagic", family = "pie", hdtable_type = "Cat-Num")
  expect_equal(funs, "hgch_pie_CatNum")
  funs <- package_vizfun(package = "hgchmagic", family = "pie", 
                  hdtable_type = "Cat", with_package = TRUE)
  expect_equal(funs, "hgchmagic::hgch_pie_Cat")
  
  funs <- package_vizfun(package = "hgchmagic", family = "xxx", with_package = T)
  expect_null(funs)
  
  
  
})
