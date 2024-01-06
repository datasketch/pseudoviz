
test_that("combine vars work", {
 
  dic <- data.frame(id = c("first"),
                    hdtype = c("Num"))
  
  combinations <- make_combinations(dic$id, 1, colname = "id_")
  expect_equal(nrow(combinations), 1)
  
  var_comb <- combine_vars(dic)
  expect_equal(nrow(var_comb), 1)
  expect_equal(var_comb$hdtable_type, "Num")
  
  dic <- data.frame(id = c("first", "second"),
                    hdtype = c("Cat", "Num"))
  var_comb <- combine_vars(dic)
  expect_equal(var_comb$hdtable_type[3], "Cat-Num")
  
  dic <- data.frame(id = c("first", "second", "third"),
                    hdtype = c("Num", "Cat", "Num"))
  var_comb <- combine_vars(dic)
  expect_equal(nrow(var_comb), 7)
  expect_equal(var_comb$hdtable_type[6], "Cat-Num")
  
  dic <- data.frame(id = c("first", "second", "third", "fourth"),
                    hdtype = c("Dat", "Num", "Cat", "Num"))
  var_comb <- combine_vars(dic)
  expect_equal(nrow(var_comb), 14)
  expect_equal(var_comb$hdtable_type[14], "Cat-Num-Num")
  
  
  
})


