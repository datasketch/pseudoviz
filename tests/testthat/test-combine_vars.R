
test_that("combine vars work", {
 
  dic <- data.frame(id = c("first"),
                    hdtype = c("Num"))
  
  make_combinations(dic$id, 1, colname = "id_")
  
  combine_vars(dic)
   
  dic <- data.frame(id = c("first", "second"),
                    hdtype = c("Cat", "Num"))
  combine_vars(dic)
  
  dic <- data.frame(id = c("first", "second", "third"),
                    hdtype = c("Num", "Cat", "Num"))
  combine_vars(dic)
  
  
  dic <- data.frame(id = c("first", "second", "third", "fourth"),
                    hdtype = c("Dat", "Num", "Cat", "Num"))
  combine_vars(dic)
  
  
  
})


