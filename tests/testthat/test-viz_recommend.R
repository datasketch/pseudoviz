

test_that("Viz recommen works", {
  
 
  d <- iris |>  dplyr::select(Species, Sepal.Length)
  viztypes <- viz_recommend(d)
  
  
  d <- iris
  viztypes <- viz_recommend(d, dic = NULL)
  names(viztypes)
  
  
  d <- dplyr::starwars[,1:10]
  viztypes <- viz_recommend(d, dic = NULL)
  
  
  d <- lubridate::lakers
  d$date <- lubridate::ymd(d$date)
  viztypes <- viz_recommend(d, dic = NULL)
  
  
  d <- dplyr::band_members
  viztypes <- viz_recommend(d, dic = NULL)
  

  
  
})
