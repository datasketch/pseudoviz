library(devtools)
load_all()

library(imager)
library(pystr)

#meta <- read_csv("inst/pseudoviz-meta.csv")

pvs <- meta %>% transpose()

lapply(pvs,function(pv){
  #pv <- pvs[[1]]
  img <- file.path("inst/2017-12-29",pv[["original_img"]])
  numid <- pv[["numid"]]
  i <- load.image(img)
  #plot(i)
  im <- imsub(i,y < height(i)*0.75)
  im <- resize(im,-30,-30)
  #width(im)
  #plot(im)
  output_file <- paste0("inst/imgs/",pv[["code"]],".png")
  save.image(im, output_file)
})



