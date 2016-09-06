library(devtools)
load_all()

library(imager)

files <- "/Users/jpmarindiaz/MEGA/datasketch/pseudoviz/paper/v0"

lapply(files,function(file){
  #regmatches(file,regexpr("[0-9]+",file))
  id <- gsub("[^0-9]", "", file)
  message(file)
  file <-sysfile(file.path("paper",file))
  i <- load.image(file)
  #plot(i)
  im <- imsub(i,y < height(i)*0.75)
  im <- resize(im,-30,-30)
  width(im)
  #plot(im)
  output_file <- pystr_format("inst/tmp/{1}.png",id)
  save.image(im, output_file)
})



