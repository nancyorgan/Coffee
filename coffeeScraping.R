library('rvest')
library("e1071")
library("rgl")
library("misc3d")
library("data.table")
library("RImagePalette")
library("png")
library("magick")
library("jpeg")
library("dplyr")
# install_github("andreacirilloac/paletter")

# url <- "https://beanbox.com/coffee/roast/cloud-city-roasting-company-cloud-city-blend/935"
url = "https://beanbox.com/coffee/subscribe-and-refill"

######################################################################
######################################################################
beanbox = function(url){
  webpage <- read_html(url)
  name = webpage %>% html_nodes(css = ".card-body") %>% html_nodes(xpath = "h5") %>% html_text()
  notes = webpage %>% html_nodes(css = ".card-subtitle") %>% html_text()
  clickthrough = webpage %>% html_nodes(css = ".bb-category-cell a") %>% html_attr("href")
  
  return(data.frame(webpage, name, notes))
}

beanbox(url)

webpage <- read_html(url)
description = webpage %>% html_nodes(css = ".card-body")
clickthrough = webpage %>% html_nodes(css = ".bb-category-cell a") %>% html_attr("href")

beanboxData = beanbox(url$beanbox)
# write.csv(revlonData, "/Users/nancyorgan/Documents/Nail-Polish/revlonData.csv", row.names = FALSE)
#@@@gi 