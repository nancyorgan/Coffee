
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

beanbox = function(url){
    webpage <- read_html(url)
    name = webpage %>% html_nodes(css = ".card-body") %>% html_nodes(xpath = "h5") %>% html_text()
    notes = webpage %>% html_nodes(css = ".card-subtitle") %>% html_text()
    clickthrough = webpage %>% html_nodes(css = ".bb-category-cell a") %>% html_attr("href")
    
    return(data.frame(webpage, name, notes))
    
}
beanbox(url)