library("igraph")
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
library("progress")
library("ggplot2")
library("d3r")
library("jsonlite")
library("Hmisc")
library("reticulate")
library("tidyr")

################################################################################
################ Link click function #############################################

beanbox = function(url){
    webpage <- read_html(url)
    name = webpage %>% html_nodes(css = ".card-body") %>% html_nodes(xpath = "h5") %>% html_text()
    notes = webpage %>% html_nodes(css = ".card-subtitle") %>% html_text()
    clickthrough = webpage %>% html_nodes(css = ".bb-category-cell a") %>% html_attr("href")
    
    return(data.frame(name, notes, clickthrough))
}

url = "https://beanbox.com/coffee/roast/blossom-coffee-roasters-dark-side-of-the-moon/294"
beanbox(url)

################################################################################
################ Details Function ##################################################

details = function(url){
  webpage <- read_html(url)
  elements = webpage %>%
    html_nodes(xpath ='//*[@id="bb-content"]/div[2]/div/div/div[2]/ul/li')
    # html_nodes(css = ".list-group-item") 

  taste = elements[grep("Tastes", elements)] %>%
    html_text()
  taste = gsub("Tastes: ", "", taste)
  taste = gsub("\n", "", taste)
  taste = gsub("^\\s+|\\s+$", "", taste)
  taste = tolower(taste)
  
  roast = elements[grep("roast profile", elements)] %>%
    html_text()
  roast = gsub("roast profile", "", roast)
  roast = gsub("^\\s+|\\s+$", "", roast)
  roast = tolower(roast)

  name = webpage %>%
    html_nodes(css = "h1") %>%
    html_text()
  name = gsub("\n", "", name)
  name = gsub("\\#[0-9].*", "", name)
  name = gsub("^\\s+|\\s+$", "", name)

  
  if(length(name) == 0){name = ""}
  if(length(taste) == 0){taste = ""}
  if(length(roast) == 0){roast = ""}

  return(data.frame(url, name, roast, taste))
}

details(url)

################################################################################
################ Get all Coffees ##################################################

getAllCoffees = function(max){
  total <- max
  pb <- txtProgressBar(min = 0, max = max, style = 3)
  
  mydata = NULL
  for(i in 1:max){
    setTxtProgressBar(pb, i)
    url = paste0("https://beanbox.com/", i)
    tryCatch(
      expr = {
        mydata[[i]] = details(url)
        # print(mydata[[i]])
      },
      error = function(e){
        message("*not a valid url", i)
        print(e)
      }
    )
  }
  mydata = rbindlist(mydata)  
  close(pb)
  return(mydata)
}

allCoffees = getAllCoffees(1090)
allCoffees
dim(allCoffees)
allCoffees$taste = as.character(allCoffees$taste)
allCoffees$roast = as.character(allCoffees$roast)
allCoffees$origin = as.character(allCoffees$origin)
allCoffees = allCoffees[allCoffees$taste != "",]
names(allCoffees) = c("url", "name", "roast", "notes")
write.csv(allCoffees, "/Users/nancyorgan/Documents/Coffee/coffeeQueriable.csv", row.names = FALSE)

################################################################################
################# Reformat All Coffees #########################################
allCoffees = read.csv("/Users/nancyorgan/Documents/Coffee/coffeedata.csv")

allCoffees2 = 
  allCoffees %>%
  group_by(url) %>%
  mutate(notes = tolower(paste(taste, collapse = " "))) %>%
  select(url, roast, origin, notes) %>%
  unique() 

# write.csv(allCoffees2, "/Users/nancyorgan/Documents/Coffee/coffeeQueriable.csv", row.names = FALSE)
################################################################################
################# Make linkages ################################################
# linkdat = list()
# newdat = list()
# for(i in 1:length(unique(allCoffees$url))){
#     current = allCoffees[allCoffees$url == unique(allCoffees$url)[i],]
#         current$source[1] = as.character(current$taste)[1]
#         current$target[1]   = as.character(current$taste)[2]
#         current$id[1] = as.character(current$url)[1]
#         current$group[1] = as.character(current$url)[1]
#         
#     if(length(current$taste) > 1){
#         current$source[2] = as.character(current$taste)[2]
#         current$target[2]   = as.character(current$taste)[3]
#         current$id[1] = as.character(current$url)[1]
#         current$group[1] = as.character(current$url)[1]
#     }
#     if(length(current$taste) > 2){
#         current$source[3] = as.character(current$taste)[3]
#         current$target[3]   = as.character(current$taste)[1]
#         current$id[1] = as.character(current$url)[1]
#         current$group[1] = as.character(current$url)[1]
#     }
#         newdat[[i]] = current
#     # nodedat[[i]] = current
# }

# newdat = rbindlist(newdat) 
# newdat = newdat %>%
#   filter(!is.na(target))

# links = newdat[,c("source", "target", "group")]
# nodes = newdat[,c("id", "group")]
# # Transform it in a graph format
# network=graph_from_data_frame(d=links, vertices=nodes, directed=F)
# plot(network)
# 
# data_json <- d3_igraph(network)
# # write(data_json, "/Users/nancyorgan/Documents/Coffee/data.json")
# 
# data = data.frame(sort(table(allCoffees$taste)))
# ggplot(data, aes(x = Var1,y = Freq)) + 
#   geom_point()

################################################################################
################# Scrape Lexicon ###############################################
url = "https://onlinelibrary.wiley.com/doi/full/10.1111/1750-3841.13555"

lexicon = function(url){
  webpage <- read_html(url)

  col1= webpage %>% 
    html_nodes(xpath = '//*[@id="jfds13555-tbl-0004"]/div[1]/table/tbody//td[1]') %>%
    html_text() 
  col1 = gsub("\\*.*", "", col1)

  col2 = webpage %>% 
    html_nodes(xpath = '//*[@id="jfds13555-tbl-0004"]/div[1]/table/tbody//td[2]') %>%
    html_text()
  col2 = gsub("\\*.*", "", col2)
  
  col3 = webpage %>% 
    html_nodes(xpath = '//*[@id="jfds13555-tbl-0004"]/div[1]/table/tbody//td[3]') %>%
    html_text()
  col3 = gsub("\\*.*", "", col3)

  lexicon = data.frame(
    col1, 
    col2,
    col3
  )
  
  return(lexicon)
  }


flavors = lexicon("https://onlinelibrary.wiley.com/doi/full/10.1111/1750-3841.13555")
flavors$col1[flavors$col1 == ""] = NA
flavors$col2[flavors$col2 == ""] = NA
flavors$col3[flavors$col3 == ""] = NA
# flavors$col3[!is.na(flavors$col1)] = 1

flavorsFilled = tibble(flavors) %>%
  fill(col1) %>%
  fill(col2)

flavorsFilled = cbind("flavors", flavorsFilled)
# flavorsFilled$value = 1

# names(flavorsFilled)[names(flavorsFilled) == "col3"] = "name"
# out = split(flavorsFilled, flavorsFilled$col1)
# out = lapply(out, function(x){ return(x[,c(2,3)])})
# out = lapply(out, function(x){ return(split(x, x$col2))})
# for(i in 1:length(out)){
#   out[[i]] = lapply(out[[i]], function(x){ return(x[,c(2)])})
# }
# 
prettify(toJSON(flavorsFilled))

toJSON(flavorsFilled, pretty = TRUE)
write.csv(flavorsFilled, "/Users/nancyorgan/Documents/Coffee/flavors.csv", row.names = FALSE)
################################################################################
################# Get Unique Notes #############################################
scrapedFlavors = unique(allCoffees$taste)
scrapedFlavors

lexiconFlavors = unique(c(flavors$col1, flavors$col2, flavors$col3))
sort(lexiconFlavors)

sort(scrapedFlavors[scrapedFlavors %nin% lexiconFlavors])

#  [1] "Apricot"           "Banana"            "Bergamot"          "Black Cherry"     
#  [5] "Black Currant"     "Black Tea"         "Brown Sugar"       "Brownie"          
#  [9] "Butterscotch"      "Cacao"             "Caramel"           "Cedar"            
# [13] "Champagne"         "Chestnut"          "Citrus"            "Cola"             
# [17] "Cookie"            "Cookie Dough"      "Cranberry"         "Cream"            
# [21] "Currant"           "Dark Chocolate"    "Date"              "Dried Fruit"      
# [25] "Fig"               "Fruit"             "Fudge"             "Ginger"           
# [29] "Graham Cracker"    "Guava"             "Herbal"            "Honeydew"         
# [33] "Huckleberry"       "Kiwi"              "Kumquat"           "Lavender"         
# [37] "Licorice"          "Mango"             "Maple"             "Maple Syrup"      
# [41] "Marionberry"       "Marmalade"         "Marshmallow"       "Marzipan"         
# [45] "Melon"             "Meyer Lemon"       "Milk Chocolate"    "Mint"             
# [49] "Mountain Apple"    "Nectarine"         "Nougat"            "Oak"              
# [53] "Orange Blossom"    "Papaya"            "Passion Fruit"     "Pecan"            
# [57] "Plum"              "Praline"           "Red Grape"         "Red Wine"         
# [61] "Rhubarb"           "Roasted Chestnuts" "Sage"              "Shortbread"       
# [65] "Spice"             "Stone Fruit"       "Tamarind"          "Toast"            
# [69] "Toffee"            "Tomato"            "Tropical"          "Tropical Fruit"   
# [73] "Walnuts"           "Watermelon"        "Wine"             

write.csv(flavorsFilled, "/Users/nancyorgan/Documents/Coffee/flavors.csv", row.names = FALSE)

