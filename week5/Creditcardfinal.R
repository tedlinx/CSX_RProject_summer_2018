# 讀取
gce <- read.csv("C:\\Users\\TEMP\\Desktop\\wd\\gc.csv",header = TRUE, sep = ",")
head(gce$Date)

# 轉換年月
library(zoo)
gce$Date <- as.Date(as.yearmon(gce$Date, "%Y/%m"))
head(gce$Date)




library(rgdal)
Taiwan = rgdal::readOGR("C:\\Users\\TEMP\\Desktop\\wd\\mapdata201805310314","COUNTY_MOI_1070516")

summary(Taiwan)
mypalette = colorNumeric( palette="viridis", domain=Taiwan, na.color="transparent")
mypalette(c(45,43))

library(dplyr)
library(leaflet)
# Basic choropleth with leaflet?
view<- leaflet(Taiwan) %>%
  addTiles() %>%
  setView( lat=23.470000, lng=120.957455 , zoom=6)
view  
