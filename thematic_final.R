# Essay Project: Thematic Map of Ebola confirmed Cases in Sierra Leone by district between 5-18 December 2015 
#Author:Isaak Karagiannidis 


# Load the necessary libraries
library(rgdal)
library(ggplot2)
library(dplyr)
library(classInt)
library(RColorBrewer)
library(maptools)
library(raster)


# https://www.opendatasl.gov.sl/dataset/ebola-virus-disease-sierra-leone
# https://data.humdata.org/dataset/5cbe4556-0f62-43fe-89a9-2a498ed00cbc
# https://gadm.org/download_country.html

# load the csv file 
ebola<- read.csv("Confirmed EVD CasesSL.csv", header = T)
View(ebola)
colnames(ebola)

# change column name and select the columns
ebola <- ebola %>%  
  rename( id = "District") %>% 
  select( "id", "Male.Case", "Female.Cases", "Total.Cases")

# check the data
summary(ebola)

#check the classes of the data columns
sapply(ebola, class)

getwd()

list.files("/home/noob/Desktop/Msc Data Science/Spatial Data Analysis/Essay-20220318/gadm40_SLE_shp",pattern='\\.shp$')
file.exists("/home/noob/Desktop/Msc Data Science/Spatial Data Analysis/Essay-20220318/gadm40_SLE_shp")
# load the shape file of Sierra Leone map
sierra <- readOGR("/home/noob/Desktop/Msc Data Science/Spatial Data Analysis/Essay-20220318/gadm40_SLE_shp",
                  layer = "gadm40_SLE_3", stringsAsFactors = F)

sierra <- shapefile(dsn=path.expand("/home/noob/Desktop/Msc Data Science/Spatial Data Analysis/Essay-20220318/gadm40_SLE_shp"),
                    layer = "gadm40_SLE_3", stringsAsFactors = F)

# convert the map as a data frame
sierra.map <- fortify(sierra, region =  "NAME_2") # "NAME_2" Contains the regions 
colnames(sierra.map)  # check the column names
sierra.map <- select(sierra.map, "id", "long", "lat", "group") # keep the necessary columns



# combine the map with csv file
sierra.map <- sierra.map %>%
  left_join(ebola, by="id")


# create the thematic map
map.final <- ggplot(data = sierra.map, aes(x= long, y=lat, group = group))
map.final + geom_path()

map.final + geom_polygon(aes(fill =Total.Cases), color = 'gray80', size= 0.5) + coord_fixed(1) + 
  scale_fill_gradient(low = "#d9dbe3",
                      high = "#a81404",
                      space = "Lab",
                      na.value = "grey80",
                      guide = "colourbar",
                      aesthetics = "fill")



 
# create variables to spot district names on the map
lat <-c(8.2789,7.87687,8.766329,9.2476, 9.12504,9.5169,8.76609,8.7389,
        7.96472,7.52639,8.15898,7.35806,8.396807,8.4657) 
long <-c(-10.573, -11.16589,-10.89031,-12.1633,-12.91816,-11.3636,-12.78696,
         -11.7980,-11.73833,-12.505,-12.43168,-11.72083,-13.08514,-13.2317)
id <- c("Kailahun","Kenema","Kono","Bombali","Kambia","Koinadugu","Port Loko",
        "Tonkolili","Bo","Bonthe","Moyamba","Pujehun","Western Area Rural","Western Area Urban")

names.id <- data.frame(lat,long,id) # create data frame of names 



# final thematic map
map.final <- ggplot(data = sierra.map, aes(x= long, y=lat, group = group))
map.final + geom_path()

map.final + geom_polygon(aes(fill =Total.Cases), color = 'gray80', size= 0.5) + coord_fixed(1) + 
  scale_fill_gradient(low = "#d9dbe3",
                      high = "#a81404",
                      space = "Lab",
                      na.value = "grey80",
                      guide = "colourbar",
                      aesthetics = "fill") + ggtitle("Thematic Map of Ebola confirmed Cases in Sierra Leone by district") +
  geom_text(data = names.id, aes(long, lat, label=id, group=NULL), size=3, color="black")+ 
  theme(text = element_text(size = 8))

# extract id & Total.Cases columns and sorted by descending 
ebola.cases <- ebola %>% 
  select("id", "Total.Cases")
ebola.cases <- ebola.cases[order(-ebola.cases$Total.Cases),]
view(ebola.cases)
  

summary(ebola)
mean(ebola$Male.Case)
sum(ebola$Male.Case, ebola$Female.Cases)
sum(ebola$Female.Cases)
sum(ebola$Total.Cases)

mean(ebola$Male.Case) - median(ebola$Male.Case)  
mean(ebola$Female.Cases) - median(ebola$Female.Cases)


# create boxplots of each gender 
fembox <- boxplot(ebola$Female.Cases, col="green", main="Female Confirmed Cases")
fembox
malebox <- boxplot(ebola$Male.Case, col = "orange", main = "Male Confirmed Cases")
malebox


#ggplot(data = ebola) + 
 # geom_point(mapping = aes(x = Male.Case, y = Female.Cases, alpha = id))

#plot(ebola$Male.Case)
#hist(ebola$Total.Cases)

#ggplot(data = ebola)+
  geom_bar(mapping = aes(x=id, alpha = Total.Cases))+
  theme(axis.text = element_text(angle = 45))

# scatterplot   
plot(ebola$Male.Case, ebola$Female.Cases,
     xlab= "Male Cases",
     ylab="Female Cases")
title("Correlation between Male & Female")
abline(lm(ebola$Male.Case ~ ebola$Female.Cases), col="darkblue", lwd=2)


# correlation between male & female
correlation <- cor(ebola$Male.Case, ebola$Female.Cases, method = "pearson")
correlation

cor(ebola$Male.Case, ebola$Female.Cases)

summary(lm(ebola$Male.Case ~ ebola$Female.Cases, data = ebola))


# color graph 
ggplot(ebola, aes(x=id, y= Female.Cases))+
  geom_line(aes(color=id), size=2)+
  geom_point(aes(color=id), size=5)+
  theme(legend.position = "top") +
  theme(axis.text = element_text(angle = 45))
 
ggplot(ebola, aes(x=id, y= Male.Case))+
  geom_line(aes(color=id), size=2)+
  geom_point(aes(color=id), size=5)+
  theme(legend.position = "top") +
  theme(axis.text = element_text(angle = 45))

ggplot(ebola, aes(x=id, y= Total.Cases))+
  geom_line(aes(color=id), size=2)+
  geom_point(aes(color=id), size=5)+
  theme(legend.position = "top") +
  theme(axis.text = element_text(angle = 45))

# pie chart
ggplot(ebola, aes(x= Total.Cases, y = id, fill = id)) +
  geom_bar(stat = "identity", width = 1, color = "white")+
  coord_polar("y", start =0)
theme_void()

