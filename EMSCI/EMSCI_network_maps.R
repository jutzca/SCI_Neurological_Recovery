####Create map of EMSCI
##Created by C.Jutzeler, 21.1.2020
###

rm(list = ls())


library(ggplot2)
library(grid)
library(rworldmap)

# Get the world map
worldMap <- getMap()

# Member States of the European Union
europeanUnion <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                   "Czech Rep.","Denmark","Estonia","Finland","France",
                   "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                   "Lithuania","Luxembourg","Malta","Netherlands","Poland",
                   "Portugal","Romania","Slovakia","Slovenia","Spain",
                   "Sweden","United Kingdom", "Switzerland")


# Select only the index of states member of the E.U.
indEU <- which(worldMap$NAME%in%europeanUnion)


# Extract longitude and latitude border's coordinates of members states of E.U. 
europeCoords <- lapply(indEU, function(i){
  df <- data.frame(worldMap@polygons[[i]]@Polygons[[1]]@coords)
  df$region =as.character(worldMap$NAME[i])
  colnames(df) <- list("long", "lat", "region")
  return(df)
})

europeCoords <- do.call("rbind", europeCoords)


# Add some data for each member

emsci<- read.csv("/Volumes/jutzelec$/8_Projects/1_Ongoing/9_EMSCI_epidemiological_shift/2_Data/emsci_data_2020.csv", sep = ',', header = T)

emsci_filtered <- subset(emsci, ExamStage=='acute I' &  AgeAtDOI >= 0 & (!(is.na( Sex))) & YEARDOI ==2002)

emsci_filtered <- subset(emsci, ExamStage=='acute I' &  AgeAtDOI >= 0 & (!(is.na( Sex))))

library(dplyr)

value <-emsci_filtered%>%
  count(Country)

value <- c(4,NA,NA,NA,NA,
           6,NA,NA,NA,2,
                   65,NA,NA,NA,1,NA,
                   NA,NA,NA,4,NA,
                   NA,NA,NA,NA,6,
                   NA,10, 1)

value_nr <- c(190,NA,NA,NA,NA,
           316,NA,NA,NA,74,
           3354,NA,NA,NA,33,NA,
           NA,NA,NA,176,NA,
           NA,NA,NA,NA,294,
           NA,486, 33)

europeanUnionTable <- merge(europeanUnion, value,by = "row.names", all = TRUE)

# Add some data for each member
europeanUnionTable <- data.frame(country = europeanUnion, value = value)
europeCoords$value <- europeanUnionTable$value[match(europeCoords$region,europeanUnionTable$country)]


library(viridis)

# Plot the map
P <- ggplot() + geom_polygon(data = europeCoords, aes(x = long, y = lat, group = region, fill = value),
                             colour = "black", size = 0.1) +
  coord_map(xlim = c(-13, 35),  ylim = c(32, 71))+scale_fill_viridis(option = 'viridis', direction=-1, na.value = "lightgrey", limits=c(0,100))+
  theme(panel.grid.minor = element_line(colour = NA),panel.grid.major = element_line(colour = NA),
  panel.background = element_rect(fill = NA, colour = NA),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(), axis.ticks.x = element_blank(),
  axis.ticks.y = element_blank(), axis.title = element_blank(),
  #rect = element_blank(),
  plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines"))

P


