##### Code to analyze create the map of patients enrolled per country.

#Clear workspace
rm(list = ls())

#where libraries are stored
.libPaths()

#The following commands will install these packages if they are not already installed:

#if(!require(ggplot2)){install.packages("ggplot2")}
#if(!require(ggridges)){install.packages("ggridges")}
#if(!require(ggpubr)){install.packages("ggpubr")}
#if(!require(plyr)){install.packages("plyr")}
#if(!require(dplyr)){install.packages("dplyr")}
#if(!require(gridExtra)){install.packages("gridExtra")}
#if(!require(maps)){install.packages("maps")}
#if(!require(viridis)){install.packages("viridis")}

#List of libraries required for the analyses below
library(ggplot2)
library(dplyr)
require(maps)
require(viridis)
theme_set(
  theme_void()
)


#----Create map figure for Europe----
# Some EU Contries
some.eu.countries <- c(
  "Portugal", "Spain", "France", "Switzerland", "Germany",
  "Austria", "Belgium", "UK", "Netherlands",
  "Denmark", "Poland", "Italy", 
  "Croatia", "Slovenia", "Hungary", "Slovakia",
  "Czech republic"
)

# Retrievethe map data for EU countries
some.eu.maps <- map_data("world", region = some.eu.countries)

# Compute the centroid as the mean longitude and lattitude
# Used as label coordinate for country's names
region.lab.data <- some.eu.maps %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))
region.lab.data

#Create dataframe with region name, relative prevalence in %, absolute prevalence, and three letter code for each EMSCI country
prevalence_sci <- data.frame("region" = c( "Portugal", "Spain", "France", "Switzerland", "Germany",
                                           "Austria", "Belgium", "UK", "Netherlands",
                                           "Denmark", "Poland", "Italy", 
                                           "Croatia", "Slovenia", "Hungary", "Slovakia",
                                           "Czech Republic"), "prevalence" =c(NA, 6.3, 1.0, 9.8,64.1,
                                                                           3.0, NA, 0.7, 3.7,
                                                                           NA,NA, 3.7, 
                                                                           NA, NA, NA, NA,
                                                                           6.5), "prevalence_nr" =c(NA, 289, 46, 451,2949,
                                                                                                 138, NA, 31, 170,
                                                                                                 NA,NA, 168, 
                                                                                                 NA, NA, NA, NA,
                                                                                                 297),
                              "Country_name" =c(NA, "ESP", "FRA", "CHE", "DEU",
                                            "AUT", NA, "GBR", "NLD",
                                            NA, NA, "ITA", 
                                            NA, NA, NA, NA,
                                            "CZE"))

###Merge data frames using left_join command
prevalence_sci_region.lab.data <-left_join(prevalence_sci, region.lab.data, by = "region")

prevalence_sci_map <- left_join(prevalence_sci, some.eu.maps, by = "region")

#Plot map and save as pdf
emsci_eu<-ggplot(prevalence_sci_map, aes(x = long, y = lat)) +
  geom_polygon(aes( group = group, fill = prevalence))+
  theme_economist(horizontal = FALSE)+
  theme(legend.position = "right",
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())+
  geom_text(aes(label = prevalence_nr), data = prevalence_sci_region.lab.data,  size = 4, hjust = 0.5, vjust=0.3, color='white')+
  geom_text(aes(label = Country_name), data = prevalence_sci_region.lab.data,  size = 4, hjust = 0.5, vjust=-0.9, color='white')+ 
  scale_fill_gradient2(low = "darkblue", mid = "yellow", high ="darkorange", 
                       midpoint = 30, space = "rgb", guide = "colourbar", na.value="#b0aeae")
  

emsci_eu

ggsave(
  "emsci_eu.pdf",
  plot = emsci_eu,
  device = 'pdf',
  path = '/Users/jutzca/Desktop/Figures',
  scale = 1,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()

#----Create map figure for India----
# Some EU Contries
india <- c(
  "India")


# Retrievethe map data for EU countries
india.map <- map_data("world", region = india)

# Compute the centroid as the mean longitude and lattitude
# Used as label coordinate for country's names
india.lab.data <- india.map %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))
india.lab.data

#Create dataframe with region name, relative prevalence in %, absolute prevalence, and three letter code for each EMSCI country
prevalence_sci.india <- data.frame("region" = c( "India"), "prevalence" =c(1.3), "prevalence_nr" =c(62),
                             "Country_name" =c("IND"))

###Merge data frames using left_join command
prevalence_sci_region.lab.data_india <-left_join(prevalence_sci.india, india.lab.data, by = "region")

prevalence_sci_map_india <- left_join(prevalence_sci.india, india.map, by = "region")

#Plot map and save as pdf
emsci_india<-ggplot(prevalence_sci_map_india, aes(x = long, y = lat)) +
  geom_polygon(aes( group = group, fill = prevalence))+
  theme_economist(horizontal = FALSE)+
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())+
  geom_text(aes(label = prevalence_nr), data = prevalence_sci_region.lab.data_india,  size = 4, hjust = 0.5, vjust=0.3, color='white')+
  geom_text(aes(label = Country_name), data = prevalence_sci_region.lab.data_india,  size = 4, hjust = 0.5, vjust=-0.9, color='white')


emsci_india

ggsave(
  "emsci_india.pdf",
  plot = emsci_india,
  device = 'pdf',
  path = '/Users/jutzca/Desktop/Figures',
  scale = 1,
  width = 5,
  height = 4,
  units = "in",
  dpi = 300
)

dev.off()




