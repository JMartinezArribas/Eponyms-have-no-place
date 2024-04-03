# Information -------------------------------------------------------------

# African Eponyms Maps
# List of species by African country retrieved from the IUCN on 01 February 2022
# Script developed by Fernanda Alves-Martins
# Last update February, 01st 2023

# Libraries --------------------------------------------------------------------
libs = c("purrr","readxl","doParallel","foreach","sp","raster","rworldmap", "rgdal",
         "dplyr","remotes","sp","fasterize","rasterSp","writexl","forcats","readxl",
         "ggplot2","ggrepel","tidyr","beepr")
lapply(libs, require, character.only = TRUE)
rm(libs)

# Analysis ------------------------------------------------------------
path = "C:/Users/Utilizador/Dropbox/TROPIBIO/Patricia/Decolonization/Data/Data/AfrSpListByCountry/"

## Import countries list
#### List of species by African country retrieved from the IUCN on 01 February 2022
africanList = list.files(
  path = path,
  pattern = "*.xls",
  full.names = TRUE,
  recursive = TRUE
) %>%
  set_names() %>%
  map_dfr(read_excel, .id = "Source")

head(africanList)
sort(unique(africanList$country))
n_distinct(africanList$scientificName)
# africanList = africanList[africanList$scientificName != "Malpolon moilensis",]

african = subset(africanList, select = c(internalTaxonId,scientificName,kingdomName,phylumName,       
                                         className,orderName,familyName,genusName,speciesName,
                                         infraType,infraName,infraAuthority,authority))
african = african[!duplicated(african$scientificName),]

### Exclude endemic species
#### Import IUCN global list - all land regions except North Africa and Sub-Saharian Africa 
#### (IUCN Search on 2022-06-08 at 10:56:19)
global = read.csv("C:/Users/Utilizador/Dropbox/TROPIBIO/Patricia/Decolonization/Data/SpeciesList/IUCNLists/GlobalTaxonomy.csv")
global = subset(global, select = c(internalTaxonId,scientificName,kingdomName,phylumName,       
                                         className,orderName,familyName,genusName,speciesName,
                                         infraType,infraName,infraAuthority,authority))

#### Search for African endemic species
length(unique(african$scientificName))
length(unique(global$scientificName))

global = global[!duplicated(global$scientificName),]
afrEndemics = setdiff(african,global) ## 6488 african endemics
length(unique(afrEndemics$scientificName))
afrEndemics = as.data.frame(afrEndemics$scientificName); colnames(afrEndemics) = "scientificName"
africanList2 = left_join(afrEndemics,africanList)
length(unique(africanList2$scientificName));length(unique(afrEndemics$scientificName))
rm(african,global)

### Eponyms of african vertebrates
dfEpo = total#organized database
# dfEpo = read.csv("C:/Users/Utilizador/Dropbox/TROPIBIO/Patricia/Decolonization/Data/Data/AfrVertEponyms.csv")
dfEpo = dfEpo[!duplicated(dfEpo$scientificName),]
length(unique(dfEpo$scientificName)); unique(dfEpo$className); table(dfEpo$EPONYM)

#### Join to eponyms database
df1 = left_join(afrEndemics,dfEpo)
df1 = df1[!is.na(df1$EPONYM),] # we excluded the species for which we did not classified eponyms (probably they were not on the first list of eponyms we retrieve from the IUCN)
summary(df1); length(unique(df1$scientificName))
rm(afrEndemics,dfEpo,africanList)

#### Input values to professions
df1$Nobility[is.na(df1$Nobility)] = 0
df1$Army[is.na(df1$Army)] = 0
df1$Academic[is.na(df1$Academic)] = 0
df1$Collector[is.na(df1$Collector)] = 0
df1$Family[is.na(df1$Family)] = 0
df1$Bankers[is.na(df1$Bankers)] = 0
df1$`Colonial administration`[is.na(df1$`Colonial administration`)] = 0
df1$Others[is.na(df1$Others)] = 0
summary(df1)

## Species occurrences per country
#### Add country name to our data 
africanList2 = africanList2[,c("scientificName","country")]
df = left_join(df1,africanList2)
length(unique(df$scientificName))
table(is.na(df$country))
sort(unique(df$country))
rm(df1,africanList2)

## Group nationalities
df$Nationality = as.factor(df$Nationality)
sort(unique(df$Nationality))
df$Nationality[df$Nationality == "portugal"] = "Portugal" 
fun <- function(z) {
  empires = c("United Kingdom","France","Germany","Portugal","Italy","Belgium",
              "United States of America","Spain","Native")
  z[!(z %in% empires)] = "Others"
  z
}
df$Nationality = fct_relabel(df$Nationality, fun)

## Summarize information by countries
### % of eponyms by countries
sort(unique(df$Nationality))
sort(table(df$Nationality))

df1 = df %>%
  group_by(country, Nationality) %>%
  summarise(sumEpoNation = sum(EPONYM))

df2 = df %>%
  group_by(country) %>%
  summarise(eponymsPerc = round((sum(EPONYM)/length(EPONYM))*100,0), 
            sumEponomy = sum(EPONYM),
            nSp = length(scientificName))

df3 = left_join(df1,df2)
df3$epoNatPerc = round(((df3$sumEpoNation/df3$sumEponomy)*100),2)
names(df3)[names(df3) == "country"] = "region"
# dfLeg = df3
df3 = df3[!is.na(df3$Nationality),]
head(df3)
# rm(df,df1,df2,fun)

## Import colonies table and join to Africa map
sort(unique(df3$region))
colony = read_excel("C:/Users/Utilizador/Dropbox/TROPIBIO/Patricia/Decolonization/Data/Data/Empires/EmpiresColonies.xlsx")
head(colony)
colony = colony[,-c(3:4)]
sort(unique(colony$region))

## Import world map in df format
world_map = map_data("world")
sort(unique(world_map$region))
colnames(colony)
colnames(world_map)
afr = left_join(colony,world_map, by = "region")
sort(unique(afr$region))
df4 = unique(df3[,c("region","eponymsPerc")])
df4$region = as.character(df4$region)
class(df4$region)
class(afr$region)
afr = left_join(afr,df4, by = "region")
head(afr)
sort(unique(afr$region))

# Figures ----------------------------------------------------------------------

## Set the output directory
output.dir = "C:/Users/Utilizador/Dropbox/TROPIBIO/Patricia/Decolonization/Results/Figures/"

## Eponyms map figure ----------------------------------------------------------
unique(afr$region[which(is.na(afr$eponymsPerc))])
region.lab.data = afr %>%
  group_by(region, eponymsPerc) %>%
  summarise(long = mean(long), lat = mean(lat))
summary(region.lab.data)
region.lab.data$eponymsPerc = as.character(region.lab.data$eponymsPerc)
region.lab.data$eponymsPerc = paste(region.lab.data$eponymsPerc,"%",sep ="")

afr$empire [is.na(afr$empire)] = "Independent"
afr$empire = as.factor(afr$empire)

X11()
ggplot() +
  geom_polygon(data = afr, aes(x = long, y = lat, group = group, fill = factor(empire)), 
               color = "white", size = 0.2, alpha = 0.5)+
  geom_text(data = region.lab.data,aes(x = long, y = lat,label = eponymsPerc),size = 4, hjust = 0.5)+ 
  scale_fill_manual(values = c("Belgium" = "#4daf4a",
                               "France" = "#377eb8",
                               "Germany" = "#984ea3",
                               "Independent" = "lightgray",
                               "Italy" = "#ff7f00",
                               "Portugal" = "#a65628",
                               "Spain" = "#f781bf",
                               "United Kingdom" = "#e41a1c"))+
  theme_void()+
  theme(legend.position = "bottom",
        legend.spacing.y = unit(2.0, 'cm'),
        panel.background = element_rect(fill = "white", colour = "white"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        # axis.line = element_blank(),
        # axis.ticks = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size=18),
        # legend.key = element_rect(size = 3, color = "black"),
        legend.box.margin=margin(5,5,5,5))+
  coord_fixed(ratio = 1.0)
  # scale_bar(lon = -24, lat = -34, 
  #           distance_lon = 500, distance_lat = 100, distance_legend = 200, 
  #           dist_unit = "km", orientation = FALSE)+
  ggsave(paste(output.dir,"EponymRevised.tiff", sep=""), width=210, height=297, unit="mm", dpi=600)

#### Bar charts (Version Jan 2023)
setwd(output.dir)
getwd()

countries = c("Algeria","Angola","Cameroon","Ethiopia","Kenya","Libya","Madagascar",
              "Democratic Republic of the Congo", "Cape Verde", 
              "Senegal", "South Africa","Sudan","Tanzania")
# countries = unique(df3$region)
class(df3$region)
  
for (i in 1:length(countries)){
  
  df.i = df3[df3$region == countries[i],]
  df.i$Nationality = as.character(df.i$Nationality)
  
  ggplot(df.i, aes(x = "", y = epoNatPerc, fill = factor(Nationality), alpha = 0.5)) +
    geom_col(color = "black", alpha = 0.5) +
    geom_text(aes(label = NA), size = 2, position = position_stack(vjust = 0.5)) +
    # geom_text(aes(label = epoNatPerc), size = 2, position = position_stack(vjust = 0.5)) +
    scale_fill_manual(values = c("Other countries" = "#ffffbf",
                                 "Belgium" = "#4daf4a",
                                 "France" = "#377eb8",
                                 "Germany" = "#984ea3",
                                 "Italy" = "#ff7f00",
                                 "Portugal" = "#a65628",
                                 "Spain" = "#f781bf",
                                 "United Kingdom" = "#e41a1c",
                                 "United States of America" = "#a6cee3",
                                 "Native" = "black"))+
    coord_polar(theta = "y")+
    theme(legend.position = "bottom",panel.background = element_rect(fill = "white", colour = "darkgray"),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          legend.title = element_blank())+
          theme(legend.key.size = unit(0.75, 'cm'), #change legend key size
                legend.key.height = unit(0.75, 'cm'), #change legend key height
                legend.key.width = unit(0.75, 'cm'), #change legend key width
                legend.title = element_text(size=14), #change legend title font size
                legend.text = element_text(size=14)) #change legend text font size
          # ggsave(paste(output.dir,countries[i],"EponymNationsEnd.tiff", sep=""), width=30, height=30, unit="mm", dpi=600)
          ggsave(paste(countries[i],"EponymNationsEnd.tiff", sep=""), width=30, height=30, unit="mm", dpi=600)
}

## Occupation map figure ----------------------------------------------------------
### Import the data
# dfWide = read_excel("C:/Users/Utilizador/Dropbox/TROPIBIO/Patricia/Decolonization/Data/TitulosContigency.xlsx")
fun <- function(z) {
  empires = c("United Kingdom","France","Germany","Portugal","Italy","Belgium","Spain")
  z[!(z %in% empires)] = "Other countries"
  z
}
total$Nationality = fct_relabel(total$Nationality, fun)
    
dfWide <- total %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  filter(!is.na(Nationality)) %>%
  group_by(Nationality) %>%
  summarise(Nobility=sum(Nobility),Army=sum(Army),Academic=sum(Academic),
            Collector=sum(Collector),Colonial.administration=sum(`Colonial administration`),
            Family=sum(Family),Bankers=sum(Bankers))

dfLong = gather(dfWide,Occupation,Value, Nobility:Bankers,factor_key=TRUE)

### Stacked Bar Chart
ggplot(dfLong, aes(fill=Occupation, y=Value, x=reorder(Nationality,Value))) + 
  geom_bar(position="stack", stat="identity")+ #stack for different frequencies
  labs(title = "", y = "Number of eponyms", x = "")+
  geom_col() +
  coord_flip() +
  theme(legend.position = "bottom") +
  scale_fill_manual(labels = c("Nobility","Army","Academic","Collector",
                               "Colonial admnistration", "Family", "Banker"),
                    # values = c("#081d58","#225ea8","#1d91c0","#41b6c4","#7fcdbb","#c7e9b4","#edf8b1"))+
                    # values = c("#440154","#443983","#31688e","#21918c","#35b779","#90d743","#fde725"))+
                    # values = c("#222B45","#8B226A","#E55A30","#580F6E","#F88E09","#BB3754","#C71E1D"))+
                    values = c("#222B45","#FECC59","#E55A30","#580F6E","#F88E09","#15607A","#C71E1D"))+
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid', colour = "lightgray"), 
        panel.grid.minor = element_line(size = 0.5, linetype = 'solid', colour = "lightgray"),
        axis.title.x = element_text(size=32, hjust=0.5, vjust=0),
        # axis.title.y = element_text(size=32, hjust=0.5, vjust=0),
        # axis.title.y = element_blank(),
        axis.text = element_text(size=32),
        legend.title = element_blank(),
        legend.text = element_text(size=32),
        legend.position = "bottom",
        axis.ticks = element_blank(),
        legend.box.margin=margin(20,20,20,20))
ggsave(paste("C:/Users/Utilizador/Dropbox/TROPIBIO/Patricia/Decolonization/Results/Figures/Figure1/","OccupationEndV3.tiff", sep=""), width=50, height=30, unit="cm", dpi=600)

