#Getting ABMI bird data using their R api

library(abmidata)
library(tidyverse)

siteTab <- ad_get_table("T01A")

siteTab <- siteTab %>% rename_with(~gsub(' ','.',.x)) %>% 
  select(ABMI.Site,contains('Latitude'),contains('Longitude')) %>% 
  filter(!grepl('OG',ABMI.Site)) %>% 
  distinct() %>% mutate(across(-ABMI.Site,as.numeric)) %>% 
  na.omit() %>% mutate(ABMI.Site=factor(ABMI.Site)) 

  # st_as_sf(coords = c('Public.Longitude','Public.Latitude'),crs=4326)

siteTab %>% st_as_sf(coords = c('Public.Longitude','Public.Latitude'),crs=4326) %>% 
  ggplot()+geom_sf()

n <- ad_get_table_names()
n <- data.frame(Description = n)

birdTab <- ad_get_table("CT26")
names(birdTab) <- c('temp',names(birdTab)[1:8])
birdTab <- birdTab %>% rename_with(~gsub(' ','.',.x))

birdTab2 <- birdTab %>% filter(grepl("(Wilson's Warbler|Pine Siskin|Yellow Warbler|Le Conte's Sparrow|Sharp-tailed Grouse)",Common.Name)) %>% 
  select(ABMI.Site:Scientific.Name,contains('Point.Count')) %>% 
  mutate(ABMI.Site=as.character(ABMI.Site)) %>% rename(detections=Average.Number.of.Detections.per.Point.Count.Station) %>%
  mutate(Common.Name=factor(Common.Name),ABMI.Site=factor(ABMI.Site,levels=levels(siteTab$ABMI.Site))) %>% 
  group_by(ABMI.Site,Common.Name,.drop=FALSE) %>% 
  summarize(medDetects=median(detections)) %>% 
  left_join(siteTab,by='ABMI.Site') %>% filter(!is.na(Public.Latitude)) %>% 
  st_as_sf(coords = c('Public.Longitude','Public.Latitude'),crs=4326)
  # mutate(detections=ifelse(is.na(detections),0,detections))

birdTab2 %>% #pivot_wider(names_from='Common.Name',values_from='detections') %>% 
  ggplot()+geom_sf(aes(col=log(medDetects)))+facet_wrap(~Common.Name)+
  labs(col='ln(Detections)')

birdTab2 %>% #pivot_wider(names_from='Common.Name',values_from='detections') %>% 
  ggplot()+geom_sf(aes(col=nDetects))+facet_wrap(~Common.Name)+
  labs(col='N years\nDetected')

birdTab2 %>% ungroup() %>% mutate(lon=st_coordinates(.)[,1],lat=st_coordinates(.)[,2]) %>% st_drop_geometry() %>% 
  write.csv('./08 Spatiotemporal models/shapefiles/birdDat.csv',row.names = FALSE)


