library(tidyverse)
library(sf)
library(ggspatial)

#natuy_mdeo <- read_csv('/home/luis/Documents/GitHub/biases_naturalistauy_montevideo/data/observations-536732.csv')
natuy_mdeo <- read_csv('data/observations-536732.csv')

natuy_data_mdeo <- natuy_mdeo %>% 
  # filter records with low positional accuracy
  filter(positional_accuracy <= 500) %>% 
  # filter records with obscured coordinates
  filter(coordinates_obscured == F)

nrow(natuy_data_mdeo) # 10299 registros
nrow(natuy_data_mdeo %>% distinct(taxon_species_name)) # 1387 especies

  # hexagonal grid with 'id_hex' as identifier 
#mvd_hex <- read_sf('/home/luis/Documents/GitHub/biases_naturalistauy_montevideo/data/hex/mvd_vars.shp')
mvd_hex <- read_sf('data/hex/mvd_vars.shp')

  # armando la base mvd_hex con variables prexistentes y el agregado de S y N
test <- st_as_sf(natuy_data_mdeo,
                 coords = c("longitude", "latitude"),
                 crs = st_crs(mvd_hex))
sf::sf_use_s2(FALSE) 
test<-st_intersection(mvd_hex, test)
hexa<- as.data.frame(test) %>% group_by (id_hex) %>%
  summarise (N=n(), S=n_distinct(taxon_species_name))

mvd_hex<-mvd_hex %>% left_join(hexa, by="id_hex")
mvd_hex<-st_as_sf(mvd_hex)
mvd_hex$N[is.na(mvd_hex$N)]<-0
mvd_hex$S[is.na(mvd_hex$S)]<-0
rm(test, hexa)

ggplot(mvd_hex)+
  geom_sf(aes(fill=S))+
  scale_fill_continuous(type="viridis")+
  theme_void()+
  annotation_scale(location = "bl", width_hint = 0.2)

head(natuy_mdeo)
hist(table(natuy_mdeo$user_login ), breaks=100)

hist(month(natuy_mdeo$observed_on))

table(natuy_mdeo$time_zone)

hist(hour(na.omit((natuy_mdeo$time_observed_at))) )

time<- natuy_mdeo %>% mutate(mes=month(observed_on),
                      hora=hour(time_observed_at) ) %>% 
        select(mes,hora)

ggplot(time)+
  geom_boxplot(aes(x=factor(mes), y=hora), outlier.shape=NA )
