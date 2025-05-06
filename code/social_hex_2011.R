#rm(list=ls())
library(tidyverse)
library(sf)
library(viridis)
library(ggspatial)
library(gridExtra)

# reading census information
census<-st_read('data/hex/Marco2011_SEG_Montevideo_Total.shp')
census<- st_transform(census, crs = 4326)
census<-st_make_valid(census)
names(census)

#plot irregular polygons
plot(census$geometry)

# select variables of interest and calculate values per square meter
## P_D:population density, H_R:house density, H_VAC_D: vacant house density,
### UBN: proportion of houses with Unsatisfied Basic Needs, EDU: Adult (>21yr) education (yr)
census<-census %>% dplyr::select(AREA, V_TOT, V_TOT_DES, P_TOT, E_F_P_21, NBI_Si) %>% 
          mutate(P_D= P_TOT/AREA, H_D=V_TOT/AREA, H_VAC_D=V_TOT_DES/AREA, UBN=NBI_Si/AREA,
                 EDU= E_F_P_21)
head(census)

### load the Montevideo hex grid and keep only the id_hex
mvd_hex<- read_sf ("data/hex/mvd_vars.shp" ) %>% dplyr::select(id_hex)
# calculate cell areas
mvd_hex$hex_area <- st_area(mvd_hex)

#work with planar geometry
sf::sf_use_s2(FALSE) 

# intersect grid and census, calculate area and proportion of intersection for each polygon
int <- st_intersection(mvd_hex, census) %>%
  mutate(area_int = st_area(geometry),
         prop=area_int/hex_area)
head(int)

# multiply densities by the area they cover, and Education and NBI by the proportion of the area
int<- int %>% mutate(P_D=P_D*area_int, H_D= H_D*area_int, H_VAC_D=H_VAC_D*area_int,
                     edu_adu=E_F_P_21*prop, UBN= UBN*area_int) %>% 

#group by grid cell id and sum values to obtain wheighted mean of: population (pop),
  # total houses (hou_tot), vacant houses (hou_vac), mean adult education(edu_adu)
  group_by(id_hex) %>% summarise(pop = as.numeric(sum(P_D, na.rm = TRUE)),
                                 hou_tot = as.numeric(sum(H_D, na.rm = TRUE)),
                                 hou_vac = as.numeric(sum(H_VAC_D, na.rm = TRUE)),
                                 hou_ubn = as.numeric(sum(UBN, na.rm = TRUE)),
                                 edu_adu = as.numeric(mean(EDU, na.rm = TRUE)) ) %>% 
                    as.data.frame() %>% dplyr::select(-geometry)
head(int)

social<- left_join(mvd_hex, int, by="id_hex") %>% select(-hex_area)


ggplot(social)+
  geom_sf(aes(fill=pop))+
  scale_fill_gradientn(colors = viridis(10), name="people")+
  theme_void()+
  annotation_scale(location = "bl", width_hint = 0.2)

ggplot(social)+
  geom_sf(aes(fill=edu_adu))+
  scale_fill_gradientn(colors = viridis(10), name="AdultEducation(yr)")+
  theme_void()+
  annotation_scale(location = "bl", width_hint = 0.2)

ggplot(social)+
  geom_sf(aes(fill=hou_tot))+
  scale_fill_gradientn(colors = viridis(10), name="total houses")+
  theme_void()+
  annotation_scale(location = "bl", width_hint = 0.2)

ggplot(social)+
  geom_sf(aes(fill=hou_vac))+
  scale_fill_gradientn(colors = viridis(10), name="vacant houses")+
  theme_void()+
  annotation_scale(location = "bl", width_hint = 0.2)

ggplot(social)+
  geom_sf(aes(fill=hou_ubn))+
  scale_fill_gradientn(colors = viridis(10), name="houses w/UBN")+
  theme_void()+
  annotation_scale(location = "bl", width_hint = 0.2)

#write results with social vars resampled for hexagonal grid
#st_write(social, "data/hex/social_hex_2011.shp")
