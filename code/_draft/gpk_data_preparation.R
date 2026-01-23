##########################################
# socio-ecological DATA PREPARATION 
##########################################

library(patchwork)
library(janitor)
library(tmap)
tmap_mode('view')
library(sf)
sf_use_s2(FALSE)
library(tidyverse)

uruguay <- geodata::gadm(country = 'UY', 
                         level = 1, 
                         path = 'data/') %>%
  st_as_sf()

montevideo <- uruguay %>% 
  filter(NAME_1 == 'Montevideo') %>%
  st_transform(32721)

##########################################
# GRID

mvd_grid <- read_sf ('data/raw/hex/mvd_vars.shp' ) %>% 
  dplyr::select(id_hex) 

# mvd_grid <- st_make_grid(montevideo,
#                          cellsize = 1000,
#                          square = F) %>%
#   st_sf(id_hex=1:length(.), geometry= .) %>%
#   st_make_valid() %>% st_cast()
# 
# idx <- st_intersects(mvd_grid, montevideo, 
#                      sparse = FALSE)[, 1]
# mvd_grid <- mvd_grid[idx, ]

mvd_grid <- mvd_grid %>% 
  mutate(hex_area = st_area(mvd_grid)) %>% 
  relocate('hex_area', .after = 'id_hex')

st_write(mvd_grid, 
         dsn='data/montevideo_data.gpkg',
         layer='grid', append=FALSE)

mvd_grid <- st_read(dsn = 'data/montevideo_data.gpkg',
                    layer = 'grid')

tm_shape(mvd_grid) + tm_polygons() +
  tm_shape(montevideo) + tm_borders() +
  tm_layout(frame=F)

##########################################
# ENVIRONMENTAL

mvd_env <- read_sf ('data/raw/hex/mvd_vars.shp')

names(mvd_env)
mvd_env <- mvd_env %>% 
  select(id_hex, ndvi, ndmi, pndbi, protection=pap,
         lcz, dist_coast=dist, alan)

st_write(mvd_env, 
         dsn='data/montevideo_data.gpkg',
         layer='env', append=FALSE)

mvd_env <- st_read(dsn = 'data/montevideo_data.gpkg',
                  layer = 'env')

names(mvd_env)

tm_shape(mvd_env) + 
  tm_polygons('ndvi', col_alpha = 0.5, 
              # fill_alpha = 0.7
              fill.scale = tm_scale(n=10, values = 'viridis'),
              fill.legend = tm_legend(frame=F,
                                      position= tm_pos_out())) +
  tm_shape(montevideo) + 
  tm_borders(col='grey10', col_alpha = 0.5) +
  tm_layout(frame=F) + 
  tm_scalebar(position = tm_pos_out('center', 'bottom'), 
              breaks = c(0,5,10))

tm_shape(mvd_env) + 
  tm_polygons('dist_coast', col_alpha = 0.5, 
              # fill_alpha = 0.7
              fill.scale = tm_scale(n=10, values = 'viridis'),
              fill.legend = tm_legend(frame=F,
                                      position= tm_pos_out())) +
  tm_shape(montevideo) + 
  tm_borders(col='grey10', col_alpha = 0.5) +
  tm_layout(frame=F)+ 
  tm_scalebar(position = tm_pos_out('center', 'bottom'), 
              breaks = c(0,5,10))

##########################################
# SOCIAL

census <- st_read('data/raw/hex/Marco2011_SEG_Montevideo_Total.shp')
census <- st_transform(census, crs = 4326) %>% 
  st_make_valid() %>% st_cast()

census <- census %>% select(AREA, V_TOT, V_TOT_DES,
                            P_TOT, E_F_P_21, NBI_Si) %>% 
  mutate(P_D= P_TOT/AREA, 
         H_D=V_TOT/AREA, 
         H_VAC_D=V_TOT_DES/AREA, 
         UBN=NBI_Si/AREA,
         EDU= E_F_P_21) %>% 
  relocate('geometry', .after = 'EDU')

# intersect grid and census, calculate area and proportion of intersection for each polygon

mvd_hex_census <- st_intersection(mvd_grid, census) %>%
  mutate(area_int = st_area(geom),
         prop=area_int/hex_area) %>% 
  # st_cast() %>% 
  # multiply densities by the area they cover, 
  # and Education and NBI by the proportion of the area
  mutate(P_D = P_D*area_int, 
         H_D = H_D*area_int, 
         H_VAC_D = H_VAC_D*area_int,
         edu_adu = E_F_P_21*prop, 
         UBN = UBN*area_int) %>%
  group_by(id_hex) %>% 
  # group by grid cell id and sum values to 
  summarise(pop = as.numeric(sum(P_D, na.rm = TRUE)), # population
            hou_tot = as.numeric(sum(H_D, na.rm = TRUE)), # total houses
            hou_vac = as.numeric(sum(H_VAC_D, na.rm = TRUE)), # vacant houses
            hou_ubn = as.numeric(sum(UBN, na.rm = TRUE)), # mean adult education
            edu_adu = as.numeric(mean(EDU, na.rm = TRUE))) 

social <- left_join(mvd_grid, 
                    st_drop_geometry(mvd_hex_census),
                    by='id_hex') %>% 
  select(-hex_area)

social %>% names()

social <- social %>% 
  select(id_hex, pop, houses=hou_tot, education=edu_adu)

st_write(obj = social, 
         dsn='data/montevideo_data.gpkg',
         layer='social', append=FALSE)

social <- st_read(dsn = 'data/montevideo_data.gpkg',
                  layer = 'social')

tm_shape(social) + 
  # tm_graticules() +
  tm_polygons('education', col_alpha = 0.5, 
              # fill_alpha = 0.7
              fill.scale = tm_scale(n=10, values = 'viridis'),
              fill.legend = tm_legend(frame=F,
                                      position= tm_pos_out())) +
  tm_shape(montevideo) + 
  tm_borders(col='grey10', col_alpha = 0.5) +
  tm_layout(frame=F) + 
  tm_scalebar(position = tm_pos_out('center', 'bottom'), 
              breaks = c(0,5,10))

##########################################
## FINAL CHECK

st_layers('data/montevideo_data.gpkg')


####################################################################################
####################################################################################
## STORE THE DATA

mvd_env <- st_read(dsn = 'data/montevideo_data.gpkg',
                   layer = 'env', quiet = TRUE)

mvd_social <- st_read(dsn = 'data/montevideo_data.gpkg',
                      layer = 'social', quiet = TRUE)

mvd_hex <- st_join(st_join(mvd_env, 
                           mvd_social %>% select(-id_hex), 
                           join = st_equals), 
                   mvd_inat %>% select(-id_hex), 
                   join = st_equals) %>% 
  mutate(pres = ifelse(N>0, 1, 0),
         lcz = as.factor(lcz)) %>% 
  relocate('pres', .after = 'S')




