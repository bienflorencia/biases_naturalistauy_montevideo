#rm(list=ls())
library(readr)
library(sf)
library(ggspatial)
library(GGally)
library(lme4)
library(effects)
library(MASS)
library(fitdistrplus)
library(mgcv)
library(gratia)
library(broom)
library(car)
library(tidyverse)

# read bulk data
natuy_mdeo <- read_csv('data/observations-536732.csv')
natuy_data_mdeo <- natuy_mdeo %>% 
  # filter records with low positional accuracy
  filter(positional_accuracy <= 500) %>% 
  # filter records with obscured coordinates
  filter(coordinates_obscured == F)

nrow(natuy_data_mdeo) # 10299 observations
nrow(natuy_data_mdeo %>% distinct(taxon_species_name)) # 1387 species

# hexagonal grid with environmental variables and 'id_hex' as identifier 
mvd_hex <- read_sf('data/hex/mvd_vars.shp')

# build database with mvd_hex 
## prexistent variables + richness iconic taxon(S) and effort(N)
test <- st_as_sf(natuy_data_mdeo,
                 coords = c("longitude", "latitude"),
                 crs = st_crs(mvd_hex))
sf::sf_use_s2(FALSE) 
test<-st_intersection(mvd_hex, test)
hexa<- as.data.frame(test) %>% group_by (id_hex) %>%
  summarise (N=n(), S=n_distinct(iconic_taxon_name))

# hexagonal grid with social variables and 'id_hex' as identifier 
social<- read_sf('data/hex/social_hex_2011.shp')
social<- as.data.frame(social) %>% dplyr::select(-geometry)

mvd_hex<-mvd_hex %>% left_join(hexa, by="id_hex")
mvd_hex<-mvd_hex %>% left_join(social, by="id_hex")
mvd_hex<-st_as_sf(mvd_hex)
mvd_hex$N[is.na(mvd_hex$N)]<-0
mvd_hex$S[is.na(mvd_hex$S)]<-0

rm(test, hexa, natuy_data_mdeo, natuy_mdeo, social)

# re coding variables
mvd_hex$lcz<- as.factor(mvd_hex$lcz)
mvd_hex$costa<- as.factor(mvd_hex$costa)
mvd_hex$pres<- as.numeric(mvd_hex$N>0)

names(mvd_hex)

## id_hex=identifier, ndvi= hex median normalized differences vegetation index, 
# ndmi= hex median normalized differences moisture index, 
# pndbi= hex built-up proportion (based on normalized differences built-up index),
# lcz= local climate zones (urban landscape categories)
# alan= artificial lights at night, costa= binary, is coastal hex?
# dist= distance (m) from the hex centroid to the coastline
# es = ecosystem services, pap = proportion of hex in protected area,
# N = number of inat observations, S= phylum richness of inat obs.,
# pop= number of people (2011), hou_tot= total number of houses (2011),
# hou_vac = number of vacant houses (2011), 
# hou_ubn= number of houses with unsatisfied basic needs (ubn) (2011),
# edu_adu= median adult (>21yr.) education in years (2011),
# phou_ubn= proportion of houses with unsatisfied basic needs (ubn) (2011),
# pres= binary, presence of inat Observations

head(mvd_hex)

#example visualization
ggplot(mvd_hex)+
  geom_sf(aes(fill=ndvi))+
  scale_fill_gradient(low= "white", high="darkgreen")+
  labs(fill="NDVI")+
  theme_void()+
  annotation_scale(location = "bl", width_hint = 0.2)

##### data analysis
# 0 inflated? 
sum(mvd_hex$N == 0)/nrow(mvd_hex)

##### Presence iNat Obs #####
#visualization
ggplot(mvd_hex)+
  geom_sf(aes(fill=factor(pres)) )+
  scale_fill_manual(values=c("white","black"))+
  theme_void()+
  annotation_scale(location = "bl", width_hint = 0.2)

# GAM modeling
fit_bin <- gam(pres ~ s(ndvi) + s(ndmi)+ s(pndbi) + lcz + s(alan) +s(dist) +s(pap) +s(pop) +
                 s(hou_tot) + s(edu_adu), family = binomial, data = mvd_hex)

summary(fit_bin)
rsq::rsq(fit_bin)
#plot(fit_bin)


# remove: lcz
fit_bin2 <- gam(pres ~ s(ndvi) + s(ndmi)+ s(pndbi) + s(alan) +s(dist) +s(pap) +s(pop) +
                 s(hou_tot) +s(edu_adu), family = binomial, data = mvd_hex)

summary(fit_bin2)
rsq::rsq(fit_bin2)
#plot(fit_bin2)

# remove: -pnbi -lcz
fit_bin3 <- gam(pres ~ s(ndvi) + s(ndmi)+ s(alan) +s(dist) +s(pap) +s(pop) +
                  s(hou_tot) +s(edu_adu), family = binomial, data = mvd_hex)


summary(fit_bin3)
rsq::rsq(fit_bin3)
#plot(fit_bin3)


# remove: -ndmi -pnbi -lcz 
fit_bin4 <- gam(pres ~ s(ndvi) + s(alan) +s(dist) +s(pap) +s(pop) +
                  s(hou_tot) +s(edu_adu), family = binomial, data = mvd_hex)

summary(fit_bin4)
rsq::rsq(fit_bin4)
#plot(fit_bin4)


#remove: -pop -ndmi -pnbi -lcz 
fit_bin5 <- gam(pres ~ s(ndvi) + s(alan) +s(dist) +s(pap)+
                  s(hou_tot) + s(edu_adu), family = binomial, data = mvd_hex)


summary(fit_bin5)
rsq::rsq(fit_bin5)
plot(fit_bin5, pages=1)


#remove: -hou_tot -pop -ndmi -pnbi -lcz 
fit_bin6 <- gam(pres ~ s(ndvi) + s(alan) +s(dist) +s(pap)+
              +s(edu_adu), family = binomial, data = mvd_hex)

summary(fit_bin6)
rsq::rsq(fit_bin6)
plot(fit_bin6, pages=1)


sqrt(log2(1148))
concurvity(fit_bin6, full=F)

# model summary results
output<-cbind( AIC(fit_bin, fit_bin2, fit_bin3, fit_bin4, fit_bin5, fit_bin6, k=3.188),
      rsq=c(rsq::rsq(fit_bin),rsq::rsq(fit_bin2),rsq::rsq(fit_bin3),
         rsq::rsq(fit_bin4),rsq::rsq(fit_bin5),rsq::rsq(fit_bin6) ),
      formula= as.character(c(formula(fit_bin),formula(fit_bin2),formula(fit_bin3),
                 formula(fit_bin4),formula(fit_bin5),formula(fit_bin6))))

output <- output %>% arrange(AIC)
output

# example ndvi effect
plot(fit_bin6, select=1)
abline(v=0.15, lty=3, col="red")

draw(fit_bin6)+
  theme_void()

##### Number of Obs #####
#work with N>0 fraction of data
positivo<-mvd_hex %>%  filter (N>0)
hist(positivo$N, breaks=30)

# data distribution
fitnb <- fitdist(positivo$N, "nbinom", method = "mle")
fitpoi <- fitdist(positivo$N, "pois", method = "mle")
summary(fitnb)
plot(fitnb)
summary(fitpoi)
plot(fitpoi)

### modeling observation counts
fit_count <- gam(N ~ s(ndvi) + s(ndmi)+ s(pndbi) + lcz + s(alan) +s(dist) +s(pap) +s(pop) +
                   s(hou_tot) + s(edu_adu), data = positivo, family=nb())
summary(fit_count)
#rsq::rsq(fit_count)
#plot(fit_count)

### remove: - pndbi
fit_count2 <- gam(N ~ s(ndvi) + s(ndmi) + lcz + s(alan) +s(dist) +s(pap) +s(pop) +
                    s(hou_tot) + s(edu_adu), data = positivo, family=nb())

summary(fit_count2)
gam.check(fit_count2)
#plot(fit_count2)
rsq::rsq(fit_count2)


### remove: -ndvi -pndbi
fit_count3 <- gam(N ~ s(ndmi) + lcz + s(alan) +s(dist) +s(pap) +s(pop) +
                    s(hou_tot) + s(edu_adu), data = positivo, family=nb())
summary(fit_count3)
gam.check(fit_count3)

###remove:  -pop -ndvi -pndbi
fit_count4 <-  gam(N ~ s(ndmi) + lcz + s(alan) +s(dist) +s(pap) +
                     s(hou_tot) + s(edu_adu), data = positivo, family=nb())
summary(fit_count4)
plot(fit_count4, pages=1)
gam.check(fit_count4)

### -alan -pop -ndvi -pndbi
fit_count5 <- gam(N ~ s(ndmi) + lcz + +s(dist) +s(pap) +
                    s(hou_tot) + s(edu_adu), data = positivo, family=nb())
summary(fit_count5)
draw(fit_count5)
gam.check(fit_count5)

### count output summary
outcount<-cbind( AIC(fit_count, fit_count2, fit_count3, fit_count4, fit_count5, k=3.18),
                 rsq=c(rsq::rsq(fit_count),rsq::rsq(fit_count2),rsq::rsq(fit_count3),
                       rsq::rsq(fit_count4),rsq::rsq(fit_count)),
                 formula= as.character(c(formula(fit_count),formula(fit_count2),
                                         formula(fit_count3),formula(fit_count4),
                                         formula(fit_count5))))

outcount <- outcount %>% arrange(AIC)
outcount


#### plot lcz effect
# Full variance-covariance matrix
vc <- vcov(fit_count5)
# Get all parameter names
param_names <- colnames(vc)
table(mvd_hex$lcz)
# Identify LCZ-related terms
lcz_terms <- grep("^lcz", param_names, value = TRUE)
# Extract estimates directly from vcov row names
estimates <- fit_count5$coefficients[lcz_terms]
# Get standard errors
ses <- sqrt(diag(vc[lcz_terms, lcz_terms]))
# Assemble dataframe
plot_df <- data.frame(
  level = gsub("^lcz", "", lcz_terms),
  estimate = estimates,
  se = ses,
  stringsAsFactors = FALSE
)
# Add 95% confidence intervals
plot_df$lower <- plot_df$estimate - 1.96 * plot_df$se
plot_df$upper <- plot_df$estimate + 1.96 * plot_df$se
# Add reference level manually (effect = 0)
ref_level <- levels(fit_count5$model$lcz)[1]
plot_df <- rbind(
  data.frame(
    level = ref_level,
    estimate = 0,
    se = NA,
    lower = 0,
    upper = 0
  ),
  plot_df
)
as.numeric(plot_df$level)
# Plot
ggplot(plot_df, aes(x = level, y = estimate)) +
  geom_point(size=6) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  labs(
    x = "LCZ Category",
    y = "Effect"
  ) +
  theme_classic()

### ploting occurence
ggplot(mvd_hex)+
  geom_sf(aes(fill=N) )+
  #scale_fill_gradient2(low="white", high="red", name="Observed")+
  scale_fill_gradientn(
    colours = c("grey70", "yellow", colorRampPalette(c("orange","red", "darkred"))(5)),
    values = scales::rescale(c(0, 1)) )+
  theme_void()+
  annotation_scale(location = "bl", width_hint = 0.2)+
  theme(legend.position="top", legend.direction = "horizontal")
