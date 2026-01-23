library(tidyverse)
library(sf)
library(ggspatial)
library(RInSp)


natuy_mdeo <- read_csv('data/observations-536732.csv')

natuy_data_mdeo <- natuy_mdeo %>% 
  # filter records with low positional accuracy
  filter(positional_accuracy <= 500) %>% 
  # filter records with obscured coordinates
  filter(coordinates_obscured == F)

nrow(natuy_data_mdeo) # 10299 registros
nrow(natuy_data_mdeo %>% distinct(taxon_species_name)) # 1387 especies

# hexagonal grid with 'id_hex' as identifier 
mvd_hex <- read_sf('data/hex/mvd_vars.shp')

# build the grid database with preexisting landscape variables adding
# S (phylum richness),  No(number of distinct observers), and N (number of observations)

test <- st_as_sf(natuy_data_mdeo,
                 coords = c("longitude", "latitude"),
                 crs = st_crs(mvd_hex))
sf::sf_use_s2(FALSE) 
test<-st_intersection(mvd_hex, test)
head(test)

user_hex<-test %>% dplyr::select(id_hex, user_id,taxon_phylum_name, taxon_class_name,
                                 taxon_species_name, iconic_taxon_name) 

user_hex<-user_hex[!is.na(user_hex$taxon_phylum_name),]

user_hex<-as.data.frame(user_hex) %>% dplyr::select(-geometry) %>% 
                  group_by(id_hex) %>% mutate(No=n_distinct(user_id),
                                              N=n(),
                                              S=n_distinct(taxon_phylum_name))

user_hex

#create a list of users with observations per cell grid
user_list <- user_hex %>%
  group_by(id_hex) %>%
  summarise(S=median(S), users = list(user_id), .groups = "drop")

user_list<- user_list %>%
            mutate(n_users = map_int(users, n_distinct))

user_list

# clean memory
rm(natuy_mdeo, test)

########## Bolnick ################

# create a table of number of phylum observations by user
tabla<-table(natuy_data_mdeo %>% dplyr::select(user_id, taxon_phylum_name))
tabla<- as.data.frame.matrix(tabla)
tabla

# create a RInSp object of users observations
observers<-import.RInSp(tabla,  row.names = 0)

#### exclude hex with one species and one user
user_list_2<-user_list %>% filter(n_users>1 & S>1)

out<- NULL

## for each cell calculate the attributes of the population of observers
# considering all observations to depict potential amplitude of observations

for (i in 1:length(user_list_2$id_hex)) {
hex<- user_list_2$id_hex[i]

# all observations of all users active on cell i
subtabla<- tabla[rownames(tabla) %in%  
                   as.character(unlist(user_list$users[user_list$id_hex==hex])),]

rinsphex<- import.RInSp(subtabla,row.names = 0)
psi<- PSicalc(rinsphex)
IS<-psi$IS
ISp<- psi$IS.pvalue
output <- capture.output(WTdMC(rinsphex)[1])
output <- paste(output, collapse = "\n")
WIC      <- str_extract(output, "Within-individual component\\s*=\\s*[0-9.]+") |> str_extract("[0-9.]+") |> as.numeric()
BIC      <- str_extract(output, "Between-individual component\\s*=\\s*[0-9.]+") |> str_extract("[0-9.]+") |> as.numeric()
TNW      <- str_extract(output, "Total Niche Width for the population\\s*=\\s*[0-9.]+") |> str_extract("[0-9.]+") |> as.numeric()
p_value  <- str_extract(output, "The p-value is:\\s*[0-9.]+") |> str_extract("[0-9.]+") |> as.numeric() 
#fin else
me<-cbind(id_hex=hex, wic=WIC, bic=BIC, tnw=TNW, pv=p_value, is=IS, isp=ISp)
out<- rbind(out, me)
}

hex_bolnick<-data.frame(out)
head(hex_bolnick)

par(pty="s")
plot(hex_bolnick$tnw, hex_bolnick$wic, ylab="Whithin-individual component", xlab="Total Population Niche Width",
     xlim=c(0,1.4), ylim=c(0,1.4), cex=0.8)
abline(a=0, b=1, lty=2, col="orange", lwd=2)
abline(a=0, b=0.75, lty=4, col="darkorange", lwd=2)
abline(a=0, b=0.5, lty=3, col="red", lwd=2)
abline(a=0, b=0.25, lty=5, col="darkred", lwd=2)
