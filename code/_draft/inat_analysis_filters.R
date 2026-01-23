library(tidyverse)
library(sf)
library(ggspatial)
library(GGally)
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

tzcheck<-hour(with_tz(natuy_mdeo$time_observed_at, tzone= "America/Argentina/Buenos_Aires"))==hour(natuy_mdeo$time_observed_at)

data.frame(natuy_mdeo[natuy_mdeo$time_zone!="Montevideo",])

time<- natuy_mdeo %>% mutate(mes=month(observed_on),
                      hora=hour(with_tz(time_observed_at, tzone= "America/Argentina/Buenos_Aires")),
                      dia= weekdays(observed_on),
                      finde=case_when(dia %in% c("Saturday", "Sunday") ~ 1,
                                      TRUE ~0 ) ) %>% 
          select(mes,hora, dia, finde)

###análisis de fin de semana, proporcion de observaciones
diario<-time %>%  
  group_by (mes,finde) %>% summarise(tot=n()) %>% 
  mutate(fesp = case_when(finde ==1 ~ 2/7,
                          TRUE ~ 5/7),
         fobs = tot/sum(tot)) %>%
  group_by(mes) %>% mutate(tot_mes=sum(tot))

#diferencias entre semana y finde por mes
for (i in 1:12){ 
dummy<-chisq.test(x=diario$tot[diario$mes==i], p=diario$fesp[diario$mes==i]) 
print(paste("mes=", i, "p=", dummy$p.value, "differences=",dummy$p.value<0.05 ))
  }

ggplot(time)+
  geom_bar(aes(x=factor(mes), fill=factor(finde)), stat="count", position="dodge")+
  #geom_text(aes(x=c(1:12), y=rep(1500,12), label=c("<",">",">",">",">",">",">","<",">",">",">","<")), vjust=0) +
  theme_classic()

ggplot(diario)+
  geom_bar(aes(x=factor(mes), y=fobs, fill=factor(finde) ), stat="identity", position="dodge" )+
  geom_segment(data=diario %>% filter(finde==0), 
               aes(x=mes-0.45, xend=mes, y= fesp, yend=fesp), linewidth=0.7, color="pink") +
  
  geom_segment(data=diario %>% filter(finde==1), 
               aes(x=mes, xend=mes+0.45, y= fesp, yend=fesp), linewidth=0.7, color="darkcyan") +
  theme_classic()

##### horas ####
ggplot(time)+
  geom_bar(aes(x=hora), stat="count", position="dodge")+
  scale_color_manual(values="red")+
  theme_classic()+
  facet_wrap(~mes)

### cambian las horas por mes?
kruskal.test(hora ~ mes, data=time)
#p<0.05 diferencias significativas mediana entre meses
res<-pairwise.wilcox.test(time$hora, time$mes, p.adjust.method = "bonferroni")
data.frame(res[3])<0.05

### cambian las horas entre finde y semana
kruskal.test(hora ~ finde, data=time)

### cambian las horas entre finde y semana cada mes?
for (i in 1:12) { 
print(paste("mes =", i, "p =", kruskal.test(hora ~ finde, data=time %>% filter(mes==i) )$p.value,
      kruskal.test(hora ~ finde, data=time %>% filter(mes==i) )$p.value<0.05))
}

#### behold! ####
head(natuy_mdeo)
names(natuy_mdeo)
unique(natuy_mdeo$iconic_taxon_name)

beho<-natuy_mdeo %>% group_by(user_id, taxon_phylum_name) %>% 
  count() %>% 
  group_by(user_id) %>% 
  mutate( pi=n/sum(n) ) %>% 
  summarise(H= -sum(pi*log(pi)),
            S= n_distinct(taxon_phylum_name),
            Hmax= log(S),
            N =sum(n), .groups="drop" ) %>% 
  mutate(E=H/Hmax)

beho$E[is.na(beho$E>0)]<-0
beho

hist(beho$S, breaks=20)

par(mar=c(5,4,4,2))
boxplot(N~S, data=beho, xlab="S(Phylum)", ylab="Nobs", main="usuarios")
par(new = TRUE)          

# Draw second plot using axis y2 
plot(x=1:10,  y=as.numeric(table(beho$S)), type="l" , axes = FALSE, xlab = "", ylab = "", lwd=2, lty=3, col="red")
axis(side = 4, at = pretty(range(table(beho$S)) ), col="red" )
text(x=2, y=600, labels="N_us", col="red")


 

plot(y=beho$N, x=beho$H, xlab="H(shannon)", ylab="Nobs", main="Phylum", pch=16)
hist(beho$H[beho$H>0], breaks=20)

plot(x=beho$H, y=beho$E, xlab="Shannon", ylab="Equity", main="Phylum", pch=16)

points(y=beho$H[beho$H==0], x=beho$E[beho$E==0], col="red", pch=16)

hist( beho$N[beho$N>3], breaks=100)

boxplot(beho$H[beho$H>0])
hist(beho$E[beho$E>0])
hist(beho$N[beho$N>1], breaks=100)


quantile(beho$H[beho$H>0])
quantile(beho$E[beho$E>0])
quantile(beho$N[beho$N>1])

ggpairs(beho)

Href<- median(beho$H[beho$H>0])
Eref<- median(beho$E[beho$E>0])
Nref<- median(beho$N[beho$N>1])

#one hit wonder
nrow(beho %>% filter(S==1 & N==1))/ nrow(beho)
#one trick pony
nrow(beho %>% filter(S==1 & N>1))/ nrow(beho)

#

beho<- beho %>% mutate(inart= E*H*log(N) )
beho<- beho %>% mutate(inart2= E*H*H*log(N) )
mcoy<- beho %>% filter(H!=0)

ggpairs(beho)
ggpairs(mcoy)


########## wtff ################
tabla<-table(natuy_data_mdeo %>% select(user_id, taxon_phylum_name))
tabla<- as.data.frame.matrix(tabla)
summary(tabla)
tabla
observers<-import.RInSp(tabla,  row.names = 0)

### wtc 
wtc<-WTcMC(observers)
wic<-wtc$montecarlo[,1]
bic<-wtc$montecarlo[,2]
tnw<- wic+bic
wont<- wic/tnw

psi<- PSicalc(observers)
psim_obs<-psi$PSi
e_obs<- Eindex(observers)

plot(tnw, wont)

length(wont)
length(psim_obs)
length(e_obs$Cw)
