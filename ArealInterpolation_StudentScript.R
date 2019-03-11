# Spatial interpolation 
# teacher script
library(dplyr)
library(sf)
library(sp)
library(rgeos)
library(cartography)

# functions written by Thibault Laurent
# https://github.com/tibo31/spatial_project/blob/master/AIM.R
source("https://raw.githubusercontent.com/tibo31/spatial_project/master/AIM.R")


# import the census data ----
load("Data/Census/Census.RData")

# illustrate target and source zones
par(mfrow = c(1,2))
plot(Census.state49$geometry, main = "Source Zones" ,sub = "49 states of the USA")
plot(Census.county49$geometry, main = "Target Zones",sub = "counties withon these 49 sates")


# Define target variable (Y) for all methodes
# (to have comparable results)

# intensive case: incomepercapE (GDP)  at source level 
Y_s.int <- Census.state49[,c("incomepercapE")] %>% st_drop_geometry()

# extensive case: carsE (number of cars)  at source level
Y_s.ext <- Census.state49[,c("carsE")] %>% st_drop_geometry()


# Methode 1: AWI ----
# areal wiegthin interpolation

# case 1:intensive variable: income at source level
County49.awi <- 
  daw(nature = "intensive",
      sources = as(Census.state49,"Spatial"),  # daw works with spatial (sp) 
      targets = as(Census.county49,"Spatial"), # convert sf objects to sp objects!
      y = "incomepercapE")

# add the estimation error to the dataframe
Census.county49 <-Census.county49 %>% 
  mutate(ERROR_awi_incomepercap = 
           abs(County49.awi$incomepercapEdaw - Census.county49$incomepercapE))

#We plot the results for the variable *ERROR_awi_incomepercap* : 
par(mfrow = c(1,1))
cols <- carto.pal(pal1 = "red.pal",n1 = 20) 

plot(Census.county49$geometry)
choroLayer(Census.county49, var = "ERROR_awi_incomepercap",
           legend.pos = "bottom",
           legend.horiz = TRUE,
           legend.title.txt = NA,
           col = cols,
           breaks = seq(min(Census.county49$ERROR_awi_incomepercap), 
                        max(Census.county49$ERROR_awi_incomepercap),
                        length.out = 20))
title("Absolute errors of AIW methode",
      "(Income per capita)")

#case 2:extensive variable
# extensive case: carsE (number of cars)  at source level
County49.awi2 <- 
  daw(nature = "extensive",
      sources = as(Census.state49,"Spatial"),  # daw works with spatial (sp) 
      targets = as(Census.county49,"Spatial"), # convert sf objects to sp objects!
      y = "carsE")

# add the estimation error to the dataframe
Census.county49 <- Census.county49 %>% 
  mutate(ERROR_awi_cars = 
           abs(County49.awi2$carsEdaw - Census.county49$carsE))

#We plot the results for the variable *ERROR_awi_cars* : 
plot(Census.county49$geometry)
choroLayer(Census.county49, var = "ERROR_awi_cars",
           legend.pos = "bottom",
           legend.horiz = TRUE,
           legend.title.txt = NA,
           col = cols,
           breaks = seq(min(Census.county49$ERROR_awi_cars), 
                        max(Census.county49$ERROR_awi_cars),
                        length.out = 20))

title("Absolute errors of interpolating the number of cars",
      "(AIW methode)")

# control sums
Census.county49$carsE %>% sum()  
County49.awi2$carsEdaw %>% sum() # pycnophylactic property is fulfilled
Census.state49$carsE %>% sum()   # pycnophylactic property is fulfilled

# Methode 2: DAX ----
# Daisymetric

#sf to spatial 
state <- st_sf(Census.state49)
country <- st_sf(Census.county49)

countrysp <- as(Census.state49, "Spatial")
statesp <- as(Census.county49, "Spatial")

##DAX
#define the intersection between sources and target
st_inter<- intersect.spdf(sources = statesp,
                          targets = countrysp,
                          out = "spdf")
row.names(st_inter)<-as.character(1:length(st_inter))

#OBATAIN the estimation at the intersection level 
daw_median_inter<- daw(sources = statesp,  #state as grid
                       targets = st_inter,
                       y= "medianincomeE",
                       nature = "extensive", scaling =F)

choroLayer(spdf=daw_median_inter, var= "medianincomeEdaw")
#na<-0
daw_median_inter@data$medianincomeEdaw[is.na(daw_median_inter@data$medianincomeEdaw)]<-0


#DAX method
# y extensive, x intensive
daxdata<- dax(sources = statesp,
              targets = countrysp,
              y= "carsE",
              st.df = daw_median_inter@data,
              x="medianincomeEdaw",
              scaling = F)
head(daxdata@data)
# resultat
choroLayer(spdf=daxdata, var= "carsEdax")

# plot the errors
Census.county49.dax <-Census.county49 %>% 
  mutate(ERROR_awi_cars = 
           abs(daxdata$carsEdax - Census.county49$carsE))
cols= carto.pal(pal1 = "red.pal",n1=20)
plot(Census.county49.dax$geometry)


choroLayer(Census.county49.dax, var = "ERROR_awi_cars",
           legend.pos = "bottomleft",
           #legend.horiz = TRUE,
           #legend.title.txt = NA,
           col = cols,
           breaks = seq(min(Census.county49.dax$ERROR_awi_cars),33240,
                        #max(Census.county49.dax$ERROR_awi_cars),
                        length.out = 20))

title("Absolute errors of DAX method",
      "(number of the cars)")

# Methode 3: Regression ----
# i) Intensive variable (average income: incomepercapE)

# Intensive variable ==> normal distribution ==> linear model
lm_income <- lm(incomepercapE ~ unemp_rate + medianageE + ginicoefE + car_per_cap + area_m2
                , data = Census.state49  # use source level data to fit the model
                , weights = Census.state49$populationE
                ) # weighted least-square!

pred_income <- predict(lm_income,newdata = Census.county49,type = "response")

Census.county49 <- Census.county49 %>% 
  mutate(incomepercapE_reg = pred_income,
         ERROR_reg_income = abs(incomepercapE - incomepercapE_reg)
         ) 


plot(Census.county49$geometry)
choroLayer(Census.county49, var = "ERROR_reg_income",
           legend.pos = "bottom",
           legend.horiz = TRUE,
           legend.title.txt = NA,
           col = cols,
           breaks = seq(min(Census.county49$ERROR_reg_income), 
                        max(Census.county49$ERROR_reg_income),
                        length.out = 20))
title("Absolute errors of regression methode",
      "(per capita income)")




# ii) Extensive variable (number of cars: carsE)
# which variables shoudl be included in the model ?
# Intensive variable ==> poisson distribution ==> glm(family = "poisson)
pois_cars <- glm(carsE ~  populationE + area_m2 + medianincomeE
                , family = poisson(link = "log")
                , data = Census.state49  # use source level data to fit the model
                , weights = Census.state49$populationE
                )
pred_cars <- predict(pois_cars,newdata = Census.county49,type = "response")

# pycnophylactic property is not fulfilled 
# we overestimate the number of cars by 48!
pred_cars %>% sum() /Census.state49$carsE %>% sum()

# enforcing the pycnophylactic property
Census.county49 <- Census.county49 %>% 
  mutate(carsE_pois = as.numeric(pred_cars)) %>% 
  group_by(STATE) %>%
  mutate(carsE_pois_pycno = carsE_pois/sum(carsE_pois)*sum(carsE)) %>%
  ungroup() %>%
  mutate(ERROR_reg_cars = abs(carsE - carsE_pois_pycno))

# check that sums are equal
Census.county49$carsE_pois_pycno %>% sum() / Census.county49$carsE %>% sum()

plot(Census.county49$geometry)
choroLayer(Census.county49, var = "ERROR_reg_cars",
           legend.pos = "bottom",
           legend.horiz = TRUE,
           legend.title.txt = NA,
           col = cols,
           breaks = seq(min(Census.county49$ERROR_reg_cars), 
                        max(Census.county49$ERROR_reg_cars),
                        length.out = 20))
title("Absolute errors of regression methode",
      "(number of cars)")
