##Set Up

library(tidyverse)
library(sf)
library(spdep)
library(caret)
library(ckanr)
library(FNN)
library(grid)
library(gridExtra)
library(ggcorrplot)
library(kableExtra)
library(jtools)
library(ggstance)
library(mapview)
library(tigris)
library(leaflet)
library(osmdata)
library(tidycensus)

root.dir = "https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/DATA/"

source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

palette5 <- c("#25CB10", "#5AB60C", "#8FA108",   "#C48C04", "#FA7800")

st_c <- st_coordinates

##Housing Data

Boulder_boundary <- st_read("/Users/inordia/Desktop/UPenn搞起来/592/MUSA508_Midterm/City_of_Boulder_City_Limits.kml") %>%
  st_transform('EPSG:26913')%>%
  select(-Name, -Description)
Boulder_boundary <- st_union(Boulder_boundary)

st_bbox(Boulder_boundary)

housing <- st_read("/Users/inordia/Desktop/UPenn搞起来/592/MUSA508_Midterm/studentData.geojson", crs = 'ESRI:102254')%>%
  st_transform('EPSG:26913')

housing <- housing %>%
  select(-Stories, -UnitCount)

housing.city <- st_intersection(housing, Boulder_boundary)

replace.na ##CLEANING THE NAs

ggplot()  +
  geom_sf(data = housing, aes(colour = q5(price)), 
          show.legend = "point", size = .75) +
  scale_colour_manual(values = palette5,
                      labels=qBr(housing,"price"),
                      name="Quintile\nBreaks") +
  labs(title="Price Per Square Foot, Boulder") +
  mapTheme()

##Internal Characteristics
housing <- housing %>%
  mutate(age = 2021- EffectiveYear)%>%
  filter(nbrBedRoom < 10, carStorageSF < 3000)

housing <- housing[-2637,]

st_drop_geometry(housing)%>%
  dplyr::select(price, TotalFinishedSF, age) %>%
  gather(Variable, Value, -price) %>% 
  ggplot(aes(Value, price)) +
  geom_point(size = .5) + geom_smooth(method = "lm", se=F, colour = "#FA7800") +
  facet_wrap(~Variable, ncol = 3, scales = "free") +
  labs(title = "Price as a function of continuous variables") +
  plotTheme()

st_drop_geometry(housing)%>%
  dplyr::select(price, bsmtSF, carStorageSF, nbrBedRoom, nbrRoomsNobath, nbrFullBaths) %>%
  filter(nbrBedRoom < 10, carStorageSF < 3000)%>%
  gather(Variable, Value, -price) %>% 
  ggplot(aes(Value, price)) +
  geom_point(size = .5) + geom_smooth(method = "lm", se=F, colour = "#FA7800") +
  facet_wrap(~Variable, ncol = 3, scales = "free") +
  labs(title = "Price as a function of continuous variables") +
  plotTheme()

st_drop_geometry(housing) %>% 
  dplyr::select(price,
                designCodeDscr,
                qualityCodeDscr,
                ConstCodeDscr) %>%
  gather(Variable, Value, -price) %>% 
  ggplot(aes(Value, price)) +
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean") +
  facet_wrap(~Variable, ncol = 1, scales = "free") +
  labs(title = "Price as a function of\ncategorical variables", y = "Mean_Price") +
  plotTheme() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

st_drop_geometry(housing) %>% 
  dplyr::select(price,
                HeatingDscr,
                ExtWallDscrPrim,
                IntWallDscr,
                Roof_CoverDscr) %>%
  gather(Variable, Value, -price) %>% 
  ggplot(aes(Value, price)) +
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean") +
  facet_wrap(~Variable, ncol = 1, scales = "free") +
  labs(title = "Price as a function of\ncategorical variables", y = "Mean_Price") +
  plotTheme() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

#IntWall not good

##Feature Engineering: Internal Characteristics
housing <- 
  housing %>%
  mutate(nbrBedRoom.cat = case_when(
    nbrBedRoom >= 0 & nbrBedRoom < 4  ~ "Up to 3 Bedrooms",
    nbrBedRoom >= 4 & nbrBedRoom < 5  ~ "4 Bedrooms",
    nbrBedRoom > 4                    ~ "5+ Bedrooms"))

##Public Facilities

park <- st_read("/Users/inordia/Desktop/UPenn搞起来/592/MUSA508_Midterm/Properties_Managed_by_Parks_and_Recreation_Points.geojson")%>%
  st_transform('EPSG:26913')%>%
  select(NAME, PARKTYPE, SUBCOMMUNITY, geometry)

q0 <- opq(bbox = c(-105.3015,39.95692,-105.1781,40.09448))

park <- add_osm_feature(opq = q0, key = 'leisure', value = "park") %>%
  osmdata_sf(.)

park.sf <- st_geometry(park$osm_points) %>%
  st_transform(4326) %>%
  st_sf() %>%
  cbind(., park$osm_points$name) %>%
  rename(NAME = park.osm_points.name)%>%
  st_transform('EPSG:26913')%>%
  st_intersection(Boulder_boundary,park.sf)%>%
  dplyr::select(geometry)%>%
  distinct()

playground <- st_read("/Users/inordia/Desktop/UPenn搞起来/592/MUSA508_Midterm/Playground_Sites_Points.geojson")%>%
  st_transform('EPSG:26913')%>%
  select(NAME, MAPLABEL, geometry)%>%
  na.omit()

housing <- housing%>%
  mutate(playground=nn_function(st_c(housing),st_c(playground),1))%>%
  mutate(park=nn_function(st_c(housing),st_c(park.sf),1))

##School District

school <- read.csv("/Users/inordia/Desktop/UPenn搞起来/592/MUSA508_Midterm/school.csv")%>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326, agr = "constant")%>%
  st_transform('EPSG:26913')

school.sf <- school%>%
  dplyr::select(geometry) %>%
  st_as_sf(crs = 4326, agr = "constant")%>%
  st_transform('EPSG:26913')

housing$school_buffer =
  st_buffer(housing, 8000) %>% ##WHAT IS THE RIGHT PARAMETER
  aggregate(mutate(school.sf, counter = 1),., sum) %>% 
  pull(counter)

housing <- housing%>%
  mutate(school=nn_function(st_c(housing),st_c(school.sf),1))

##Bus Station

bus_station <- add_osm_feature(opq = q0, key = 'amenity', value = "bus_station") %>%
  osmdata_sf(.)

bus_station.sf <- st_geometry(bus_station$osm_points) %>%
  st_transform(4326) %>%
  st_sf() %>%
  cbind(., bus_station$osm_points$amenity) %>%
  rename(NAME = bus_station.osm_points.amenity)%>%
  st_transform('EPSG:26913')%>%
  st_intersection(Boulder_boundary,bus_station.sf)

housing <- housing%>%
  mutate(bus_stop_nn1=nn_function(st_c(housing),st_c(bus_station.sf),1),
         bus_stop_nn2=nn_function(st_c(housing),st_c(bus_station.sf),2),
         bus_stop_nn3=nn_function(st_c(housing),st_c(bus_station.sf),3))


##Median Income
Median_income <- get_acs(geography = "tract",
                         year = 2019, 
                         variables = c("B06011_001E", #Median income in the past 12 months
                                       "B19013_001E"), #Median household income in the past 12 months
                         geometry = T, 
                         state = "CO", 
                         county = "Boulder", 
                         output = "wide") %>%
  st_transform('EPSG:26913')%>%
  dplyr::select(-NAME,-B06011_001M,-B19013_001M) %>%
  rename(MedianInc = B06011_001E,
         MedHHInc = B19013_001E)

Median_income.county <-  get_acs(geography = "county",
                                 year = 2019, 
                                 variables = c("B06011_001E",
                                               "B19013_001E"),
                                 geometry = T, 
                                 state = "CO", 
                                 county = "Boulder", 
                                 output = "wide") %>%
  st_transform('EPSG:26913')%>%
  dplyr::select(-NAME,-B06011_001M,-B19013_001M) %>%
  rename(MedianInc = B06011_001E,
         MedHHInc = B19013_001E)

##Water

water <- add_osm_feature(opq = q0, key = 'natural', value = "water") %>%
  osmdata_sf()

water.sf <- st_geometry(water$osm_points) %>%
  st_transform(4326) %>%
  st_sf() %>%
  cbind(., water$osm_points$name) %>%
  rename(NAME = water.osm_points.name)%>%
  st_transform('EPSG:26913')%>%
  st_intersection(Boulder_boundary,water.sf)%>%
  dplyr::select(geometry)%>%
  distinct()

housing <- housing%>%
  mutate(water_nn1=nn_function(st_c(housing),st_c(water.sf),1))

##Fast Food

fast_food <- add_osm_feature(opq = q0, key = 'amenity', value = "fast_food") %>%
  osmdata_sf(.)

fast_food.sf <- st_geometry(fast_food$osm_points) %>%
  st_transform(4326) %>%
  st_sf() %>%
  cbind(., fast_food$osm_points$amenity) %>%
  rename(NAME = fast_food.osm_points.amenity)%>%
  st_transform('EPSG:26913')%>%
  st_intersection(Boulder_boundary,fast_food.sf)

fast_food.sf<-
  fast_food.sf%>%
  select(-NAME)

housing$fastfood_buffer =
  st_buffer(housing, 8000) %>% ##WHAT IS THE RIGHT PARAMETER
  aggregate(mutate(fast_food.sf, counter = 1),., sum) %>% 
  pull(counter)


##Correlation Test
numericVars <- 
  select_if(st_drop_geometry(housing), is.numeric) %>% na.omit()

ggcorrplot(
  round(cor(numericVars), 1), 
  p.mat = cor_pmat(numericVars),
  colors = c("#25CB10", "white", "#FA7800"),
  type="lower",
  insig = "blank") +  
  labs(title = "Correlation across numeric variables")

##Regression 1
reg1 <- lm(price ~ ., data = st_drop_geometry(housing) %>% 
             dplyr::select(price,
                           age,
                           designCodeDscr,
                           qualityCodeDscr,
                           TotalFinishedSF,
                           nbrBedRoom.cat,
                           HeatingDscr,
                           Roof_CoverDscr,
                           park,
                           school,
                           bus_stop_nn1,
                           bus_stop_nn2,
                           bus_stop_nn3,
                           fastfood_buffer,
                           water_nn1))
summary(reg1)

##Accuracy
Boulder.training <- housing %>%
  filter(toPredict == 0)

inTrain <- createDataPartition(
  y = paste(housing$designCodeDscr,
            housing$qualityCodeDscr,
            housing$nbrBedRoom.cat,
            housing$HeatingDscr,
            housing$Roof_CoverDscr), 
  p = .75, list = FALSE)
housing.training <- Boulder.training[inTrain,] 
housing.test <- Boulder.training[-inTrain,]  

reg.training <-
  lm(price ~ ., data = as.data.frame(housing.training) %>%
       dplyr::select(price,
                     age,
                     designCodeDscr,
                     qualityCodeDscr,
                     TotalFinishedSF,
                     nbrBedRoom.cat,
                     HeatingDscr,
                     Roof_CoverDscr,
                     park,
                     school,
                     bus_stop_nn1,
                     bus_stop_nn2,
                     bus_stop_nn3,
                     fastfood_buffer,
                     water_nn1))

housing.test <-
  housing.test %>%
  mutate(Regression = "Baseline Regression",
         price.Predict = predict(reg.training, housing.test),
         price.Error = price.Predict - price,
         price.AbsError = abs(price.Predict - price),
         price.APE = (abs(price.Predict - price)) /
           price.Predict)%>%
  filter(price < 8000000)

##Generalizibility

fitControl <- trainControl(method = "cv", number = 100)
set.seed(825)

reg.cv <- 
  train(price ~ ., data = st_drop_geometry(Boulder.training) %>% 
          dplyr::select(price,
                        age,
                        designCodeDscr,
                        qualityCodeDscr,
                        TotalFinishedSF,
                        nbrBedRoom.cat,
                        HeatingDscr,
                        Roof_CoverDscr,
                        park,
                        school,
                        bus_stop_nn1,
                        bus_stop_nn2,
                        bus_stop_nn3,
                        fastfood_buffer,
                        water_nn1), 
        method = "lm", trControl = fitControl, na.action = na.pass)

reg.cv

mean(reg.cv$resample[,3])


##Spatial Structure
neighborhoods <- Median_income%>%
  dplyr::select(-MedianInc,-MedHHInc)

##the spatial lag
Boulder_neigh <-st_join(housing, neighborhoods, join = st_intersects)
housing<-st_join(Boulder_neigh,Median_income,join = st_intersects)
housing <- housing%>%
  select(-GEOID.y)
Boulder.training.nhood <- housing %>%
  filter(toPredict == 0)
coords <- st_coordinates(Boulder.training.nhood)
neighborList <- knn2nb(knearneigh(coords, 5))
spatialWeights <- nb2listw(neighborList, style="W")
Boulder.training.nhood$lagPrice <- lag.listw(spatialWeights,
                                             Boulder.training.nhood$price)

##Reg.nhood
reg.nhood <- lm(price ~ ., data = st_drop_geometry(Boulder.training.nhood) %>% 
             dplyr::select(price,
                           GEOID.x,
                           age,
                           designCodeDscr,
                           qualityCodeDscr,
                           TotalFinishedSF,
                           nbrBedRoom.cat,
                           HeatingDscr,
                           Roof_CoverDscr,
                           park,
                           school,
                           bus_stop_nn1,
                           bus_stop_nn2,
                           bus_stop_nn3,
                           fastfood_buffer,
                           water_nn1,
                           MedHHInc))
summary(reg.nhood)

##Feature Engineering: Income
housing <- 
  housing %>%
  mutate(MedHHInc = case_when(
    MedHHInc >= 0 & MedHHInc < 83019  ~ "Below Average Income",
    MedHHInc >= 83019 ~ "Above Average Income"))
Boulder.training.nhood <- housing %>%
  filter(toPredict == 0)

##Accuracy for Neighborhood Model
inTrain <- createDataPartition(
  y = paste(housing$designCodeDscr,
            housing$qualityCodeDscr,
            housing$nbrBedRoom.cat,
            housing$HeatingDscr,
            housing$Roof_CoverDscr,
            housing$GEOID.x), 
  p = .75, list = FALSE)
housing.training.nhood <- Boulder.training.nhood[inTrain,] 
housing.test.nhood <- Boulder.training.nhood[-inTrain,]  

reg.nhood.training <- lm(price ~ ., data = st_drop_geometry(housing.training.nhood) %>% 
                  dplyr::select(price,
                                GEOID.x,
                                age,
                                designCodeDscr,
                                qualityCodeDscr,
                                TotalFinishedSF,
                                nbrBedRoom.cat,
                                HeatingDscr,
                                Roof_CoverDscr,
                                park,
                                school,
                                bus_stop_nn1,
                                bus_stop_nn2,
                                bus_stop_nn3,
                                fastfood_buffer,
                                water_nn1,
                                MedHHInc))


housing.test.nhood <-
  housing.test.nhood %>%
  mutate(Regression = "Baseline Regression",
         price.Predict = predict(reg.nhood.training, housing.test.nhood),
         price.Error = price.Predict - price,
         price.AbsError = abs(price.Predict - price),
         price.APE = (abs(price.Predict - price)) /
           price.Predict)%>%
  filter(price < 8000000)

