
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
library(jtools)     # for regression model plots
library(ggstance) # to support jtools plots
library(mapview)
library(tigris)
library(leaflet)
library(osmdata)
library(tidycensus)

root.dir = "https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/DATA/"

source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

palette5 <- c("#25CB10", "#5AB60C", "#8FA108",   "#C48C04", "#FA7800")

qBr <- function(df, variable, rnd) {
  if (missing(rnd)) {
    as.character(quantile(round(df[[variable]],0),
                          c(.01,.2,.4,.6,.8), na.rm=T))
  } else if (rnd == FALSE | rnd == F) {
    as.character(formatC(quantile(df[[variable]]), digits = 3),
                 c(.01,.2,.4,.6,.8), na.rm=T)
  }
}

##Housing Data

setwd("C:/Users/zheng/Desktop/MUSA508_Midterm")
Boulder_boundary <- st_read("City_of_Boulder_City_Limits.kml") %>%
  st_transform('EPSG:26913')%>%
  select(-Name, -Description)
Boulder_boundary <- st_union(Boulder_boundary)

housing <- st_read("studentData.geojson", crs = 'ESRI:102254')%>%
  st_transform('EPSG:26913')%>%
  st_intersection(Boulder_boundary,housing)

housing <- housing [!is.na(housing$price),]
housing <- housing [!is.na(housing$geometry),]

ggplot() +
  geom_sf(data = Boulder_boundary, fill = "grey40") +
  geom_sf(data = housing, aes(colour = q5(na.omit(price))), 
          show.legend = "point", size = .75) +
  scale_colour_manual(values = palette5,
                      labels=qBr(housing,"price"),
                      name="Quintile\nBreaks") +
  labs(title="House price distribution at Boulder") +
  mapTheme()

##Public Facilities

# Parks
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

#restaurants

restaurant <- add_osm_feature(opq = q0, key = 'amenity', value = "restaurant") %>%
  osmdata_sf(.)

restaurant.sf <- st_geometry(restaurant$osm_points) %>%
  st_transform(4326) %>%
  st_sf() %>%
  cbind(., restaurant$osm_points$amenity) %>%
  rename(NAME = restaurant.osm_points.amenity)%>%
  st_transform('EPSG:26913')%>%
  st_intersection(Boulder_boundary,restaurant.sf)%>%
  dplyr::select(geometry)%>%
  distinct()

#company

company <- add_osm_feature(opq = q0, key = 'office', value = "company") %>%
  osmdata_sf(.)

company.sf <- st_geometry(company$osm_points) %>%
  st_transform(4326) %>%
  st_sf() %>%
  cbind(., company$osm_points$office) %>%
  rename(NAME = company.osm_points.office)%>%
  st_transform('EPSG:26913')%>%
  st_intersection(Boulder_boundary,company.sf)%>%
  dplyr::select(geometry)%>%
  distinct()

#cafe

cafe <- add_osm_feature(opq = q0, key = 'amenity', value = "cafe") %>%
  osmdata_sf(.)

cafe.sf <- st_geometry(cafe$osm_points) %>%
  st_transform(4326) %>%
  st_sf() %>%
  cbind(., cafe$osm_points$amenity) %>%
  rename(NAME = cafe.osm_points.amenity)%>%
  st_transform('EPSG:26913')%>%
  st_intersection(Boulder_boundary,cafe.sf)%>%
  dplyr::select(geometry)%>%
  distinct()

#fast food

fast_food <- add_osm_feature(opq = q0, key = 'amenity', value = "fast_food") %>%
  osmdata_sf(.)

fast_food.sf <- st_geometry(fast_food$osm_points) %>%
  st_transform(4326) %>%
  st_sf() %>%
  cbind(., fast_food$osm_points$amenity) %>%
  rename(NAME = fast_food.osm_points.amenity)%>%
  st_transform('EPSG:26913')%>%
  st_intersection(Boulder_boundary,fast_food.sf)%>%
  dplyr::select(geometry)%>%
  distinct()

#water

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

#playground 

playground <- add_osm_feature(opq = q0, key = 'leisure', value = "playground") %>%
  osmdata_sf(.)

playground.sf <- st_geometry(playground$osm_points) %>%
  st_transform(4326) %>%
  st_sf() %>%
  cbind(., playground$osm_points$name) %>%
  rename(NAME = playground.osm_points.name)%>%
  st_transform('EPSG:26913')%>%
  st_intersection(Boulder_boundary,playground.sf)%>%
  dplyr::select(geometry)%>%
  distinct()


##School District

school <- add_osm_feature(opq = q0, key = 'amenity', value = "school") %>%
  osmdata_sf(.)

school.sf <- st_geometry(school$osm_points) %>%
  st_transform(4326) %>%
  st_sf() %>%
  cbind(., school$osm_points$amenity) %>%
  rename(NAME = school.osm_points.amenity)%>%
  st_transform('EPSG:26913')%>%
  st_intersection(Boulder_boundary,school.sf)%>%
  dplyr::select(geometry)%>%
  distinct()

#Bus station

bus_station <- add_osm_feature(opq = q0, key = 'amenity', value = "bus_station") %>%
  osmdata_sf(.)

bus_station.sf <- st_geometry(bus_station$osm_points) %>%
  st_transform(4326) %>%
  st_sf() %>%
  cbind(., bus_station$osm_points$amenity) %>%
  rename(NAME = bus_station.osm_points.amenity)%>%
  st_transform('EPSG:26913')%>%
  st_intersection(Boulder_boundary,bus_station.sf)

#median income by census tracts
census_api_key("744a6c627b7cc93310688f5ce408189636167475", install = TRUE)
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

#Split the ¡®to Predict¡¯ == 0 into a separate training and test set using a 75/25 split.
inTrain<-createDataPartition(
  y=housing$price,
  p=.75,list=FALSE)
Boulder.training <- housing %>%
  filter(toPredict == 0) %>%
  select(-toPredict)
Boulder.training<-housing[inTrain,]
Boulder.test<-housing[-inTrain,]

#internal characteristics
housing <- housing %>%
  mutate(age = 2021- EffectiveYear)%>%
  filter(price <= 8000000)%>%
  filter(nbrBedRoom < 10, carStorageSF < 3000)

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
st_drop_geometry(housing) %>% 
  dplyr::select(price, nbrBedRoom.cat) %>% #4
  gather(Variable, Value, -price) %>% 
  ggplot(aes(Value, price)) +
  geom_point(size = 1) + 
  labs(title = "Price as a function of number of beds") +
  plotTheme()
#Public Services
st_c <- st_coordinates 
b.sf <- housing
nn_function <- function(measureFrom,measureTo,k) {
  measureFrom_Matrix <-
    as.matrix(measureFrom)
  measureTo_Matrix <-
    as.matrix(measureTo)
  nn <-   
    get.knnx(measureTo, measureFrom, k)$nn.dist
  output <-
    as.data.frame(nn) %>%
    rownames_to_column(var = "thisPoint") %>%
    gather(points, point_distance, V1:ncol(.)) %>%
    arrange(as.numeric(thisPoint)) %>%
    group_by(thisPoint) %>%
    summarize(pointDistance = mean(point_distance)) %>%
    arrange(as.numeric(thisPoint)) %>% 
    dplyr::select(-thisPoint) %>%
    pull()
  
  return(output)  
}
b.sf <- b.sf %>%
  mutate(
    restaurant_nn1 = nn_function(st_c(b.sf), st_c(restaurant.sf), 1),
    restaurant_nn2 = nn_function(st_c(b.sf), st_c(restaurant.sf), 2), 
    restaurant_nn3 = nn_function(st_c(b.sf), st_c(restaurant.sf), 3), 
    restaurant_nn4 = nn_function(st_c(b.sf), st_c(restaurant.sf), 4), 
    restaurant_nn5 = nn_function(st_c(b.sf), st_c(restaurant.sf), 5),
    park_nn1 = nn_function(st_c(b.sf), st_c(park.sf), 1),
    park_nn2 = nn_function(st_c(b.sf), st_c(park.sf), 2), 
    park_nn3 = nn_function(st_c(b.sf), st_c(park.sf), 3), 
    park_nn4 = nn_function(st_c(b.sf), st_c(park.sf), 4), 
    park_nn5 = nn_function(st_c(b.sf), st_c(park.sf), 5),
    company_nn1 = nn_function(st_c(b.sf), st_c(company.sf), 1),
    company_nn2 = nn_function(st_c(b.sf), st_c(company.sf), 2), 
    company_nn3 = nn_function(st_c(b.sf), st_c(company.sf), 3), 
    company_nn4 = nn_function(st_c(b.sf), st_c(company.sf), 4), 
    company_nn5 = nn_function(st_c(b.sf), st_c(company.sf), 5),
    cafe_nn1 = nn_function(st_c(b.sf), st_c(cafe.sf), 1),
    cafe_nn2 = nn_function(st_c(b.sf), st_c(cafe.sf), 2), 
    cafe_nn3 = nn_function(st_c(b.sf), st_c(cafe.sf), 3), 
    cafe_nn4 = nn_function(st_c(b.sf), st_c(cafe.sf), 4), 
    cafe_nn5 = nn_function(st_c(b.sf), st_c(cafe.sf), 5),
    fast_food_nn1 = nn_function(st_c(b.sf), st_c(fast_food.sf), 1),
    fast_food_nn2 = nn_function(st_c(b.sf), st_c(fast_food.sf), 2), 
    fast_food_nn3 = nn_function(st_c(b.sf), st_c(fast_food.sf), 3), 
    fast_food_nn4 = nn_function(st_c(b.sf), st_c(fast_food.sf), 4), 
    fast_food_nn5 = nn_function(st_c(b.sf), st_c(fast_food.sf), 5),
    water_nn1 = nn_function(st_c(b.sf), st_c(water.sf), 1),
    water_nn2 = nn_function(st_c(b.sf), st_c(water.sf), 2), 
    water_nn3 = nn_function(st_c(b.sf), st_c(water.sf), 3), 
    water_nn4 = nn_function(st_c(b.sf), st_c(water.sf), 4), 
    water_nn5 = nn_function(st_c(b.sf), st_c(water.sf), 5),
    playground_nn1 = nn_function(st_c(b.sf), st_c(playground.sf), 1),
    playground_nn2 = nn_function(st_c(b.sf), st_c(playground.sf), 2), 
    playground_nn3 = nn_function(st_c(b.sf), st_c(playground.sf), 3), 
    playground_nn4 = nn_function(st_c(b.sf), st_c(playground.sf), 4), 
    playground_nn5 = nn_function(st_c(b.sf), st_c(playground.sf), 5),
    bus_stop_nn1 = nn_function(st_c(b.sf), st_c(bus_station.sf), 1),
    bus_stop_nn2 = nn_function(st_c(b.sf), st_c(bus_station.sf), 2),
    bus_stop_nn3 = nn_function(st_c(b.sf), st_c(bus_station.sf), 3),
    bus_stop_nn4 = nn_function(st_c(b.sf), st_c(bus_station.sf), 4),
    bus_stop_nn5 = nn_function(st_c(b.sf), st_c(bus_station.sf), 5),
    school_nn1 = nn_function(st_c(b.sf), st_c(school.sf), 1),
    school_nn2 = nn_function(st_c(b.sf), st_c(school.sf), 2),
    school_nn3 = nn_function(st_c(b.sf), st_c(school.sf), 3),
    school_nn4 = nn_function(st_c(b.sf), st_c(school.sf), 4),
    school_nn5 = nn_function(st_c(b.sf), st_c(school.sf), 5)
  )
reg2 <- lm(price ~., data = st_drop_geometry(b.sf) %>%
            dplyr::select(price,restaurant_nn1,restaurant_nn3,restaurant_nn4,restaurant_nn5,
                          park_nn1,park_nn2,park_nn3,park_nn4,park_nn5,
                          company_nn1,company_nn2,company_nn3,company_nn4,company_nn5,
                          cafe_nn1,cafe_nn2,cafe_nn4,cafe_nn5,
                          fast_food_nn1,fast_food_nn2,fast_food_nn3,fast_food_nn4,fast_food_nn5,
                          water_nn1,water_nn2,water_nn3,water_nn4,water_nn5,
                          playground_nn1,playground_nn2,playground_nn3,playground_nn4,playground_nn5,
                          school_nn1,school_nn2,school_nn3,school_nn4,school_nn5
                          ))
summary(reg2)
#OLS for the whole dataset (NOT TRAINING DATA!!)
#internal characteristics
reg <- lm(price ~., data = st_drop_geometry(housing) %>%
            dplyr::select(price,TotalFinishedSF,carStorageSF, mainfloorSF,nbrBedRoom, nbrFullBaths, nbrRoomsNobath, designCodeDscr,Ac, Heating,
                          designCodeDscr,qualityCode,
                          qualityCodeDscr,
                          ConstCodeDscr,
                          ExtWallDscrPrim,
                          IntWall,
                          Roof_CoverDscr,age,nbrBedRoom.cat))
summary(reg)
#add amenities
reg3 <- lm(price ~., data = st_drop_geometry(b.sf) %>%
             dplyr::select(price,restaurant_nn1,restaurant_nn3,restaurant_nn4,restaurant_nn5,
                           park_nn1,park_nn2,park_nn3,park_nn4,park_nn5,
                           company_nn1,company_nn2,company_nn3,company_nn4,company_nn5,
                           cafe_nn1,cafe_nn2,cafe_nn4,cafe_nn5,
                           fast_food_nn1,fast_food_nn2,fast_food_nn3,fast_food_nn4,fast_food_nn5,
                           water_nn1,water_nn2,water_nn3,water_nn4,water_nn5,
                           playground_nn1,playground_nn2,playground_nn3,playground_nn4,playground_nn5,
                           school_nn1,school_nn2,school_nn3,school_nn4,school_nn5,TotalFinishedSF,carStorageSF, mainfloorSF,nbrBedRoom, nbrFullBaths, nbrRoomsNobath, designCodeDscr,Ac, Heating,
                           designCodeDscr,qualityCode,
                           qualityCodeDscr,
                           ConstCodeDscr,
                           ExtWallDscrPrim,
                           IntWall,
                           Roof_CoverDscr,age,nbrBedRoom.cat
             ))
summary(reg3)
#add spatial structure

#Spatial Structure
neighborhoods <- Median_income%>%
  dplyr::select(-MedianInc,-MedHHInc)

#the spatial lag
Boulder_neigh <-st_join(housing, neighborhoods, join = st_intersects)
housing<-st_join(Boulder_neigh,Median_income,join = st_intersects)
coords <- st_coordinates(housing)
neighborList <- knn2nb(knearneigh(coords, 5))
spatialWeights <- nb2listw(neighborList, style="W")
housing$lagPrice <- lag.listw(spatialWeights,
                                housing$price)

reg.training <-
  lm(price ~ ., data = as.data.frame(housing) %>%
       dplyr::select(price, TotalFinishedSF, carStorageSF, nbrBedRoom, nbrRoomsNobath, nbrFullBaths, designCodeDscr,
                     qualityCodeDscr,
                     ConstCodeDscr,HeatingDscr,
                     ExtWallDscrPrim,
                     IntWallDscr,
                     Roof_CoverDscr,HeatingDscr,
                     ExtWallDscrPrim,
                     IntWallDscr,
                     Roof_CoverDscr,age,nbrBedRoom.cat))
Boulder.test <-
  Boulder.test %>%
  mutate(Regression = "Baseline Regression",
         price.Predict = predict(reg.training, Boulder.test),
         price.Error = price.Predict - price,
         price.AbsError = abs(price.Predict - price),
         price.APE = (abs(price.Predict - price)) /
           price.Predict)%>%
  filter(price < 8000000)

coords.test <- st_coordinates(Boulder.test)
neighborList.test <- knn2nb(knearneigh(coords.test, 5))
spatialWeights.test <- nb2listw(neighborList.test, style="W")
Boulder.test %>%
  mutate(lagPriceError = lag.listw(spatialWeights.test,
                                   price.Error)) %>%
  ggplot(aes(lagPriceError, price.Error))

#Moran's I 
moranTest <- moran.mc(Boulder.test$price.Error,
                      spatialWeights.test, nsim = 999)
ggplot(as.data.frame(moranTest$res[c(1:999)]),
       aes(moranTest$res[c(1:999)])) +
  geom_histogram(binwidth = 0.01) +
  geom_vline(aes(xintercept = moranTest$statistic),
             colour = "#FA7800",size=1) +
  scale_x_continuous(limits = c(-1, 1)) +
  labs(title="Observed and permuted Moran's I",
       subtitle= "Observed Moran's I in orange",
       x="Moran's I",
       y="Count") +
  plotTheme()
