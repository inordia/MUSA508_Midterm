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
library(stargazer)

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

st_c <- st_coordinates

##Housing Data

Boulder_city <- st_read("C:/Users/zheng/Desktop/MUSA508_Midterm/City_of_Boulder_City_Limits.kml") %>%
  st_transform('EPSG:26913')%>%
  select(-Name, -Description)
Boulder_city <- st_union(Boulder_city)

Boulder.county=st_read("https://opendata.arcgis.com/api/v3/datasets/964b8f3b3dbe401bb28d49ac93d29dc4_0/downloads/data?format=kml&spatialRefId=4326")%>%
  st_as_sf()
Boulder.county.reproject<-Boulder.county %>%
  st_transform('EPSG:26913')%>%
  dplyr::select(-Name,-Description)

q0 <- opq(bbox = c(-105.6945,39.91297,-105.0528,40.26396))

st_bbox(Boulder.county.reproject)

housing <- st_read("C:/Users/zheng/Desktop/MUSA508_Midterm/studentData.geojson", crs = 'ESRI:102254')%>%
  st_transform('EPSG:26913')

housing <- housing %>%
  select(-Stories, -UnitCount)%>%
  mutate(age = 2021- EffectiveYear)%>%
  filter(nbrBedRoom < 10, carStorageSF < 3000)

housing <- housing[-2637,]

##Public Facilities

#parks

park <- add_osm_feature(opq = q0, key = 'leisure', value = "park") %>%
  osmdata_sf(.)

park.sf <- st_geometry(park$osm_points) %>%
  st_transform(4326) %>%
  st_sf() %>%
  cbind(., park$osm_points$name) %>%
  rename(NAME = park.osm_points.name)%>%
  st_transform('EPSG:26913')%>%
  st_intersection(Boulder.county.reproject,park.sf)%>%
  dplyr::select(geometry)

housing <- housing%>%
  mutate(park=nn_function(st_c(housing),st_c(park.sf),1))

#fast food

fast_food <- add_osm_feature(opq = q0, key = 'amenity', value = "fast_food") %>%
  osmdata_sf(.)

fast_food.sf <- st_geometry(fast_food$osm_points) %>%
  st_transform(4326) %>%
  st_sf() %>%
  cbind(., fast_food$osm_points$amenity) %>%
  rename(NAME = fast_food.osm_points.amenity)%>%
  st_transform('EPSG:26913')%>%
  st_intersection(Boulder.county.reproject,park.sf)

fast_food.sf<-
  fast_food.sf%>%
  dplyr::select(geometry)

housing$fastfood_buffer =
  st_buffer(housing, 800) %>% ##WHAT IS THE RIGHT PARAMETER
  aggregate(mutate(fast_food.sf, counter = 1),., sum) %>% 
  pull(counter)


#water

water <- add_osm_feature(opq = q0, key = 'natural', value = "water") %>%
  osmdata_sf(.)

water.sf <- water$osm_points %>%
  dplyr::select(geometry) %>%
  st_as_sf(crs = 4326, agr = "constant") %>%
  distinct() %>%
  st_transform('EPSG:26913')%>%
  st_intersection(Boulder.county.reproject,park.sf)

housing <- housing%>%
  mutate(water_nn1=nn_function(st_c(housing),st_c(water.sf),1),
         water_nn2=nn_function(st_c(housing),st_c(water.sf),2),
         water_nn3=nn_function(st_c(housing),st_c(water.sf),3))

#playground 

playground <- add_osm_feature(opq = q0, key = 'leisure', value = "playground") %>%
  osmdata_sf(.)

playground.sf <- st_geometry(playground$osm_points) %>%
  st_transform(4326) %>%
  st_sf() %>%
  cbind(., playground$osm_points$name) %>%
  rename(NAME = playground.osm_points.name)%>%
  st_transform('EPSG:26913')%>%
  st_intersection(Boulder.county.reproject,park.sf)%>%
  dplyr::select(geometry)

housing <- housing%>%
  mutate(playground=nn_function(st_c(housing),st_c(playground.sf),1))

#restaurant
restaurant <- add_osm_feature(opq = q0, key = 'amenity', value = "restaurant") %>%
  osmdata_sf(.)

restaurant.sf <- st_geometry(restaurant$osm_points) %>%
  st_transform(4326) %>%
  st_sf() %>%
  cbind(., restaurant$osm_points$amenity) %>%
  rename(NAME = restaurant.osm_points.amenity)%>%
  st_transform('EPSG:26913')%>%
  st_intersection(Boulder.county.reproject,park.sf)%>%
  dplyr::select(geometry)%>%
  distinct()

housing<-housing%>%
  mutate(
    restaurant_nn1 = nn_function(st_c(housing), st_c(restaurant.sf), 1),
    restaurant_nn2 = nn_function(st_c(housing), st_c(restaurant.sf), 2), 
    restaurant_nn3 = nn_function(st_c(housing), st_c(restaurant.sf), 3))

##School District

school <- read.csv("C:/Users/zheng/Desktop/MUSA508_Midterm/school.csv")%>%
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

#Company

company <- add_osm_feature(opq = q0, key = 'office', value = "company") %>%
  osmdata_sf(.)

company.sf <- st_geometry(company$osm_points) %>%
  st_transform(4326) %>%
  st_sf() %>%
  cbind(., company$osm_points$office) %>%
  rename(NAME = company.osm_points.office)%>%
  st_transform('EPSG:26913')%>%
  st_intersection(Boulder.county.reproject,park.sf)%>%
  na.omit()

housing <- housing %>%
  mutate(company=nn_function(st_c(housing),st_c(company.sf),1))

#Bus station

bus_station <- add_osm_feature(opq = q0, key = 'amenity', value = "bus_station") %>%
  osmdata_sf(.)

bus_station.sf <- st_geometry(bus_station$osm_points) %>%
  st_transform(4326) %>%
  st_sf() %>%
  cbind(., bus_station$osm_points$amenity) %>%
  rename(NAME = bus_station.osm_points.amenity)%>%
  st_transform('EPSG:26913')%>%
  dplyr::select(geometry)%>%
  st_intersection(Boulder.county.reproject,park.sf)


housing <- housing%>%
  mutate(bus_stop_nn1=nn_function(st_c(housing),st_c(bus_station.sf),1),
         bus_stop_nn2=nn_function(st_c(housing),st_c(bus_station.sf),2),
         bus_stop_nn3=nn_function(st_c(housing),st_c(bus_station.sf),3))

#urban area
urban_area <- urban_areas(cb = FALSE, year = NULL)%>%
  st_transform('EPSG:26913')%>%
  select(NAME10, geometry)
urban_area <- st_intersection(Boulder.county.reproject, urban_area)%>%
  filter(NAME10 != "Denver--Aurora, CO")

##Spatial Process
neighborhood <- get_acs(geography = "tract",
                        year = 2019, 
                        variables = c("B06011_001E", #Median income in the past 12 months
                                      "B19013_001E"), #Median household income in the past 12 months
                        geometry = T, 
                        state = "CO", 
                        county = "Boulder", 
                        output = "wide") %>%
  st_transform('EPSG:26913')%>%
  dplyr::select(-NAME,-B06011_001M,-B19013_001M,-B06011_001E,-B19013_001E)

#internal characteristics

##Feature Engineering: Internal Characteristics
housing <- 
  housing %>%
  mutate(nbrBedRoom.cat = case_when(
    nbrBedRoom >= 0 & nbrBedRoom < 4  ~ "Up to 3 Bedrooms",
    nbrBedRoom >= 4 & nbrBedRoom < 5  ~ "4 Bedrooms",
    nbrBedRoom > 4                    ~ "5+ Bedrooms"))

#spatial structure

housing <- st_join(housing, neighborhood, join = st_intersects)

housing <- st_join(housing, urban_area, join = st_intersects)

housing$NAME10[is.na(housing$NAME10)] <- 0

housing <- housing %>%
  rename(urban_status = NAME10)

housing$urban_status <- ifelse(housing$urban_status == "0", "non-urban", "urban")

#the spatial lag of housing
Boulder <- housing %>%
  filter(toPredict == 0) %>%
  dplyr::select(-toPredict)
coords <- st_coordinates(Boulder)
neighborList <- knn2nb(knearneigh(coords, 5))
spatialWeights <- nb2listw(neighborList, style="W")
Boulder$lagPrice <- lag.listw(spatialWeights,
                              Boulder$price)

#Present a table of summary statistics with variable descriptions.
vars<-st_drop_geometry(Boulder)%>%
  select(
    age,
    TotalFinishedSF,
    HeatingDscr,
    Roof_CoverDscr,
    park,
    school,
    restaurant_nn1,
    bus_stop_nn1,
    company,
    water_nn1,
    lagPrice)
stargazer(vars,type = "text",title = "summary statistics with variable descriptions")

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

#Present 4 home price correlation scatter plots that you think are of interest.
st_drop_geometry(housing) %>% 
  dplyr::select(price, restaurant_nn1, water_nn1, bus_stop_nn1,park) %>% 
  gather(Variable, Value, -price) %>% 
  ggplot(aes(Value, price)) +
  geom_point(size = .5) + 
  geom_smooth(method = "lm", se=T, colour = "#FA7800") +
  facet_wrap(~Variable, ncol = 2, scales = "free") +
  labs(title = "home price correlation scatter plots") +
  plotTheme()

#Develop 1 map of your dependent variable (sale price)
ggplot()  +
  geom_sf(data = Boulder.county.reproject, fill = "grey40") +
  geom_sf(data = housing, aes(colour = q5(price)), 
          show.legend = "point", size = .75) +
  scale_colour_manual(values = palette5,
                      labels=qBr(housing,"price"),
                      name="Quintile\nBreaks") +
  labs(title="Sales price distribution in Boulder") +
  mapTheme()

#Develop 3 maps of 3 of your most interesting independent variables.
#urban status
ggplot() +
  geom_sf(data = Boulder.county.reproject, fill = "grey40") +
  geom_sf(data = housing, aes(colour = urban_status), 
          show.legend = "point", size = .75) +
  labs(title="Urban status in Boulder") +
  mapTheme()

#distance to park
ggplot() +
  geom_sf(data = Boulder.county.reproject, fill = "grey40") +
  geom_sf(data = housing, aes(colour = q5(park)), 
          show.legend = "point", size = .75) +
  scale_colour_manual(values = palette5,
                      labels=qBr(housing,"park"),
                      name="Quintile\nBreaks") +
  labs(title="Distance to park, Boulder") +
  mapTheme()

#distance to restaurant
ggplot() +
  geom_sf(data = Boulder.county.reproject, fill = "grey40") +
  geom_sf(data = housing, aes(colour = q5(restaurant_nn1)), 
          show.legend = "point", size = .75) +
  scale_colour_manual(values = palette5,
                      labels=qBr(housing,"restaurant_nn1"),
                      name="Quintile\nBreaks") +
  labs(title="Distance to restaurant, Boulder") +
  mapTheme()

#Include any other maps/graphs/charts you think might be of interest.

st_drop_geometry(housing) %>% 
  dplyr::select(price, nbrBedRoom.cat) %>% #4
  gather(Variable, Value, -price) %>% 
  ggplot(aes(Value, price)) +
  geom_point(size = 1) + 
  labs(title = "Price as a function of number of beds") +
  plotTheme()

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
                           restaurant_nn1,
                           bus_stop_nn1,
                           company,
                           water_nn1,
                           urban_status))
summary(reg1)

#split the dataset into training and testing

inTrain <- createDataPartition(
  y = paste(Boulder$designCodeDscr,
            Boulder$nbrBedRoom.cat,
            Boulder$HeatingDscr,
            Boulder$Roof_CoverDscr,
            Boulder$GEOID,
            Boulder$urban_status), 
  p = .75, list = FALSE)

Boulder.training <- Boulder[inTrain,]
Boulder.testing  <- Boulder[-inTrain,] 
housing.test.nhood <- Boulder.testing

reg.training <-
  lm(price ~ ., data = as.data.frame(Boulder.training) %>%
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
                     restaurant_nn1,
                     bus_stop_nn1,
                     company,
                     water_nn1,
                     urban_status,
                     lagPrice))
Boulder.testing <-
  Boulder.testing %>%
  mutate(Regression = "Baseline Regression",
         price.Predict = predict(reg.training, Boulder.testing),
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
                        urban_status,
                        park,
                        school,
                        restaurant_nn1,
                        bus_stop_nn1,
                        company,
                        water_nn1,
                        lagPrice), 
        method = "lm", trControl = fitControl, na.action = na.pass)

reg.cv

mean(reg.cv$resample[,3])

#Spatial Structure

##Feature Engineering: Income
##Reg.nhood
reg.nhood <- lm(price ~ ., data = st_drop_geometry(Boulder.training) %>% 
                  dplyr::select(price,
                                age,
                                GEOID,
                                designCodeDscr,
                                qualityCodeDscr,
                                TotalFinishedSF,
                                nbrBedRoom.cat,
                                urban_status,
                                HeatingDscr,
                                Roof_CoverDscr,
                                park,
                                school,
                                restaurant_nn1,
                                bus_stop_nn1,
                                company,
                                water_nn1,
                                lagPrice))
summary(reg.nhood)

##Accuracy for Neighborhood Model

reg.nhood.training <- lm(price ~ ., data = st_drop_geometry(Boulder.training) %>% 
                           dplyr::select(price,
                                         age,
                                         GEOID,
                                         designCodeDscr,
                                         qualityCodeDscr,
                                         TotalFinishedSF,
                                         nbrBedRoom.cat,
                                         urban_status,
                                         HeatingDscr,
                                         Roof_CoverDscr,
                                         park,
                                         school,
                                         restaurant_nn1,
                                         bus_stop_nn1,
                                         bus_stop_nn3,
                                         company,
                                         water_nn1,
                                         lagPrice))


housing.test.nhood <-
  housing.test.nhood %>%
  mutate(Regression = "Neighbourhood effects",
         price.Predict = predict(reg.nhood.training, housing.test.nhood),
         price.Error = price.Predict - price,
         price.AbsError = abs(price.Predict - price),
         price.APE = (abs(price.Predict - price)) /
           price.Predict)%>%
  filter(price < 8000000)

#Provide a polished table of mean absolute error and MAPE for a single test set.

# errors of housing.test.nhood
coords_1 <-  st_coordinates(housing.test.nhood) 
neighborList_1 <- knn2nb(knearneigh(coords_1, 5))
spatialWeights_1 <- nb2listw(neighborList_1, style="W")
housing.test.nhood$lagPriceError <- lag.listw(spatialWeights_1, housing.test.nhood$price.AbsError)

coords_2 <- st_coordinates(Boulder.testing)
neighborList_2 <- knn2nb(knearneigh(coords_2, 5))
spatialWeights_2 <- nb2listw(neighborList_2, style="W")
Boulder.testing$lagPriceError <- lag.listw(spatialWeights_2,
                                           Boulder.testing$price.AbsError)

comparison <- 
  rbind(
    dplyr::select(Boulder.testing, starts_with("price"), Regression) %>%
      mutate(lagPriceError = lag.listw(spatialWeights_2, price.Error)),
    dplyr::select(housing.test.nhood, starts_with("price"), Regression) %>%
      mutate(lagPriceError = lag.listw(spatialWeights_1, price.Error))) 

st_drop_geometry(comparison) %>%
  gather(Variable, Value, -Regression) %>%
  filter(Variable == "price.AbsError" | Variable == "price.APE") %>%
  group_by(Regression, Variable) %>%
  summarize(meanValue = mean(Value, na.rm = T)) %>%
  spread(Variable, meanValue) %>%
  kable() %>%
  kable_styling("striped", full_width = F) %>%
  row_spec(1, color = "black", background = "#25CB10") %>%
  row_spec(2, color = "black", background = "#FA7800") %>%
  footnote(general_title = "\n",
           general = "Table of mean absolute error and MAPE for a single test set")

#Do 100 folds and plot your cross-validation MAE as a histogram. 
reg.cv$resample %>% 
  pivot_longer(-Resample) %>% 
  mutate(name = as.factor(name)) %>% 
  ggplot(., aes(x = value, color = name)) +
  geom_histogram(bins = 30, colour="black", fill = "#FDE725FF") +
  facet_wrap(~name, ncol = 3, scales = "free") +
  theme_bw() +
  theme(
    legend.position = "none"
  )

MAE_mean<- mean(reg.cv$resample[,3])
MAE_SD<- sd(reg.cv$resample[,3])

compare<- data.frame(MAE_mean=MAE_mean,
                     MAE_SD=MAE_SD)
compare

#Plot predicted prices as a function of observed prices
housing.test.nhood1<-housing.test.nhood%>%
  filter(price<5500000, price.Predict<5500000&price.Predict>0)

ggplot(housing.test.nhood1,aes(price, price.Predict)) +
  geom_point() +
  stat_smooth(data=housing.test.nhood1,aes(price, price),
              method = "lm", se = FALSE, size = 1, colour="#FA7800") +
  stat_smooth(data=housing.test.nhood1,aes(price,price.Predict),
              method = "lm", se = FALSE, size = 1, colour="#25CB10") +
  facet_wrap(~Regression) +
  labs(title="Predicted sale price as a function of observed price",
       subtitle="Orange line represents a perfect prediction;
Green line represents prediction") +
  plotTheme()

#Provide a map of your residuals for your test set.Include a Moran's I test and a plot of the spatial lag in errors.
#Map of residuals
ggplot() +
  geom_sf(data = Boulder.county.reproject, fill = "grey40") +
  geom_sf(data = housing.test.nhood, aes(colour = q5(price.AbsError)), 
          show.legend = "point", size = 1) +
  scale_colour_manual(values = palette5,
                      labels=qBr(housing.test.nhood,"price.AbsError"),
                      name="Quintile\nBreaks") +
  labs(title="Map of Residuals for the test set") +
  mapTheme()

#Moran's I 
moranTest <- moran.mc(housing.test.nhood$price.AbsError,
                      spatialWeights_1, nsim = 999)
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

#plot of the spatial lag in errors
ggplot(housing.test.nhood, aes(x=lagPriceError, y=price)) +
  geom_point(colour = "#FA7800") +
  geom_smooth(method = "lm", se = FALSE, colour = "#25CB10") +
  labs(title = "plot of the spatial lag in errors",
       x = "Spatial lag of errors",
       y = "price") +
  plotTheme()

#Provide a map of your predicted values for where toPredict is both 0 and 1.(?)
ggplot() +
  geom_sf(data = Boulder.county.reproject, fill = "grey40") +
  geom_sf(data = housing.test.nhood, aes(colour = q5(price.Predict)), 
          show.legend = "point", size = 1) +
  scale_colour_manual(values = palette5,
                      labels=qBr(housing.test.nhood,"price.Predict"),
                      name="Quintile\nBreaks") +
  labs(title="map of predicted values for where toPredict is both 0 and 1") +
  mapTheme()

#Using the test set predictions, provide a map of mean absolute percentage error(MAPE) by neighborhood.

st_drop_geometry(housing.test.nhood)%>%
  group_by(GEOID) %>% 
  summarise(MAPE = mean(price.APE, na.rm = T))%>%
  ungroup()%>%
  left_join(neighborhoods)%>%
  st_sf()%>%
  ggplot() +
  geom_sf(aes(fill = MAPE)) +
  geom_sf(data = housing.test.nhood, colour ="black", size =.5) +
  scale_fill_gradient(low = palette5[1], high = palette5[5],
                      name = "MAPE") +
  labs(title = "map of mean absolute percentage error(MAPE) by neighborhood") +
  mapTheme()

#Provide a scatterplot plot of MAPE by neighborhood as a function of mean price by neighborhood.
plot(nhood.summary$meanPrice, 
     nhood.summary$MAPE, 
     main="MAPE by neighborhood as a function of mean price by neighbourhood", 
     cex.main=0.75, ylim=range(0:1))


#Using tidycensus, split your city into two groups (by income) and test your model's generalizability. Is your model generalizable?
tracts19<- get_acs(geography = "tract", variables = c("B01001_001E","B01001A_001E","B06011_001"), 
                   year = 2019,                              
                   geometry = T, 
                   state = "CO", 
                   county = "Boulder", 
                   output = "wide") %>%
  st_transform('EPSG:26913')  %>%
  rename(Median_Income = B06011_001E)  %>%
  mutate(incomeContext = ifelse(Median_Income > 40453, "High Income", "Low income"))

ggplot() + 
  geom_sf(data = na.omit(tracts19),
          aes(fill = incomeContext)) +
  scale_fill_manual(values = c("#25CB10", "#FA7800"),
                    name="Income Context") +
  labs(title = "Income Context") +
  mapTheme() + theme(legend.position="bottom")


st_join(comparison, tracts19) %>% 
  group_by(Regression, incomeContext) %>%
  summarize(mean.MAPE = scales::percent(mean(price.APE, na.rm = T))) %>%
  st_drop_geometry() %>%
  spread(incomeContext, mean.MAPE) %>%
  kable(caption = "Table set MAPE by neighbourhood income context") %>%
  kable_styling("striped", full_width = F) %>%
  row_spec(1, color = "black", background = "#25CB10") %>%
  row_spec(2, color = "black", background = "#FA7800") 



