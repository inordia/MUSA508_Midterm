
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

Boulder_boundary <- st_read("/Users/inordia/Desktop/UPenn搞起来/592/MUSA508_Midterm/City_of_Boulder_City_Limits.kml") %>%
  st_transform('EPSG:26913')%>%
  select(-Name, -Description)
Boulder_boundary <- st_union(Boulder_boundary)

st_bbox(Boulder_boundary)

housing <- st_read("/Users/inordia/Desktop/UPenn搞起来/592/MUSA508_Midterm/studentData.geojson", crs = 'ESRI:102254')%>%
  st_transform('EPSG:26913')

housing <- housing %>%
  select(-Stories, -UnitCount)%>%
  mutate(age = 2021- EffectiveYear)%>%
  filter(nbrBedRoom < 10, carStorageSF < 3000)

housing <- housing[-2637,]


ggplot()  +
  geom_sf(data = housing, aes(colour = q5(price)), 
          show.legend = "point", size = .75) +
  scale_colour_manual(values = palette5,
                      labels=qBr(housing,"price"),
                      name="Quintile\nBreaks") +
  labs(title="Price Per Square Foot, Boulder") +
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
  dplyr::select(geometry)

housing<-housing%>%
  mutate(
    park_nn1 = nn_function(st_c(housing), st_c(park.sf), 1),
    park_nn2 = nn_function(st_c(housing), st_c(park.sf), 2), 
    park_nn3 = nn_function(st_c(housing), st_c(park.sf), 3))

#fast food

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
  dplyr::select(geometry)

housing$fastfood_buffer =
  st_buffer(housing, 8000) %>% ##WHAT IS THE RIGHT PARAMETER
  aggregate(mutate(fast_food.sf, counter = 1),., sum) %>% 
  pull(counter)


#water

water <- st_read("/Users/inordia/Desktop/UPenn搞起来/592/MUSA508_Midterm/Lakes_and_Reservoirs/Lakes_and_Reservoirs.shp")%>%
  st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant")%>%
  st_transform('EPSG:26913')

housing <- housing%>%
  mutate(water_nn1=nn_function(st_c(housing),st_c(water),1),
         water_nn2=nn_function(st_c(housing),st_c(water),2),
         water_nn3=nn_function(st_c(housing),st_c(water),3))

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
  dplyr::select(geometry)

housing <- housing%>%
  mutate(playground=nn_function(st_c(housing),st_c(playground.sf),1))
housing <- housing%>%
  mutate(park=nn_function(st_c(housing),st_c(park.sf),1))

#restaurant
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

housing<-housing%>%
  mutate(
    restaurant_nn1 = nn_function(st_c(housing), st_c(restaurant.sf), 1),
    restaurant_nn2 = nn_function(st_c(housing), st_c(restaurant.sf), 2), 
    restaurant_nn3 = nn_function(st_c(housing), st_c(restaurant.sf), 3))

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
  na.omit()
housing <- housing%>%
  mutate(company=nn_function(st_c(housing),st_c(company.sf),1))

#Bus station

bus_station <- read.csv("/Users/inordia/Desktop/UPenn搞起来/592/MUSA508_Midterm/bus_stop.csv")%>%
  st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant")%>%
  st_transform('EPSG:26913')

bus_station.sf <- bus_station%>%
  dplyr::select(geometry) %>%
  st_as_sf(crs = 4326, agr = "constant")%>%
  st_transform('EPSG:26913')

housing <- housing%>%
  mutate(bus_stop_nn1=nn_function(st_c(housing),st_c(bus_station.sf),1),
         bus_stop_nn2=nn_function(st_c(housing),st_c(bus_station.sf),2),
         bus_stop_nn3=nn_function(st_c(housing),st_c(bus_station.sf),3))

#median income by census tracts
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

#internal characteristics
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

##Feature Engineering: Internal Characteristics
housing <- 
  housing %>%
  mutate(nbrBedRoom.cat = case_when(
    nbrBedRoom >= 0 & nbrBedRoom < 4  ~ "Up to 3 Bedrooms",
    nbrBedRoom >= 4 & nbrBedRoom < 5  ~ "4 Bedrooms",
    nbrBedRoom > 4                    ~ "5+ Bedrooms"))
housing<-housing%>%
  mutate(Ac.cat=case_when(
    Ac<200 ~ "not have Ac",
    Ac>=200 ~ "have AC"
  ))

st_drop_geometry(housing) %>% 
  dplyr::select(price, nbrBedRoom.cat) %>% #4
  gather(Variable, Value, -price) %>% 
  ggplot(aes(Value, price)) +
  geom_point(size = 1) + 
  labs(title = "Price as a function of number of beds") +
  plotTheme()


var<-housing%>%
  dplyr::select(price,
                qualityCode,
                age,
                designCodeDscr,
                qualityCodeDscr,
                TotalFinishedSF,
                nbrBedRoom.cat,
                Ac.cat,
                HeatingDscr,
                Roof_CoverDscr,
                park,
                school,
                restaurant_nn1,
                restaurant_nn2,
                restaurant_nn3,
                bus_stop_nn1,
                bus_stop_nn2,
                bus_stop_nn3,
                park_nn1,
                park_nn2,
                park_nn3,
                fastfood_buffer,
                company,
                nbrBedRoom.cat, 
                nbrRoomsNobath, 
                nbrFullBaths)

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
                           fastfood_buffer,
                           restaurant_nn1,
                           bus_stop_nn1,
                           company))
summary(reg1)

#split the dataset into training and testing
Boulder.training <- housing %>%
  filter(toPredict == 0) %>%
  dplyr::select(-toPredict)

inTrain <- createDataPartition(
  y = paste(Boulder.training$designCodeDscr,
  Boulder.training$nbrBedRoom.cat,
  Boulder.training$HeatingDscr,
  Boulder.training$Roof_CoverDscr), 
  p = .75, list = FALSE)

Boulder.training <- Boulder.training[inTrain,]
Boulder.testing  <- Boulder.training[-inTrain,] 

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
                     park_nn1,
                     company))
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
                        park,
                        school,
                        restaurant_nn1,
                        bus_stop_nn1,
                        company), 
        method = "lm", trControl = fitControl, na.action = na.pass)

reg.cv

mean(reg.cv$resample[,3])

Boulder.training.nhood <- housing %>%
  filter(toPredict == 0)%>%
  dplyr::select(-toPredict)

#Spatial Structure
neighborhoods <- Median_income%>%
  dplyr::select(-MedianInc,-MedHHInc)

#the spatial lag of housing
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

##Feature Engineering: Income
##Reg.nhood
reg.nhood <- lm(price ~ ., data = st_drop_geometry(Boulder.training.nhood) %>% 
                  dplyr::select(price,
                                age,
                                GEOID.x,
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
                                lagPrice))
summary(reg.nhood)

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
                                         age,
                                         GEOID.x,
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
                                         bus_stop_nn3,
                                         company,
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
comparison %>%
  dplyr::select(price.Predict, price, Regression) %>%
  ggplot(aes(price, price.Predict)) +
  geom_point() +
  stat_smooth(aes(price, price),
              method = "lm", se = FALSE, size = 1, colour="#FA7800") +
  stat_smooth(aes(price.Predict, price),
              method = "lm", se = FALSE, size = 1, colour="#25CB10") +
  facet_wrap(~Regression) +
  labs(title="Predicted sale price as a function of observed price",
       subtitle="Orange line represents a perfect prediction;
Green line represents prediction") +
  plotTheme()

#Provide a map of your residuals for your test set.Include a Moran??s I test and a plot of the spatial lag in errors.

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

#Provide a map of your predicted values for where ??toPredict?? is both 0 and 1.
ggplot() +
  geom_sf(data = Boulder_boundary, fill = "grey40") +
  geom_sf(data = housing.test.nhood, aes(colour = q5(price.Predict)), 
          show.legend = "point", size = 1) +
  scale_colour_manual(values = palette5,
                      labels=qBr(housing.test.nhood,"price.Predict"),
                      name="Quintile\nBreaks") +
  labs(title="map of your predicted values for where ??toPredict?? is both 0 and 1") +
  mapTheme()

#Using tidycensus, split your city into two groups (perhaps by race or income) and testyour model??s generalizability. Is your model generalizable?
tracts19<- get_acs(geography = "tract", variables = c("B01001_001E","B01001A_001E","B06011_001"), 
                   year = 2019,                              
                   geometry = T, 
                   state = "CO", 
                   county = "Boulder", 
                   output = "wide") %>%
  st_transform('EPSG:26913')  %>%
  rename(TotalPop = B01001_001E,
         Whites = B01001A_001E,
         Median_Income = B06011_001E)  %>%
mutate(percentWhite = Whites / TotalPop,
       raceContext = ifelse(percentWhite > .5, "White majority", "Non-White majority"),
       incomeContext = ifelse(Median_Income > 40453, "High Income", "Low income"))

#tracts19<-st_join(housing.test.nhood,tracts19,join = st_intersects)
grid.arrange(ncol = 2,
             ggplot() + 
               geom_sf(data = na.omit(tracts19),
                                aes(fill = raceContext)) +
               scale_fill_manual(values = c("#25CB10", "#FA7800"),
                                 name="Race Context") +
               labs(title = "Race Context") +
               mapTheme() + theme(legend.position="bottom"),
             ggplot() + 
               geom_sf(data = na.omit(tracts19),
                                aes(fill = incomeContext)) +
               scale_fill_manual(values = c("#25CB10", "#FA7800"),
                                 name="Income Context") +
               labs(title = "Income Context") +
               mapTheme() + theme(legend.position="bottom"))

