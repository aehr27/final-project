

library(terra)
library(sf)
library(tidyverse)
library(readxl)
library(dplyr)
library(stringr)
library(tmap)
library(ggplot2)
library(spdep)
library(spatialreg)

# importing data

canopy.2011 <- rast("./nlcd_tcc_PRUSVI_2011_v2021-4/nlcd_tcc_PRUSVI_2011_v2021-4.tif")
canopy.2021 <- rast("./nlcd_tcc_PRUSVI_2021_v2021-4/nlcd_tcc_PRUSVI_2021_v2021-4.tif")

canopy.2011
canopy.2021

municipios <- st_read("./GOVTUNIT_Puerto_Rico_State_Shape/Shape/GU_CountyOrEquivalent.shp")

plot(st_geometry(municipios))

pop2011 <- read_excel("prm-est2011-01.xls", skip = 1)

pop2021 <- read_excel("prm-est2021-pop.xlsx", skip = 1)

head(pop2011)
head(pop2021)

# clean population data

names(pop2011)
view(pop2011)

pr.total.2011 <- pop2011[3, 5] |> pull()

pop2011.clean <- pop2011 %>%
  slice(4:81) %>%
  rename(Municipio = 1, Pop2011 = 5) %>%
  mutate(
    Municipio = gsub("^\\.| Municipio$", "", Municipio),
    Pop2011 = as.numeric(Pop2011)
  )


view(pop2021)

pr.total.2021 <- pop2021[3, 4] |> pull()

pop2021.clean <- pop2021 %>%
  slice(4:81) %>%
  rename(Municipio = 1, Pop2021 = 4) %>%
  mutate(
    Municipio = gsub("^\\.", "", Municipio),
    Municipio = gsub(" Municipio, Puerto Rico$", "", Municipio),
    Municipio = trimws(Municipio),
    Pop2021 = as.numeric(Pop2021)
  )

view(pop2021.clean)

#merging data

str(municipios)

municipios.clean <- municipios %>%
  rename(Municipio = county_nam)

view(municipios.clean)

pop.data <- left_join(municipios.clean, pop2011.clean, by = "Municipio") %>%
  left_join(pop2021.clean, by = "Municipio")

view(pop.data)

head(pop.data[c("Municipio", "Pop2011", "Pop2021")])


# canopy data!

class(pop.data)
pop.vector <- terra::vect(pop.data)

class(pop.vector)

canopy.2011
names(canopy.2011)
crs(canopy.2011)

crs(pop.vector)

canopy.2011.projected <- project(canopy.2011, crs(pop.vector),
                                 method = "bilinear")
canopy.2021.projected <- project(canopy.2021, crs(pop.vector),
                                 method = "bilinear")

canopy.mean.2011 <- terra::extract(canopy.2011.projected, 
                                   pop.vector, fun = mean, na.rm = TRUE)
canopy.mean.2021 <- terra::extract(canopy.2021.projected, 
                                   pop.vector, fun = mean, na.rm = TRUE)

str(canopy.mean.2011)
names(canopy.mean.2011) <- c("Municipio_ID", "Canopy_2011")
names(canopy.mean.2021) <- c("Municipio_ID", "Canopy_2021")

pop.data$Municipio_ID <- 1:nrow(pop.data)

canopy.mean.2011$Municipio_ID <- 1:nrow(canopy.mean.2011)
canopy.mean.2021$Municipio_ID <- 1:nrow(canopy.mean.2021)
view(canopy.mean.2011)
view(canopy.mean.2021)

pop.data <- left_join(pop.data, canopy.mean.2011, by = "Municipio_ID")
pop.data <- left_join(pop.data, canopy.mean.2021, by = "Municipio_ID")

print(st_geometry(pop.data))

head(pop.data)

summary(pop.data$Canopy_2011)
summary(pop.data$Canopy_2021)

# analysis?

map_2011 <- tm_shape(pop.data) +
  tm_borders() +
  tm_fill("Canopy_2011", palette = "brewer.greens", title = "Canopy Cover 2011") +
  tm_layout(main.title = "Forest Canopy Cover 2011", main.title.size = 1.5)

map_2021 <- tm_shape(pop.data) +
  tm_borders() +
  tm_fill("Canopy_2021", palette = "brewer.greens", title = "Canopy Cover 2021") +
  tm_layout(main.title = "Forest Canopy Cover 2021", main.title.size = 1.5)

tmap_arrange(map_2011, map_2021, ncol = 2)

# correlation analysis of FCC (per municipality)

cor(pop.data$Canopy_2011, pop.data$Canopy_2021)

pop.data$Canopy_Decrease_Percent <- 
  ((pop.data$Canopy_2011 - pop.data$Canopy_2021) / pop.data$Canopy_2011) * 100

summary(pop.data$Canopy_Decrease_Percent)

ggplot(pop.data, aes(x = Canopy_Decrease_Percent)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Canopy Cover Decrease (2011-2021) per Municipality",
       x = "Percent Decrease in Canopy Cover",
       y = "Frequency") +
  theme_minimal()

ggplot(pop.data, aes(y = Canopy_Decrease_Percent)) +
  geom_boxplot(fill = "lightgreen", color = "darkgreen") +
  labs(title = "Boxplot of Canopy Cover Decrease (2011-2021) per Municipality",
       y = "Percent Decrease in Canopy Cover") +
  theme_minimal()

ggplot(data = pop.data) +
  geom_sf(aes(fill = Canopy_Decrease_Percent), color = "white", size = 0.2) +
  scale_fill_gradient2(
    low = "darkgreen", mid = "yellow", high = "purple",
    midpoint = median(pop.data$Canopy_Decrease_Percent, na.rm = TRUE),
    name = "% Canopy Loss"
  ) +
  labs(
    title = "Percent Decrease in Forest Canopy Cover by Municipality (2011–2021)",
    subtitle = "Purple indicates greater canopy loss, Green indicates canopy gain",
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "right"
  )


# population data analysis

pop.data$Pop_Change <- pop.data$Pop2021 - pop.data$Pop2011
pop.data$Pop_Change_Percent <- 
  ((pop.data$Pop2021 - pop.data$Pop2011) / pop.data$Pop2011) * 100


summary(pop.data$Pop_Change_Percent)

ggplot(data = pop.data) +
  geom_sf(aes(fill = Pop_Change_Percent), color = "white", size = 0.2) +
  scale_fill_gradient2(
    low = "darkorange", mid = "yellowgreen", high = "blue",
    midpoint = 0,
    name = "% Population Change"
  ) +
  labs(
    title = "Population Change by Municipality (2011–2021)",
    subtitle = "Orange indicates population decline; Blue indicates growth",
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "right"
  )

#model that accounts for clusters

model <- lm(Canopy_Decrease_Percent ~ Pop_Change_Percent, data = pop.data)

pop.data$residuals <- residuals(model)

neighbors <- poly2nb(pop.sp, queen = TRUE)
weights <- nb2listw(neighbors, style = "W", zero.policy = TRUE)

moran.test(pop.data$residuals, weights, zero.policy = TRUE)


lag.model <- lagsarlm(Canopy_Decrease_Percent ~ Pop_Change_Percent, data = pop.sp, listw = weights, method = "eigen")
summary(lag.model)

#new and BETTER charts/graphs

pop.sp$fitted <- fitted(lag.model)

class(pop.sp)

pop.sp <- st_as_sf(pop.sp)

head(pop.sp$fitted)
pop.sp$fitted <- fitted(lag.model)

coef_table <- data.frame(
  Coefficient = c("(Intercept)", "Pop_Change_Percent"),
  Estimate = c(1.129524, -0.011530),
  `Std. Error` = c(1.105718, 0.080733),
  `p-value` = c(0.3070, 0.8864)
)

ggplot(coef_table, aes(x = Coefficient, y = Estimate)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = Estimate - `Std. Error`, ymax = Estimate + `Std. Error`), width = 0.2) +
  labs(title = "Model Coefficients with Standard Errors", y = "Coefficient Estimate") +
  theme_minimal()

pop.sp$residuals <- residuals(lag.model)

ggplot(pop.sp) +
  geom_sf(aes(fill = residuals)) +
  scale_fill_viridis_c() +
  labs(title = "Residuals of the Spatial Lag Model", fill = "Residuals") +
  theme_minimal()

pop.sp$fitted <- fitted(lag.model)

ggplot(pop.sp, aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Residuals vs. Fitted Values", x = "Fitted Values", y = "Residuals") +
  theme_minimal()

ggplot(pop.sp) +
  geom_sf(aes(fill = fitted)) +
  scale_fill_viridis_c() +
  labs(title = "Fitted Values of the Spatial Lag Model", fill = "Fitted Value") +
  theme_minimal()
