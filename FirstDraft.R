# Mortality Rate in Texas 2022
MortalityRateTexas2022 <- read.delim("MortalityRateTexas2022.txt", header = TRUE, stringsAsFactors = FALSE)
# Mortality Rate in New Mexico 2022
MortalityRateNewMexico2022 <- read.delim("MortalityRateNewMexico2022.txt", header = TRUE, stringsAsFactors = FALSE)
# Mortality Rate in California 2022
MortalityRateCalifornia2022 <- read.delim("MortalityRateCalifornia2022.txt", header = TRUE, stringsAsFactors = FALSE)
# Mortality Rate in Arizona 2022
MortalityRateArizona2022 <- read.delim("MortalityRateArizona2022.txt", header = TRUE, stringsAsFactors = FALSE)

# Combine data sets into one
MortalityRateCombined <- rbind(MortalityRateTexas2022, 
                               MortalityRateNewMexico2022, 
                               MortalityRateCalifornia2022, 
                               MortalityRateArizona2022)
# Standardize county names and state abbreviations
MortalityRateCombined$County <- gsub(",", "", MortalityRateCombined$County)
MortalityRateCombined$State <- substr(MortalityRateCombined$County, nchar(MortalityRateCombined$County) - 1, nchar(MortalityRateCombined$County))
MortalityRateCombined$County <- gsub(" TX| NM| AR| CA", "", MortalityRateCombined$County)
#------------------------------#
install.packages("readxl")
library(readxl)
#------------------------------#
#Air Quality data
AirQuality <- read_excel("AirQualityStatisticsByCounty2022 - Cleaned.xlsx")
#Remove extra information/unnecessary rows
AirQuality <- AirQuality[-c(22:60), ]
AirQuality <- AirQuality[-c(1), ]

# Extract the second row as column names
col_names <- as.character(unlist(AirQuality[1, ]))
col_names <- make.names(col_names, unique=TRUE)
colnames(AirQuality) <- col_names

# Remove the first two rows as they are headers
AirQuality <- AirQuality[-c(1,2), ]
colnames(AirQuality) <- c("State", "County", "County_Code", 
                                "Population_2010", "CO", "Pb",
                                "NO2_AM", "NO2_1hr", "O3",
                                "PM10", "PM2.5_Wtd_AM", "PM2.5_24hr",
                                "SO2_1hr")
AirQuality[AirQuality == "ND"] <- NA

#-------------Visualize Air Quality-----------------#

# PM Concentration by State
ggplot(AirQuality, aes(x = State, y = PM10, fill = State)) +
  geom_boxplot() +
  labs(x = "State", y = "PM10 concentration") +
  theme_minimal()  
# PM Concentration by County
ggplot(AirQuality, aes(x = State, y = PM10, color = County)) +
  geom_point() +
  labs(x = "State", y = "PM10 concentration") +
  theme_minimal()

#-------------Water Quality Data----------------#
library(readxl)
#Water Quality data by county
WaterQuality <- read_excel("WaterQualityByCounty2024.xlsx")
WaterQuality <- WaterQuality[-c(1), ]
col_names <- as.character(unlist(WaterQuality[1, ]))
col_names <- make.names(col_names, unique=TRUE)
colnames(WaterQuality) <- col_names
WaterQuality <- WaterQuality[-c(1), ]
# Rename columns
colnames(WaterQuality) <- c("State", "County", "Temperature_C", "Dissolved_Oxygen_mg_L", "pH")
#-------------Water Quality Visualization----------------#
library(ggplot2)
# Plot Temperature vs. Dissolved Oxygen
ggplot(WaterQuality, aes(x = Temperature_C, y = Dissolved_Oxygen_mg_L, color = County)) +
  geom_point() +
  labs(title = "Temperature vs. Dissolved Oxygen",
       x = "Temperature (°C)",
       y = "Dissolved Oxygen (mg/L)",
       color = "County") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank())

# Plot Temperature vs. pH
ggplot(WaterQuality, aes(x = Temperature_C, y = pH, color = County)) +
  geom_point() +
  labs(title = "Temperature vs. pH",
       x = "Temperature (°C)",
       y = "pH",
       color = "County") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank())

#------------PFAS Analysis------------------#

library(readxl)
library(sf)
library(ggplot2)
PFAS_Original2023 <- read_excel("PFAS_Original2023.xlsx")
View(PFAS_Original2023)

#NO WAY TO KNOW which water system belongs to which county without the zip codes

texas_zipcodes <- st_read("tx_texas_zip_codes_geo.min.json")
california_zipcodes <- st_read("ca_california_zip_codes_geo.min.json")
newmexico_zipcodes <- st_read("nm_new_mexico_zip_codes_geo.min.json")
arizona_zipcodes <- st_read("az_arizona_zip_codes_geo.min.json")

# Merge the data for each state
merged_data_tx <- merge(texas_zipcodes, PFAS_Original2023, by.x = "ZCTA5CE10", by.y = "ZIP Codes Served")
merged_data_ca <- merge(california_zipcodes, PFAS_Original2023, by.x = "ZCTA5CE10", by.y = "ZIP Codes Served")
merged_data_nm <- merge(newmexico_zipcodes, PFAS_Original2023, by.x = "ZCTA5CE10", by.y = "ZIP Codes Served")
merged_data_az <- merge(arizona_zipcodes, PFAS_Original2023, by.x = "ZCTA5CE10", by.y = "ZIP Codes Served")
#there is no data in the PFAS for Arizona
# Plot the data for each state



ggplot() +
  geom_sf(data = texas_zipcodes, fill = "cornsilk") +
  geom_sf(data = merged_data_tx, aes(fill = Contaminant)) +
  ggtitle("Texas Water Contaminants 2023") +
  theme_minimal()

ggplot() +
  geom_sf(data = newmexico_zipcodes, fill = "cornsilk") +
  geom_sf(data = merged_data_nm, aes(fill = Contaminant)) +
  ggtitle("New Mexico Water Contaminants 2023") +
  theme_minimal()
ggplot() +
  geom_sf(data = california_zipcodes, fill = "cornsilk") +
  geom_sf(data = merged_data_ca, aes(fill = Contaminant)) +
  ggtitle("California Water Contaminants 2023") +
  theme_minimal()



#After plotting the whole state, its obvious to see that most zip codes are missing from the PFAS data set



#------------------------------#
#Health Outcomes Survey
HealthOutcomes <- read.csv("PlacesByCountyCleaned2022.csv")
# Selecting only specific states and counties
library(dplyr)

# Define the states and counties 
states <- c("Arizona", "California", "Texas", "New Mexico")
counties_arizona <- c("Conchise", "La Paz", "Maricopa", "Pima", "Santa Cruz", "Yuma")
counties_california <- c("Imperial", "Riverside", "San Diego")
counties_texas <- c("Brewster", "Brooks", "Cameron", "Crocket", "Culberson", "Dimmit", "Duval", "Edwards", "El Paso", "Hidalgo", "Hudspeth", "Jeff Davis", "Jim Hogg", "Kenedy", "Kinney", "La Salle", "Maverick", "Pecos", "Presidio", "Starr", "Terrell", "Uvalde", "Val Verde", "Webb", "Willacy", "Zapata", "Zevala")
counties_newmexico<- c("Dona Ana", "Grant", "Hidalgo", "Luna", "Otero")


# Filter the dataset based on the selected states and counties
filtered_data <- HealthOutcomes %>%
  filter(StateDesc %in% states &
           ((StateDesc == "Arizona" & LocationName %in% counties_arizona) |
              (StateDesc == "California" & LocationName %in% counties_california) |
              (StateDesc == "New Mexico" & LocationName %in% counties_newmexico) |
              (StateDesc == "Texas" & LocationName %in% counties_texas)))

#---------------------------------------------------------------------------------------#
ggplot(filtered_data, aes(x = LocationName, y = Data_Value, fill = Data_Value_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Location", y = "Prevalence (%)", fill = "Prevalence Type") +
  ggtitle("Prevalence of Health Measures by Location")
#############################
ggplot(filtered_data, aes(x = Measure, y = Data_Value, fill = LocationName)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Health Measure", y = "Prevalence (%)", fill = "Counties") +
  ggtitle("Prevalence of Health Measures by Location") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#
p <- ggplot(filtered_data, aes(x = Measure, y = Data_Value, fill = LocationName)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Health Measure", y = "Prevalence (%)", fill = "Counties") +
  ggtitle("Prevalence of Health Measures by Location") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(aspect.ratio = 1)
#over 400 variables to visualize i selcted teh most iportant indicators fo health
selected_measures <- c(
  "Current asthma among adults aged >=18 years",
  "Depression among adults aged >=18 years",
  "Cancer (excluding skin cancer) among adults aged >=18 years",
  "Current lack of health insurance among adults aged 18-64 years",
  "Fair or poor self-rated health status among adults aged >=18 years",
  "High blood pressure among adults aged >=18 years",
  "Stroke among adults aged >=18 years",
  "Current smoking among adults aged >=18 years",
  "Obesity among adults aged >=18 years",
  "Sleeping less than 7 hours among adults aged >=18 years",
  "Diagnosed diabetes among adults aged >=18 years",
  "Coronary heart disease among adults aged >=18 years"
)

# Filter the original dataset to only include rows with selected measures
filtered_data <- filtered_data %>%
  filter(Measure %in% selected_measures)
# Filter the original dataset to only include rows with selected measures and "Age-adjusted prevalence" data type
filtered_data <- filtered_data %>%
  filter(Measure %in% selected_measures, Data_Value_Type == "Age-adjusted prevalence")

#Visualize it
ggplot(filtered_data, aes(x = Measure, y = Data_Value, fill = LocationName)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Health Measure", y = "Prevalence", fill = "Counties") +
  ggtitle("Prevalence of Health Measures by Location") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Abbreviated titles
abbreviated_titles <- c(
  "Asthma",
  "Depression",
  "Cancer",
  "NoInsurance",
  "BadHealth",
  "BloodPressure",
  "Stroke",
  "Smoking",
  "Obesity",
  "Sleep<7",
  "Diabetes",
  "HeartDisease"
)

#Making it look better 
my_palette <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")

ggplot(filtered_data, aes(x = Measure, y = Data_Value, fill = LocationName)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Health Measure", y = "Prevalence", fill = "Counties") +
  ggtitle("Prevalence of Health Measures by Location 2020") +
  theme(axis.text.x = element_text(size = 8, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.position = "right",
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        legend.key = element_rect(fill = "white", color = "black"),
        plot.title = element_text(size = 14, hjust = 0.5),
        plot.background = element_rect(fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(color = "black", fill = "transparent")
  ) +
  scale_fill_manual(values = my_palette)+
  scale_x_discrete(labels = abbreviated_titles)

#----------------Asthma Data--------------#

library(readr)
library(readr)
library(ggplot2)
library(dplyr)
AsthmaHospitalization<- read.csv("AsthmaHospitalizationByCounty2015_2020_California.txt")
View(AsthmaHospitalization)
#filter the dataset to only select relevant counties
Asthma_selected_counties <- AsthmaHospitalization %>%
  filter(COUNTY %in% c("Imperial", "Riverside", "San Diego"))
#Rename columns
colnames(Asthma_selected_counties) <- c("COUNTY", "YEAR", "STRATA", "STRATA_NAME", "AGE_GROUP", "NUMBER_OF_HOSPITALIZATIONS", "AGE_ADJUSTED_HOSPITALIZATION_RATE", "COMMENT")


#Fixing formatting
# Replace "0�17" with "0-17"
#not working i will only view total population data
Asthma_selected_counties <- Asthma_selected_counties %>%
  filter(STRATA %in% c("Total population"))

#make hospitalization into number
Asthma_selected_counties$NUMBER_OF_HOSPITALIZATIONS <- as.numeric(gsub(",", "", Asthma_selected_counties$NUMBER_OF_HOSPITALIZATIONS))
#Visualize
 ggplot(Asthma_selected_counties, aes(x = YEAR, y = NUMBER_OF_HOSPITALIZATIONS, color = COUNTY, group = COUNTY)) +
  geom_line() +
  facet_wrap(~ COUNTY, scales = "free_y") +
  labs(title = "Asthma Hospitalizations by Ethnicity and County",
       x = "Date", y = "Hospitalizations(total Popultaion") +
  theme_minimal()

 ggplot(Asthma_selected_counties, aes(x = YEAR, y = NUMBER_OF_HOSPITALIZATIONS, color = COUNTY, group = COUNTY)) +
   geom_line() +
   labs(title = "Asthma Hospitalizations by County",
        x = "Date", y = "Hospitalizations (total Population)") +
   theme_minimal()










#_______Visualize Mortality rate by county____________#
library(sf)
library(ggplot2)
#__________________Texas______________________________#
# Load spatial data
county_map <- st_read("Texas_County_Boundaries.geojson")
county_map$County <- paste0(county_map$CNTY_NM, " County")
# Merge data
merged_data <- merge(county_map, MortalityRateCombined, by = "County")

# Convert "Crude.Rate" column to numeric
merged_data$Crude.Rate <- as.numeric(as.character(merged_data$Crude.Rate))

# Check for missing or non-numeric values
missing_values <- is.na(merged_data$Crude.Rate) | !is.numeric(merged_data$Crude.Rate)

# Replace missing or non-numeric values with NA
merged_data$Crude.Rate[missing_values] <- NA

# Plot the data
ggplot() +
  geom_sf(data = merged_data, aes(fill = Crude.Rate), color = "black", size = 0.1) +
  scale_fill_gradient(name = "Mortality Rate", low = "blue", high = "red") +
  labs(title = "Mortality Rate by County 2022")
#__________________California______________________________#

unzip("ca_counties.zip")

# Load the shapefile
CA_Counties <- st_read("CA_Counties.shp")
CA_Counties$County <- paste0(CA_Counties$NAMELSAD)
# Merge data
merged_data_California <- merge(CA_Counties, MortalityRateCombined, by = "County", all.x = TRUE)

# Convert "Crude.Rate" column to numeric
merged_data_California$Crude.Rate <- as.numeric(as.character(merged_data_California$Crude.Rate))

# Check for missing or non-numeric values
missing_values <- is.na(merged_data_California$Crude.Rate) | !is.numeric(merged_data_California$Crude.Rate)

# Replace missing or non-numeric values with NA
merged_data_California$Crude.Rate[missing_values] <- NA

# Plot the data
ggplot() +
  geom_sf(data = merged_data_California, aes(fill = Crude.Rate), color = "black", size = 0.1) +
  scale_fill_gradient(name = "Mortality Rate", low = "blue", high = "red") +
  labs(title = "Mortality Rate by County California 2022")

#__________________New Mexico______________________________#

unzip("nm_counties.zip")
NM_Counties <- st_read("tl_2018_nm_county.geojson")
NM_Counties$County <- paste0(NM_Counties$NAMELSAD)
# Replace "Dona Ana County" with "Doña Ana County" in NM_Counties
MortalityRateCombined$County <- gsub("Dona Ana County", "Doña Ana County", MortalityRateCombined$County)

# Merge the modified NM_Counties with MortalityRateCombined
merged_data_NewMexico <- merge(NM_Counties, MortalityRateCombined, by = "County", all.x = TRUE)
# Convert "Crude.Rate" column to numeric
merged_data_NewMexico$Crude.Rate <- as.numeric(as.character(merged_data_NewMexico$Crude.Rate))

# Check for missing or non-numeric values
missing_values <- is.na(merged_data_NewMexico$Crude.Rate) | !is.numeric(merged_data_NewMexico$Crude.Rate)

# Replace missing or non-numeric values with NA
merged_data_NewMexico$Crude.Rate[missing_values] <- NA

# Plot the data
ggplot() +
  geom_sf(data = merged_data_NewMexico, aes(fill = Crude.Rate), color = "black", size = 0.1) +
  scale_fill_gradient(name = "Mortality Rate", low = "blue", high = "red") +
  labs(title = "Mortality Rate by County New Mexico 2022")

#__________________Arizona______________________________#

AZ_Counties <- st_read("AZ_counties.geojson")
library(stringr)
AZ_Counties$County <- paste0(str_to_title(AZ_Counties$NAME), " County")

#Removing the AZ in the county row 
MortalityRateCombined$County <- sub(" AZ$", "", MortalityRateCombined$County)

# Merge the modified AZ_Counties with MortalityRateCombined
merged_data_Arizona <- merge(AZ_Counties, MortalityRateCombined, by = "County", all.x = TRUE)
# Convert "Crude.Rate" column to numeric
merged_data_Arizona$Crude.Rate <- as.numeric(as.character(merged_data_Arizona$Crude.Rate))

# Check for missing or non-numeric values
missing_values <- is.na(merged_data_Arizona$Crude.Rate) | !is.numeric(merged_data_Arizona$Crude.Rate)

# Replace missing or non-numeric values with NA
merged_data_Arizona$Crude.Rate[missing_values] <- NA

# Plot the data
ggplot() +
  geom_sf(data = merged_data_Arizona, aes(fill = Crude.Rate), color = "black", size = 0.1) +
  scale_fill_gradient(name = "Mortality Rate", low = "blue", high = "red") +
  labs(title = "Mortality Rate by County Arizona 2022")




####################################
#too much of the water quality is missing lets run a regression just for air quality and mortality

library(dplyr)
library(ggplot2)
library(lmtest)

# Remove rows with missing values
AirQuality[AirQuality == "ND"] <- NA
AirQuality <- AirQuality[complete.cases(AirQuality), ]

# Check again for missing values
sum(is.na(AirQuality))

Mortality_Air <- merge(AirQuality, MortalityRateCombined, by = "County")

# Explore the Mortality_Air data set
summary(Mortality_Air)
str(Mortality_Air)
#####There is only complete data for Riverside and San Diego

# Visualize the relationship between air quality and mortality rate
ggplot(Mortality_Air, aes(x = PM10........24.hr..µg.m3., y = Crude.Rate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Relationship between Air Quality and Mortality Rate",
       x = "Air Quality Variable",
       y = "Mortality Rate Variable")

# Run regression analysis
model <- lm(Crude.Rate ~ PM10........24.hr..µg.m3., data = Mortality_Air)
summary(model)
#Not enought data to show anything significant

