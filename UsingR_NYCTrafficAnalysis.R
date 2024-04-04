# Following best practices, install all packages at the beginning
install.packages("tidyverse")
install.packages("dplyr")
install.packages("lubridate")
install.packages("factoextra")
install.packages("ggmap")
install.packages("geosphere")
install.packages("osmdata")
install.packages("googleVis")
install.packages("MASS")

# Then, load the libraries
library(lubridate)
library(dplyr)
library(cluster)
library(hms)
library(factoextra)
library(tidyverse)
library(ggmap)
library(ggplot)
library(ggplot2)
library(patchwork)
library(geosphere)
library(osmdata)
library(googleVis)
library(scales)
library(MASS) #for running stepwise on clusters

# Google API key mapping
register_google("AIzaSyBKvO5vCjsT0qC7iUtzMJdgdzpz4jfkY08")
# code shared with others
# register_google("API_key")

#  Import “NYC 2020 Accidents” file
accidents <- read.csv("path/toTheFile")

# Attach headers
attach(accidents)

# Take an initial look at the data
head(accidents)

# Check headings for renaming columns
names(accidents)

# Rename each column
colnames(accidents)[colnames(accidents) == "CRASH.DATE"] <- "Date" 
colnames(accidents)[colnames(accidents) == "CRASH.TIME"] <- "Time"
colnames(accidents)[colnames(accidents) == "BOROUGH"] <- "Borough"
colnames(accidents)[colnames(accidents) == "ZIP.CODE"] <- "ZIP"
colnames(accidents)[colnames(accidents) == "LATITUDE"] <- "Lat"
colnames(accidents)[colnames(accidents) == "LONGITUDE"] <- "Long"
colnames(accidents)[colnames(accidents) == "LOCATION"] <- "Location"
colnames(accidents)[colnames(accidents) == "LOCATION"] <- "Location"
colnames(accidents)[colnames(accidents) == "ON.STREET.NAME"] <- "OnStreet"
colnames(accidents)[colnames(accidents) == "CROSS.STREET.NAME"] <- "CrossStreet"
colnames(accidents)[colnames(accidents) == "OFF.STREET.NAME"] <- "OffStreet"
colnames(accidents)[colnames(accidents) == "NUMBER.OF.PERSONS.INJURED"] <- "Injuries"
colnames(accidents)[colnames(accidents) == "NUMBER.OF.PERSONS.KILLED"] <- "Deaths"
colnames(accidents)[colnames(accidents) == "NUMBER.OF.PEDESTRIANS.INJURED"] <- "PInjuries"
colnames(accidents)[colnames(accidents) == "NUMBER.OF.PEDESTRIANS.KILLED"] <- "PDeaths"
colnames(accidents)[colnames(accidents) == "NUMBER.OF.CYCLIST.INJURED"] <- "CInjuries"
colnames(accidents)[colnames(accidents) == "NUMBER.OF.CYCLIST.KILLED"] <- "CDeaths"
colnames(accidents)[colnames(accidents) == "NUMBER.OF.MOTORIST.INJURED"] <- "MInjuries"
colnames(accidents)[colnames(accidents) == "NUMBER.OF.MOTORIST.KILLED"] <- "MDeaths"
colnames(accidents)[colnames(accidents) == "CONTRIBUTING.FACTOR.VEHICLE.1"] <- "CF1"
colnames(accidents)[colnames(accidents) == "CONTRIBUTING.FACTOR.VEHICLE.2"] <- "CF2"
colnames(accidents)[colnames(accidents) == "CONTRIBUTING.FACTOR.VEHICLE.3"] <- "CF3"
colnames(accidents)[colnames(accidents) == "CONTRIBUTING.FACTOR.VEHICLE.4"] <- "CF4"
colnames(accidents)[colnames(accidents) == "CONTRIBUTING.FACTOR.VEHICLE.5"] <- "CF5"
colnames(accidents)[colnames(accidents) == "COLLISION_ID"] <- "ID"
colnames(accidents)[colnames(accidents) == "VEHICLE.TYPE.CODE.1"] <- "Vehicle1"
colnames(accidents)[colnames(accidents) == "VEHICLE.TYPE.CODE.2"] <- "Vehicle2"
colnames(accidents)[colnames(accidents) == "VEHICLE.TYPE.CODE.3"] <- "Vehicle3"
colnames(accidents)[colnames(accidents) == "VEHICLE.TYPE.CODE.4"] <- "Vehicle4"
colnames(accidents)[colnames(accidents) == "VEHICLE.TYPE.CODE.5"] <- "Vehicle5"

# Count the number of duplicate rows
duplicate_count <- sum(duplicated(accidents))
print(duplicate_count)
# 
# # Count the number of missing values in each column
missing_count <- colSums(is.na(accidents))
print(missing_count)

# Convert the date to date format
accidents$Date <- as.Date(accidents$Date)



### Clean Contributing Factor 1 column

# Capitalize all letters
accidents$CF1 <- toupper(accidents$CF1)
sort(unique(accidents$CF1))

# Fill in the blanks as "unspecified"
accidents$CF1 <- ifelse(accidents$CF1 == "", "UNSPECIFIED", accidents$CF1)

### Clean Contributing Factor 2 column

accidents$CF2 <- toupper(accidents$CF2)
sort(unique(accidents$CF2))
accidents$CF2 <- ifelse(accidents$CF2 == "", "UNSPECIFIED", accidents$CF2)

#### Clean Vehicle 1 column

accidents$Vehicle1 <- toupper(accidents$Vehicle1)


sort(unique(accidents$Vehicle1))

accidents$Vehicle1 <- ifelse(accidents$Vehicle1 %in% c("NYC AMBULA", "WHITE AMBU","NYS AMBULA","GEN  AMBUL","FDNY AMBUL","FDNY EMT", "AMBU", "AMB", "abulance", "ambulance", "AMBULENCE" ,"AMBULANCE"), "AMBULANCE", accidents$Vehicle1)
accidents$Vehicle1 <- ifelse(accidents$Vehicle1 %in% c("BACK HOE","BACKH","BACKHOE" ), "BACKHOE", accidents$Vehicle1)
accidents$Vehicle1 <- ifelse(accidents$Vehicle1 %in% c("com","COM","COM TRANS","commercial","COMMERCIAL" ), "COMMERCIAL", accidents$Vehicle1)
accidents$Vehicle1 <- ifelse(accidents$Vehicle1 %in% c( "Delivery","DELIVERY","DELIVERY T" ,"delviery","dilevery t"), "DELIVERY TRUCK", accidents$Vehicle1)
accidents$Vehicle1 <- ifelse(accidents$Vehicle1 %in% c( "D/V WB", "delivery v","DELIVERY V","Delv" ,"DELV"), "DELIVERY VAN", accidents$Vehicle1)
accidents$Vehicle1 <- ifelse(accidents$Vehicle1 %in% c("Dump","DUMP","DUMP TRUCK", "dump truck" ), "DUMP TRUCK", accidents$Vehicle1)
accidents$Vehicle1 <- ifelse(accidents$Vehicle1 %in% c("E-Bik","E-bike","E-Bike","E-BIKE", "E bike" ), "E-BIKE", accidents$Vehicle1)
accidents$Vehicle1 <- ifelse(accidents$Vehicle1 %in% c("FIRE TRUCK" ,"NYC FIRETR","NYC FIRE T","FDNY","FD TRUCK","FDNY FIRE","FDNY TRUCK","FDNY FIRET","FIRE","FDNY TRUCK","FIRETRUCK" ),"FDNY FIRETRUCK" , accidents$Vehicle1)
accidents$Vehicle1 <- ifelse(accidents$Vehicle1 %in% c( "school bus","School bus","School Bus","SCHOOL BUS"), "SCHOOL BUS", accidents$Vehicle1)
accidents$Vehicle1 <- ifelse(accidents$Vehicle1 %in% c("18 WHEELER","TR","TR-TRAILER", "TRAC","TRACTOR","TRACTOR TR","TRACTOR TRUCK DIESEL" ,"TRACTOR TRUCK GASOLINE" ,"TRAIL" ,"TRC","TRAILER" ,"TRK","TRL"  ), "TRACTOR TRAILER", accidents$Vehicle1)
accidents$Vehicle1 <- ifelse(accidents$Vehicle1 %in% c( "UNKN" , "UNK"),"UNKNOWN" , accidents$Vehicle1)
accidents$Vehicle1 <- ifelse(accidents$Vehicle1 %in% c("US POSTAL" ,"USPS" ,"USPS #7530" ,"USPS POSTA" , "USPS","USPS POSTA", "USPS #7530", "USPS VAN"  ),"US POSTAL" , accidents$Vehicle1)
accidents$Vehicle1 <- ifelse(accidents$Vehicle1 %in% c("TRUCK","PICK-UP TR","PICK UP","PICK","PICK-UP TRUCK","PICK UP TR","PICKUP TRU"  ),"PICK-UP TRUCK" , accidents$Vehicle1)
accidents$Vehicle1 <- ifelse(accidents$Vehicle1 %in% c("UT","UTIL","UTILITY TR","UTILITY VE" ,"UTILITY." ),"UTILITY"  , accidents$Vehicle1)
accidents$Vehicle1 <- ifelse(accidents$Vehicle1 %in% c("FORK LIFT"),"FORKLIFT"   , accidents$Vehicle1)
accidents$Vehicle1 <- ifelse(accidents$Vehicle1 %in% c("","0"),"UNSPECIFIED"   , accidents$Vehicle1)

#### Clean Vehicle 2 column

accidents$Vehicle2 <- toupper(accidents$Vehicle2)


sort(unique(accidents$Vehicle2))

accidents$Vehicle2 <- ifelse(accidents$Vehicle2 %in% c("NYC AMBULA", "WHITE AMBU","NYS AMBULA","GEN  AMBUL","FDNY AMBUL","FDNY EMT", "AMBU", "AMB", "abulance", "ambulance", "AMBULENCE" ,"AMBULANCE"), "AMBULANCE", accidents$Vehicle2)
accidents$Vehicle2 <- ifelse(accidents$Vehicle2 %in% c("BACK HOE","BACKH","BACKHOE" ), "BACKHOE", accidents$Vehicle2)
accidents$Vehicle2 <- ifelse(accidents$Vehicle2 %in% c("com","COM","COM TRANS","commercial","COMMERCIAL" ), "COMMERCIAL", accidents$Vehicle2)
accidents$Vehicle2 <- ifelse(accidents$Vehicle2 %in% c( "Delivery","DELIVERY","DELIVERY T" ,"delviery","dilevery t"), "DELIVERY TRUCK", accidents$Vehicle2)
accidents$Vehicle2 <- ifelse(accidents$Vehicle2 %in% c( "D/V WB", "delivery v","DELIVERY V","Delv" ,"DELV"), "DELIVERY VAN", accidents$Vehicle2)
accidents$Vehicle2 <- ifelse(accidents$Vehicle2 %in% c("Dump","DUMP","DUMP TRUCK", "dump truck" ), "DUMP TRUCK", accidents$Vehicle2)
accidents$Vehicle2 <- ifelse(accidents$Vehicle2 %in% c("E-Bik","E-bike","E-Bike","E-BIKE", "E bike" ), "E-BIKE", accidents$Vehicle2)
accidents$Vehicle2 <- ifelse(accidents$Vehicle2 %in% c( "school bus","School bus","School Bus","SCHOOL BUS"), "SCHOOL BUS", accidents$Vehicle2)
accidents$Vehicle2 <- ifelse(accidents$Vehicle2 %in% c("18 WEELER","18 WHEELER","TR","TR-TRAILER", "TRAC","TRACTOR","TRACTOR TR","TRACTOR TRUCK DIESEL" ,"TRACTOR TRUCK GASOLINE" ,"TRAIL" ,"TRC","TRAILER" ,"TRK","TRL"  ), "TRACTOR TRAILER", accidents$Vehicle2)
accidents$Vehicle2 <- ifelse(accidents$Vehicle2 %in% c( "UKN","UNKN" , "UNK"),"UNKNOWN" , accidents$Vehicle2)
accidents$Vehicle2 <- ifelse(accidents$Vehicle2 %in% c("US POSTAL" ,"USPS" ,"USPS #7530" ,"USPS POSTA" , "USPS","USPS POSTA", "USPS #7530", "USPS VAN"  ),"US POSTAL" , accidents$Vehicle2)
accidents$Vehicle2 <- ifelse(accidents$Vehicle2 %in% c("PICK TRUCK" ,"PICK-UP TRUCK","TRUCK","PICK-UP TR","PICK UP","PICK","PICK-UP TRUCK","PICK UP TR","PICKUP TRU"  ),"PICK-UP TRUCK" , accidents$Vehicle2)
accidents$Vehicle2 <- ifelse(accidents$Vehicle2 %in% c("UT","UTIL","UTILITY TR","UTILITY VE" ,"UTILITY." ),"UTILITY"  , accidents$Vehicle2)
accidents$Vehicle2 <- ifelse(accidents$Vehicle2 %in% c("FORK LIFT"),"FORKLIFT"   , accidents$Vehicle2)
accidents$Vehicle2 <- ifelse(accidents$Vehicle2 %in% c("","0"),"UNSPECIFIED"   , accidents$Vehicle2)
accidents$Vehicle2 <- ifelse(accidents$Vehicle2 %in% c("FIRE TRUCK" ,"NYC FIRETR","NYC FIRE T" ,"FDNY","FD TRUCK","FDNY FIRE","FDNY TRUCK","FDNY FIRET","FIRE","FDNY TRUCK","FIRETRUCK" ),"FDNY FIRETRUCK" , accidents$Vehicle2)

### ADDING COLUMNS

# Create separate columns
accidents$Morning <- as.numeric(accidents$Time >= "06:00:00" & accidents$Time <= "09:59:59")
accidents$Midday <- as.numeric(accidents$Time >= "10:00:00" & accidents$Time <= "14:59:59")
accidents$Afternoon <- as.numeric(accidents$Time >= "15:00:00" & accidents$Time <= "18:59:59")
accidents$Evening <- as.numeric(accidents$Time >= "19:00:00" & accidents$Time <= "23:59:59")
accidents$Overnight <- as.numeric(accidents$Time >= "00:00:00" & accidents$Time <= "05:59:59")

# Check:
sum(accidents$Morning)
sum(accidents$Midday)
sum(accidents$Afternoon)
sum(accidents$Evening)
sum(accidents$Overnight)

# Create an additional column where Morning = 1, Midday = 2, Afternoon = 3
# Evening = 4, Overnight = 5

# Initialize new column with default value of 0
accidents$TimeCategory <- 0

accidents$TimeCategory[accidents$Time >= "06:00:00" & accidents$Time <= "09:59:59"] <- 1  # Value 1 to Morning
accidents$TimeCategory[accidents$Time >= "10:00:00" & accidents$Time <= "14:59:59"] <- 2  # Value 2 to Midday
accidents$TimeCategory[accidents$Time >= "15:00:00" & accidents$Time <= "18:59:59"] <- 3  # Value 3 to Afternoon
accidents$TimeCategory[accidents$Time >= "19:00:00" & accidents$Time <= "23:59:59"] <- 4  # Value 4 to Evening
accidents$TimeCategory[accidents$Time >= "00:00:00" & accidents$Time <= "05:59:59"] <- 5  # Value 5 to Overnight

# # Convert the time to time format
accidents$Time <- as_hms(accidents$Time)

# # Extract hour of the day from the time
accidents$Hour <- hour(accidents$Time)

# Create a new column called "Weekday" using weekdays() function
accidents$Weekday <- weekdays(accidents$Date)

# Add “Month” column to data
accidents$Month <- month(accidents$Date)

# Add “InjuriesDeaths” column to data
accidents$InjuriesDeaths <- accidents$Injuries + accidents$Deaths

# Add “PInjuriesDeaths” column to data
accidents$PInjuriesDeaths <- accidents$PInjuries + accidents$PDeaths

# Add “CInjuriesDeaths” column to data
accidents$CInjuriesDeaths <- accidents$CInjuries + accidents$CDeaths

# Add “MInjuriesDeaths” column to data
accidents$MInjuriesDeaths <- accidents$MInjuries + accidents$MDeaths

# Check column headings
names(accidents)

### Add columns for day parts

# Check type
class(accidents$Time)

# Any N/A?
sum(is.na(accidents$Time)) # None


# Check column headings
names(accidents)

### Adding columns for weekdays
# Convert Weekday to numbers so it can be used for Cluster Analysis
accidents$WeekdayNum <- as.numeric(factor(accidents$Weekday, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")))
# Scale note: Sunday = 1… Saturday = 7 

# Create separate columns for each day
accidents$Sunday <- as.numeric(accidents$WeekdayNum == 1)
accidents$Monday <- as.numeric(accidents$WeekdayNum == 2)
accidents$Tuesday <- as.numeric(accidents$WeekdayNum == 3)
accidents$Wednesday <- as.numeric(accidents$WeekdayNum == 4)
accidents$Thursday <- as.numeric(accidents$WeekdayNum == 5)
accidents$Friday <- as.numeric(accidents$WeekdayNum == 6)
accidents$Saturday <- as.numeric(accidents$WeekdayNum == 7)

### Add column for month names

# Duplicate the Month column
accidents <- accidents %>%
  mutate(MonthName = as.character(Month))

# Recode the MonthName column
accidents <- accidents %>%
  mutate(MonthName = recode(MonthName, "1" = "January", "2" = "February", "3" = "March", "4"
                            = "April",   "5" = "May", "6" = "June", "7" = "July", "8" = "August"))

# Order MonthName
accidents$MonthName <- factor(accidents$MonthName, levels = month.name)

### Adding COVID/Pre-COVID columns

# PRECOVID WILL = 0, COVID = 1
# ON MARCH 20, 2020, NEW YORK STATE GOVERNOR'S OFFICE ISSUED AN EXECUTIVE ORDER
# CLOSING "NON-ESSENTIAL" BUSINESSES.  WE WILL USE BEFORE 3/20/2020 & AS OF
# 3/20/2020 - CURRENT AS OUR TIME FRAME.

# Created a new column called COVID and initialize it to 0
accidents$COVID <- 0

# Set COVID value to 1 for dates on or after 3/20/2020
accidents$COVID[accidents$Date >= as.Date("2020-03-20")] <- 1

# Check if worked properly
sum(accidents$COVID == 0) == sum(accidents$Date < as.Date("2020-03-20"))
sum(accidents$COVID == 1) == sum(accidents$Date > as.Date("2020-03-19"))
# Both of the above are TRUE!  It worked properly.

### Make Seperate Cols:
# Create PreCovid and AfterCovid columns with default value of 0
accidents$PreCovid <- 0
accidents$AfterCovid <- 0

# Assign 1 to PreCovid column if COVID is 0, and to AfterCovid column if COVID is 1
accidents$PreCovid[accidents$COVID == 0] <- 1
accidents$AfterCovid[accidents$COVID == 1] <- 1

######  END OF  ADDING COVID / PRE COVID COLUMNS: #############

### Add accident count column
accidents$AccidentCount <- 1

# Check column names
names(accidents)

# Check variables
glimpse(accidents)

# Review summary
summary(accidents)


### SUMMARY GRAPHS

# Calculate the total accidents by date
total_accidents_by_date <- accidents %>%
  group_by(Date) %>%
  summarize(total_accidents = sum(AccidentCount))

# Create a time series line graph with every month on the x-axis
ggplot(total_accidents_by_date, aes(x = Date, y = total_accidents)) +
  geom_line() +
  labs(x = "Date", y = "Accident Count") +
  ggtitle("Accidents Over Time") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")

##############JANELLE TO DO BEGINNING ###################################
### Top 10 contributing factors (overall)

# Filter the top 10 values in descending order
top_10_cf1 <- accidents %>%
   group_by(CF1) %>%
   summarize(total_accidents = sum(AccidentCount)) %>%
   top_n(10, total_accidents) %>%
   arrange(desc(total_accidents))

# Calculate the total accidents
total_accidents <- sum(top_10_cf1$total_accidents)

# Calculate the percentages
top_10_cf1$percentage <- (top_10_cf1$total_accidents / total_accidents) * 100

# Create a horizontal bar chart with the top 10 values and percentage labels
ggplot(top_10_cf1, aes(x = reorder(CF1, total_accidents), y = total_accidents)) +
   geom_bar(stat = "identity", fill = "steelblue") +
   geom_text(aes(label = paste0(round(percentage, 1), "%")), hjust = -0.2, color = "black", size = 4) +
   labs(x = "Primary Contributing Factor", y = "Accident Count") +
   ggtitle("Top 10 Primary Contributing Factors by Accident Count") +
   theme_minimal() +
   coord_flip()

##############JANELLE TO DO END ###################################

### Top 10 contributing factors (when Pedestrians are injured or killed)

# Filter the top 10 values when PInjuries or PDeaths > 0
top_10_cf1_p <- accidents %>%
  filter(PInjuries > 0 | PDeaths > 0) %>%
  group_by(CF1) %>%
  summarize(total_accidents = sum(AccidentCount)) %>%
  top_n(10, total_accidents) %>%
  arrange(desc(total_accidents))

# Calculate the percentage for each bar
top_10_cf1_p <- top_10_cf1_p %>%
  mutate(percentage = total_accidents / sum(total_accidents) * 100)

# Create a horizontal bar chart with the top 10 values and data labels
ggplot(top_10_cf1_p, aes(x = reorder(CF1, total_accidents), y = total_accidents)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), hjust= -0.05, vjust = -0.5, color = "black", size = 4) +
  labs(x = "Primary Contributing Factor", y = "Accident Count") +
  ggtitle("Top 10 Primary Contributing Factors by Accident Count (When PInjuries or PDeaths > 0)") +
  theme_minimal() +
  coord_flip()


### Top 10 contributing factors (when Cyclists are injured or killed)

# Filter the top 10 values when CInjuries or CDeaths > 0
top_10_cf1_c <- accidents %>%
  filter(CInjuries > 0 | CDeaths > 0) %>%
  group_by(CF1) %>%
  summarize(total_accidents = sum(AccidentCount)) %>%
  top_n(10, total_accidents) %>%
  arrange(desc(total_accidents))

# Calculate the percentage for each bar
top_10_cf1_c <- top_10_cf1_c %>%
  mutate(percentage = total_accidents / sum(total_accidents) * 100)

# Create a horizontal bar chart with the top 10 values and data labels
ggplot(top_10_cf1_c, aes(x = reorder(CF1, total_accidents), y = total_accidents)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), hjust= -0.05, vjust = -0.5, color = "black", size = 4) +
  labs(x = "Primary Contributing Factor", y = "Accident Count") +
  ggtitle("Top 10 Primary Contributing Factors by Accident Count (When CInjuries or CDeaths > 0)") +
  theme_minimal() +
  coord_flip()


### Count of accidents by month (January through August)

# Create a bar chart to show the count of accidents by month
ggplot(accidents, aes(x = MonthName, y = AccidentCount, fill = MonthName)) +
  geom_bar(stat = "identity") +
  labs(x = "Month", y = "Accidents", fill = "Month") +
  ggtitle("Count of Accidents by Month") +
  theme_minimal()


### Count of deaths by month and category (January through August)

# Create a new data frame with the required columns
deaths <- data.frame(
  MonthName = accidents$MonthName,
  MDeaths = accidents$MDeaths,
  PDeaths = accidents$PDeaths,
  CDeaths = accidents$CDeaths
)

# Reshape the data to long format
deaths_long <- deaths %>%
  pivot_longer(cols = c(MDeaths, PDeaths, CDeaths), names_to = "Category", values_to = "Deaths")

# Create a stacked bar chart to show the count of deaths by month and category
ggplot(deaths_long, aes(x = MonthName, y = Deaths, fill = Category)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Month", y = "Deaths", fill = "Category") +
  ggtitle("Count of Deaths by Month and Category") +
  theme_minimal()


### Count of cyclist injuries by month (January through August)

# Aggregate the data by month and calculate the sum of CInjuries and CDeaths
agg_c_injuries_deaths_month <- accidents %>%
  group_by(MonthName) %>%
  summarize(CInjuries = sum(CInjuries),
            CDeaths = sum(CDeaths))

# Create a bar chart to show the count of accidents by month and overlay CDeaths
ggplot(agg_c_injuries_deaths_month, aes(x = MonthName)) +
  geom_col(aes(y = CInjuries, fill = "CInjuries"), position = "identity") +
  geom_col(aes(y = CDeaths, fill = "CDeaths"), position = "identity", alpha = 0.5) +
  labs(x = "Month", y = "Count", fill = "") +
  ggtitle("Count of Cyclist Injuries and Deaths by Month") +
  scale_fill_manual(values = c(CInjuries = "steelblue", CDeaths = "red"), guide = "legend") +
  theme_minimal()


### Count of cyclist injuries by weekday (January through August)

# Aggregate the data by month and calculate the sum of CInjuries and CDeaths
agg_c_injuries_deaths_weekday <- accidents %>%
  group_by(Weekday) %>%
  summarize(CInjuries = sum(CInjuries),
            CDeaths = sum(CDeaths))

# Create a bar chart to show the count of accidents by month and overlay CDeaths
ggplot(agg_c_injuries_deaths_weekday, aes(x = Weekday)) +
  geom_col(aes(y = CInjuries, fill = "CInjuries"), position = "identity") +
  geom_col(aes(y = CDeaths, fill = "CDeaths"), position = "identity", alpha = 0.5) +
  labs(x = "Weekday", y = "Count", fill = "") +
  ggtitle("Count of Cyclist Injuries and Deaths by Weekday") +
  scale_fill_manual(values = c(CInjuries = "steelblue", CDeaths = "red"), guide = "legend") +
  theme_minimal()


### Count of pedestrian injuries and deaths by month

# Aggregate the data by month and calculate the sum of CInjuries and CDeaths
agg_p_injuries_deaths_month <- accidents %>%
  group_by(MonthName) %>%
  summarize(PInjuries = sum(PInjuries),
            PDeaths = sum(PDeaths))

# Create a bar chart to show the count of accidents by month and overlay CDeaths
ggplot(agg_p_injuries_deaths_month, aes(x = MonthName)) +
  geom_col(aes(y = PInjuries, fill = "PInjuries"), position = "identity") +
  geom_col(aes(y = PDeaths, fill = "PDeaths"), position = "identity", alpha = 0.5) +
  labs(x = "Month", y = "Count", fill = "") +
  ggtitle("Count of Pedestrian Injuries and Deaths by Month") +
  scale_fill_manual(values = c(PInjuries = "steelblue", PDeaths = "red"), guide = "legend") +
  theme_minimal()

### Count of Pedestrian injuries and deaths by weekday (January through August)

# Aggregate the data by month and calculate the sum of CInjuries and CDeaths
agg_p_injuries_deaths_weekday <- accidents %>%
  group_by(Weekday) %>%
  summarize(PInjuries = sum(PInjuries),
            PDeaths = sum(PDeaths))

# Define the order of weekdays
weekday_order <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
# Convert Weekday to an ordered factor with the desired order
agg_p_injuries_deaths_weekday$Weekday <- factor(agg_p_injuries_deaths_weekday$Weekday, levels = weekday_order, ordered = TRUE)


# Create a bar chart to show the count of accidents by month and overlay CDeaths
ggplot(agg_p_injuries_deaths_weekday, aes(x = Weekday)) +
  geom_col(aes(y = PInjuries, fill = "PInjuries"), position = "identity") +
  geom_col(aes(y = PDeaths, fill = "PDeaths"), position = "identity", alpha = 0.5) +
  labs(x = "Weekday", y = "Count", fill = "") +
  ggtitle("Count of Pedestrian Injuries and Deaths by Weekday") +
  scale_fill_manual(values = c(PInjuries = "steelblue", PDeaths = "red"), guide = "legend") +
  theme_minimal()


### Count of motorist injuries and deaths by month

# Aggregate the data by month and calculate the sum of CInjuries and CDeaths
agg_m_injuries_deaths_month <- accidents %>%
  group_by(MonthName) %>%
  summarize(MInjuries = sum(MInjuries),
            MDeaths = sum(MDeaths))

# Create a bar chart to show the count of accidents by month and overlay CDeaths
ggplot(agg_m_injuries_deaths_month, aes(x = MonthName)) +
  geom_col(aes(y = MInjuries, fill = "MInjuries"), position = "identity") +
  geom_col(aes(y = MDeaths, fill = "MDeaths"), position = "identity", alpha = 0.5) +
  labs(x = "Month", y = "Count", fill = "") +
  ggtitle("Count of Motorist Injuries and Deaths by Month") +
  scale_fill_manual(values = c(MInjuries = "steelblue", MDeaths = "red"), guide = "legend") +
  theme_minimal()


### Count of motorist injuries and deaths by weekday (January through August)

# Aggregate the data by month and calculate the sum of CInjuries and CDeaths
agg_m_injuries_deaths_weekday <- accidents %>%
  group_by(Weekday) %>%
  summarize(MInjuries = sum(MInjuries),
            MDeaths = sum(MDeaths))

# Convert Weekday to an ordered factor with the desired order
agg_m_injuries_deaths_weekday$Weekday <- factor(agg_m_injuries_deaths_weekday$Weekday, levels = weekday_order, ordered = TRUE)

# Create a bar chart to show the count of accidents by month and overlay CDeaths
ggplot(agg_m_injuries_deaths_weekday, aes(x = Weekday)) +
  geom_col(aes(y = MInjuries, fill = "MInjuries"), position = "identity") +
  geom_col(aes(y = MDeaths, fill = "MDeaths"), position = "identity", alpha = 0.5) +
  labs(x = "Weekday", y = "Count", fill = "") +
  ggtitle("Count of Motorist Injuries and Deaths by Weekday") +
  scale_fill_manual(values = c(MInjuries = "steelblue", MDeaths = "red"), guide = "legend") +
  theme_minimal()

### Average accidents per day (January through August)

# Reorder the levels of the Weekday factor
accidents$Weekday <- factor(accidents$Weekday, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Calculate the average number of accidents per day of the week
avg_accidents <- accidents %>%
  group_by(Weekday) %>%
  summarize(total_accidents = sum(AccidentCount),
            unique_dates = n_distinct(Date),
            average_accidents_per_day = total_accidents / unique_dates,
            .groups = "drop")

# Plotting the average number of accidents per day of the week
ggplot(avg_accidents, aes(x = Weekday, y = average_accidents_per_day, fill = Weekday)) +
  geom_bar(stat = "identity") +
  labs(x = "Day of Week", y = "Average Accidents per Day", fill = "Day of Week") +
  ggtitle("Average Accidents per Day by Day of Week") +
  theme_minimal()

### Average accidents per day - February

# Calculate the average number of accidents per day of the week
avg_accidents <- accidents %>%
  filter(Month == "2") %>%
  group_by(Weekday) %>%
  summarize(total_accidents = sum(AccidentCount),
            unique_dates = n_distinct(Date),
            average_accidents_per_day = total_accidents / unique_dates,
            .groups = "drop")

# Plotting the average number of accidents per day of the week
ggplot(avg_accidents, aes(x = Weekday, y = average_accidents_per_day, fill = Weekday)) +
  geom_bar(stat = "identity") +
  labs(x = "Day of Week", y = "Average Accidents per Day", fill = "Day of Week") +
  ggtitle("Average Accidents per Day in February") +
  theme_minimal()

### Average accidents per day - April

# Calculate the average number of accidents per day of the week
avg_accidents <- accidents %>%
  filter(Month == "4") %>%
  group_by(Weekday) %>%
  summarize(total_accidents = sum(AccidentCount),
            unique_dates = n_distinct(Date),
            average_accidents_per_day = total_accidents / unique_dates,
            .groups = "drop")

# Plotting the average number of accidents per day of the week
ggplot(avg_accidents, aes(x = Weekday, y = average_accidents_per_day, fill = Weekday)) +
  geom_bar(stat = "identity") +
  labs(x = "Day of Week", y = "Average Accidents per Day", fill = "Day of Week") +
  ggtitle("Average Accidents per Day by Day of Week") +
  theme_minimal()


### Average number of accidents per hour - Weekday vs. Weekend (bar chart)

# Calculate the average number of accidents per day of the week, including Hour
avg_accidents <- accidents %>%
  group_by(Weekday, Hour) %>%
  summarize(total_accidents = sum(AccidentCount),
            unique_dates = n_distinct(Date),
            average_accidents_per_day = total_accidents / unique_dates,
            .groups = "drop")

# Create a new grouping column
avg_accidents$Group <- ifelse(avg_accidents$Weekday %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), "Weekday", "Weekend")

# Plotting the average number of accidents by hour and group
ggplot(avg_accidents, aes(x = as.integer(Hour), fill = Group)) +
  geom_bar(aes(y = average_accidents_per_day), stat = "identity", position = "dodge") +
  labs(x = "Hour of Day", y = "Average Accidents per Day", fill = "Day of Week") +
  scale_x_continuous(breaks = 0:23, labels = sprintf("%02d", 0:23)) +
  scale_fill_manual(values = c("Weekday" = "steelblue", "Weekend" = "orange"), name = "Day of Week") +
  theme_minimal()

### Average number of accidents per hour - Weekday vs. Weekend (line chart)

# Calculate the average number of accidents per day of the week, including Hour
avg_accidents <- accidents %>%
  group_by(Weekday, Hour) %>%
  summarize(total_accidents = sum(AccidentCount),
            unique_dates = n_distinct(Date),
            average_accidents_per_day = total_accidents / unique_dates,
            .groups = "drop")

# Create a new grouping column
avg_accidents$Group <- ifelse(avg_accidents$Weekday %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), "Weekday", "Weekend")

# Plotting the average number of accidents by hour and group as a smooth line chart
ggplot(avg_accidents, aes(x = as.integer(Hour), y = average_accidents_per_day, color = Group, group = Group)) +
  geom_line() +
  labs(x = "Hour of Day", y = "Average Accidents per Day", color = "Day of Week") +
  scale_x_continuous(breaks = 0:23, labels = sprintf("%02d", 0:23)) +
  scale_color_manual(values = c("Weekday" = "steelblue", "Weekend" = "orange"), name = "Day of Week") +
  theme_minimal()

### Average number of accidents per hour - Before Covid (bar chart)

avg_accidents_0 <- accidents %>%
  filter(COVID == 0) %>%
  group_by(Weekday, Hour) %>%
  summarize(total_accidents = sum(AccidentCount),
            unique_dates = n_distinct(Date),
            average_accidents_per_day = total_accidents / unique_dates,
            .groups = "drop")

avg_accidents_0$Group <- ifelse(avg_accidents_0$Weekday %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), "Weekday", "Weekend")

ggplot(avg_accidents_0, aes(x = as.integer(Hour), fill = Group)) +
  geom_bar(aes(y = average_accidents_per_day), stat = "identity", position = "dodge") +
  labs(x = "Hour of Day", y = "Average Accidents per Day", fill = "Day of Week") +
  scale_x_continuous(breaks = 0:23, labels = sprintf("%02d", 0:23)) +
  scale_fill_manual(values = c("Weekday" = "steelblue", "Weekend" = "orange"), name = "Day of Week") +
  theme_minimal()

### Average number of accidents per hour - After Covid (bar chart)

avg_accidents_0 <- accidents %>%
  filter(COVID == 1) %>%
  group_by(Weekday, Hour) %>%
  summarize(total_accidents = sum(AccidentCount),
            unique_dates = n_distinct(Date),
            average_accidents_per_day = total_accidents / unique_dates,
            .groups = "drop")

avg_accidents_0$Group <- ifelse(avg_accidents_0$Weekday %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), "Weekday", "Weekend")

ggplot(avg_accidents_0, aes(x = as.integer(Hour), fill = Group)) +
  geom_bar(aes(y = average_accidents_per_day), stat = "identity", position = "dodge") +
  labs(x = "Hour of Day", y = "Average Accidents per Day", fill = "Day of Week") +
  scale_x_continuous(breaks = 0:23, labels = sprintf("%02d", 0:23)) +
  scale_fill_manual(values = c("Weekday" = "steelblue", "Weekend" = "orange"), name = "Day of Week") +
  theme_minimal()


### MAPPING FUNCTION

###Accident Mapping Function
###Combined plot, density and map-only function
###Pass dataframe to function along with optional "DENSITY" (or "D") to change plot to density, "MAP" or "M" for just the map (no charting), otherwise PLOT is default. Did not include error handling yet.
###Must include LONGITUDE, LATITUDE, BOROUGH and DEATHS data as a minimum in df - examples below function
###refer to https://cran.r-project.org/web/packages/ggmap/ggmap.pdf for details about the library (also noted in presentation)

accident_plot <- function(a,plot_type="PLOT") {
  
  #prepare data with temporary headers
  acc_loc<-data.frame(cbind(Longitude=a$LONGITUDE,Latitude=a$LATITUDE,Borough=a$BOROUGH,Deaths=a$DEATHS))
  
  acc_loc$Longitude<-as.numeric(acc_loc$Longitude)
  acc_loc$Latitude<-as.numeric(acc_loc$Latitude)
  acc_loc$Borough<-as.factor(acc_loc$Borough)
  acc_loc$Deaths<-as.numeric(acc_loc$Deaths)
  
  #prepare death locations
  death_loc<-acc_loc[acc_loc$Deaths>0,]
  
  #remove no location data, which will mess up geoplotting
  acc_loc<-subset(acc_loc,acc_loc$Longitude!=0)
  acc_loc<-subset(acc_loc,acc_loc$Latitude!=0)
  
  #find center of map data 
  lon_center<-mean(acc_loc$Longitude)
  lat_center<-mean(acc_loc$Latitude)
  
  #find range of map data
  lon_range <- extendrange(acc_loc$Longitude)
  lat_range <- extendrange(acc_loc$Latitude)
  
  #calculate best zoom value
  zoom_value<-calc_zoom(lon_range, lat_range)
  
  ###maptype character string providing google map theme. options available are "terrain", "satellite", "roadmap", and "hybrid". color can be 'color' or 'bw'. Decreased zoom by 1 in map_image, but could remove if better output - zoom=zoom_value
  map_image<-ggmap(get_googlemap(center=c(lon = lon_center,lat=lat_center),zoom=zoom_value-1,scale=2,maptype="roadmap",color='color'))
  
  if (plot_type=="MAP" | plot_type=="M"){
    map_image
  } else if (plot_type=="DENSITY" | plot_type=="D") {
    map_image + stat_density2d(aes(x = acc_loc$Longitude, y = acc_loc$Latitude, fill = ..level.., alpha = 0.25), size = 0.05, bins = 4, data = acc_loc, geom = "polygon",show.legend = TRUE) +
      geom_density2d(aes(x = acc_loc$Longitude, y = acc_loc$Latitude), size = 0.3, data = acc_loc) + scale_fill_distiller(palette = 'RdYlBu') +
      geom_point(aes(x = death_loc$Longitude, y = death_loc$Latitude), data = death_loc,color="black",label="FATALITY", size = 3) + labs(x = "Longitude", y="Latitude",title="Accident Density",caption="Black dots = Fatalities")
  } else if (plot_type=="PLOT"){
    map_image + geom_point(aes(x = acc_loc$Longitude, y = acc_loc$Latitude,  colour = acc_loc$Borough), alpha=.5, data = acc_loc, size = 0.6,show.legend = TRUE) +
      geom_point(aes(x = death_loc$Longitude, y = death_loc$Latitude), data = death_loc,color="black",label="FATALITY", size = 3) + labs(x = "Longitude", y="Latitude",title="Accident Plot",caption="Black dots = Fatalities") +
      guides(color = guide_legend(override.aes = list(size = 5),title="Boroughs"))
  }
}


### Test of accident_plotting
###Mapping plot and density across all data_mjm
acc_temp<-accidents
test_acc<-data.frame(cbind(LONGITUDE=acc_temp$Long,LATITUDE=acc_temp$Lat,BOROUGH=acc_temp$Borough,DEATHS=acc_temp$Deaths))
accident_plot(test_acc,"DENSITY")
# A different plot view is here:
accident_plot(test_acc)

# ###Mapping plot and density across selected data - subset is one way_mjm
acc_temp<-accidents
# #  possible delete?
# # #pick one or more examples    possible delete?
# acc_temp<-subset(acc_temp,acc_temp$Morning==1)   possible delete?
acc_temp<-subset(acc_temp,acc_temp$Month=="January")
acc_temp<-subset(acc_temp,acc_temp$CInjuries!=0)
acc_temp<-subset(acc_temp,acc_temp$PInjuries!=0)
acc_temp<-subset(acc_temp,acc_temp$Borough=="BRONX")

test_acc<-data.frame(cbind(LONGITUDE=acc_temp$Long,LATITUDE=acc_temp$Lat,BOROUGH=acc_temp$Borough,DEATHS=acc_temp$Deaths))
accident_plot(test_acc,"DENSITY")
accident_plot(test_acc)

###acc_temp<-subset(acc_temp,acc_temp$Borough=="BRONX")_mjm

#just get a map from the above with “MAP” or “M” option - in case you don’t want plot, or custom plot
accident_plot(test_acc,"MAP")


###Mapping Case Study - Accident Intersection using ZIP versus Using Lat/Long
###AKA Why ZIP codes are not sufficient...
###ZIP codes are only present in 64% of the cases, so we can either use the ZIP with less data, or use the ZIP to get min/max boundaries of Lat/Long to use in a new map.
###Below we use ZIP to get boundaries, then use the boundaries to get all Lat/Long in that domains

###Unfortunately if an accident has only ZIP we cannot properly place it in the plot
###Ex: Boerum Hill, Brooklyn 11217
acc_temp<-accidents
acc_temp<-subset(acc_temp,acc_temp$ZIP=="11217")
acc_temp<-subset(acc_temp,acc_temp$Long!=0)
acc_temp<-subset(acc_temp,acc_temp$Lat!=0)

###Boundaries min/max of the Lat/Long within the ZIP - this does assume there is sufficient data in ZIP to encompass min/max
min_lat<-min(acc_temp$Lat)
max_lat<-max(acc_temp$Lat)
min_long<-min(acc_temp$Long)
max_long<-max(acc_temp$Long)

acc_temp<-accidents

###Use boundaries for LatLong subset of df
acc_temp<-subset(acc_temp,acc_temp$Lat>=min_lat & acc_temp$Lat<=max_lat)
acc_temp<-subset(acc_temp,acc_temp$Long>=min_long & acc_temp$Long<=max_long)

test_acc<-data.frame(cbind(LONGITUDE=acc_temp$Long,LATITUDE=acc_temp$Lat,BOROUGH=acc_temp$Borough,DEATHS=acc_temp$Deaths))

#pick a plot
accident_plot(test_acc,"DENSITY")
#accident_plot(test_acc,"MAP") - demonstrating multiple ways to do this, but we like Density
#accident_plot(test_acc,"PLOT") - demonstrating multiple ways to do this, but we like Density

###ZIP alone for 11217 n=380

###Using ZIP to find Lat/Long boundaries, and then searching Lat/Long increases to n=656
###More complete map if we don’t know the Lat/Long boundaries in advance - which would also allow us to develop a multi-ZIP boundary with min/max


#########

###Mapping Case Study - Adjacent ZIP codes for larger area
###Ex: 11217 Boerum Hill 11238 Prospect Heights
###combining in a subset, whether adjacent or disconnected

acc_temp<-accidents
acc_temp<-subset(acc_temp,acc_temp$ZIP=="11217" | acc_temp$ZIP=="11238")
acc_temp<-subset(acc_temp,acc_temp$Long!=0)
acc_temp<-subset(acc_temp,acc_temp$Lat!=0)

###Boundaries min/max
min_lat<-min(acc_temp$Lat)
max_lat<-max(acc_temp$Lat)
min_long<-min(acc_temp$Long)
max_long<-max(acc_temp$Long)

acc_temp<-accidents

###Use boundaries for LatLong search
acc_temp<-subset(acc_temp,acc_temp$Lat>=min_lat & acc_temp$Lat<=max_lat)
acc_temp<-subset(acc_temp,acc_temp$Long>=min_long & acc_temp$Long<=max_long)

test_acc<-data.frame(cbind(LONGITUDE=acc_temp$Long,LATITUDE=acc_temp$Lat,BOROUGH=acc_temp$Borough,DEATHS=acc_temp$Deaths))

accident_plot(test_acc,"DENSITY")
# accident_plot(test_acc,"MAP") - other options
# accident_plot(test_acc,"PLOT")- other options

###n=1134

###CASE Study: Use a Borough to get all covered ZIP codes, then use ZIP codes to get Lat/Long boundaries, then get plots
###This improves the overall dataset from a Borough, yet does require evaluation of the ZIP data if it is correct. Each steps adds potential value and noise.
acc_temp<-accidents

acc_temp<-subset(acc_temp,acc_temp$Borough=="BROOKLYN")
acc_zips<-as.factor(acc_temp$ZIP)

acc_temp<-accidents
acc_temp<-subset(acc_temp,acc_temp$ZIP %in% c(acc_zips))

acc_temp<-subset(acc_temp,acc_temp$Long!=0)
acc_temp<-subset(acc_temp,acc_temp$Lat!=0)

###Boundaries min/max
min_lat<-min(acc_temp$Lat)
max_lat<-max(acc_temp$Lat)
min_long<-min(acc_temp$Long)
max_long<-max(acc_temp$Long)

acc_temp<-accidents

acc_temp<-subset(acc_temp,acc_temp$Lat>=min_lat & acc_temp$Lat<=max_lat)
acc_temp<-subset(acc_temp,acc_temp$Long>=min_long & acc_temp$Long<=max_long)

test_acc<-data.frame(cbind(LONGITUDE=acc_temp$Long,LATITUDE=acc_temp$Lat,BOROUGH=acc_temp$Borough,DEATHS=acc_temp$Deaths))
accident_plot(test_acc,"DENSITY")
# accident_plot(test_acc,"MAP") - other options
# accident_plot(test_acc,"PLOT")- other options
###example with just Borough=="BROOKLYN"


# Create a df with columns from accidents
test_acc<-data.frame(cbind(LONGITUDE,LATITUDE,BOROUGH,DEATHS=accidents$Deaths))
# Optional limit to a borough
# test_acc<-subset(test_acc,test_acc$BOROUGH=="STATEN ISLAND")
accident_plot(test_acc,"DENSITY")
# another worthwhile view:
accident_plot(test_acc)

# Or create a temp set acc_temp to filter to other values in accidents
acc_temp<-accidents
acc_temp<-subset(accidents,accidents$Morning==1)
test_acc<-data.frame(cbind(LONGITUDE=acc_temp$Long,LATITUDE=acc_temp$Lat,BOROUGH=acc_temp$Borough,DEATHS=acc_temp$Deaths))
accident_plot(test_acc,"DENSITY")
# another worthwhile view:
accident_plot(test_acc)



### CLUSTER ANALYSIS

#######  CLUSTER ANLYSIS ################
### SET UP DATA ###
# duplicate original dataset and modify the new set to not disrupt the original data set:
accidents_delete = accidents
names(accidents_delete)

# delete where Lat and Long are blank
accidents_clean = accidents_delete[complete.cases(accidents_delete$Lat), ]
accidents_clean = accidents_delete[complete.cases(accidents_delete$Long), ]

# used below for data types, because to run Cluster We need them to be Numeric:
# Ran each col
class(accidents_clean$PDeaths)

names(accidents_clean)


# Add column for Deaths / Injuries to be numeric:
accidents_clean$Death_Num <- as.numeric(accidents_clean$Deaths)
accidents_clean$Injuries_Num <- as.numeric(accidents_clean$Injuries)
accidents_clean$InjDeath = as.numeric(accidents_clean$InjuriesDeaths)
accidents_clean$Ped_InjDeath =as.numeric(accidents_clean$PInjuriesDeaths)
accidents_clean$Cyc_InjDeath = as.numeric(accidents_clean$CInjuriesDeaths)
accidents_clean$Moto_InjDeath = as.numeric(accidents_clean$MInjuriesDeaths)
accidents_clean$Ped_Injury = as.numeric(accidents_clean$PInjuries)
accidents_clean$Ped_Death = as.numeric(accidents_clean$PDeaths)                                            
accidents_clean$Cyc_Injury = as.numeric(accidents_clean$CInjuries)
accidents_clean$Cyc_Death = as.numeric(accidents_clean$CDeaths)
accidents_clean$Moto_Injjury = as.numeric(accidents_clean$MInjuries)
accidents_clean$Moto_Death = as.numeric(accidents_clean$MDeaths)

# Check the new columns add, are they there:
head(accidents_clean)

# check for no NA values, should all be 0 value:
sum(is.na(accidents_clean$Month))
sum(is.na(accidents_clean$Long))
sum(is.na(accidents_clean$Lat))
sum(is.na(accidents_clean$Injuries_Num))
sum(is.na(accidents_clean$Death_Num))
sum(is.na(accidents_clean$WeekdayNum))
sum(is.na(accidents_clean$InjDeath))
sum(is.na(accidents_clean$Ped_InjDeath))
sum(is.na(accidents_clean$Cyc_InjDeath))
sum(is.na(accidents_clean$Moto_InjDeath))
sum(is.na(accidents_clean$Ped_Injury))
sum(is.na(accidents_clean$Ped_Death))
sum(is.na(accidents_clean$Cyc_Injury))
sum(is.na(accidents_clean$Cyc_Death))
sum(is.na(accidents_clean$Moto_Injjury))
sum(is.na(accidents_clean$Moto_Death))
sum(is.na(accidents_clean$COVID))
sum(is.na(accidents_clean$TimeCategory))

############  SHOW THE IMPORTANCE OF DEATH ON THE DATASET AS A WHOLE ########
# 1st Cluster being ran to show that Deaths & Injuries are the most important pieces of the data set:
# Subset the data to the columns we want to cluster on
# Long and Lat data is irrelevant here - since it doesn't provide that area
accidents_cluster <- accidents_clean[c("Injuries_Num","Death_Num", "Month", "WeekdayNum", "COVID", "TimeCategory")]

# Scale the data
accidents_cluster_scale <- scale(accidents_cluster)

# Determine the optimal number of clusters using the elbow method
wss <- sapply(1:10, function(k){kmeans(accidents_cluster_scale, k, nstart=10)$tot.withinss})
plot(1:10, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

# Perform k-means clustering with 7 clusters (elbow starts around there)
set.seed(1)
accidents_cluster_kmeans <- kmeans(accidents_cluster_scale, 7, nstart=25)


# Visualize the clustering results, takes some time to run
fviz_cluster(accidents_cluster_kmeans, data = accidents_cluster_scale)

# Take a look at the Cluster profiles to see how the clusters correspond with data:  
cluster_profiles <- aggregate(accidents_cluster_scale, by=list(accidents_cluster_kmeans$cluster), FUN=mean)
cluster_profiles

#### Analysis of first Cluster Set #####
# We used the general data set here to get a feel for the data.  Did not include each day broken down, used the 
# column with it marked 1 - 7.  Likewise for time category.  This is not the most detailed Cluster, however, it does 
# show the importance of Deaths and Injuries when looking at this data as a whole.  
# If you look at the graph and the Cluster Profiles, you will see that Cluster 6 is almost on it's own island.
# Cluster 6 also has the most Deaths.  It only overlaps with cluster 5, which has the most Injuries. Looking 
# at Cluster 5, it too is mostly away from the other clusters. Although, there is some overlap. 
#
# This cluster displays that Deaths and Injuries are the two most important categories in this entire data set.
# We will run another more detailed Cluster to find more observations, now that we are comfortable that this 
# data set is showing us what we initially desired from our proposal:  that deaths and injuries would be the main
# driver of this data set.  

# Now that we have verified that the data set appears to be what we initially wanted.  We will run another cluster 
# analysis to dig deeper in this data set.


######## Second Cluster SET ####################
# Rerunning with individually broken down columns instead of single columns: 

accidents_cluster2 <- accidents_clean[c("Morning","Midday","Afternoon","Evening", "Overnight","Sunday","Monday","Tuesday","Wednesday",
                                        "Thursday","Friday","Saturday","COVID")]

# Scale the data
accidents_cluster_scale2 <- scale(accidents_cluster2)

# Determine the optimal number of clusters using the elbow method, takes time to run
wss <- sapply(1:15, function(k){kmeans(accidents_cluster_scale2, k, nstart=15)$tot.withinss})
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares", main="Cluster2 Elbow: Days, Time & COVID")


### Then start running from here again ####
# Perform k-means clustering with 8 clusters (Line chart goes straight down, so do not want to many clusters, 
# 10 Clusters does not seem overwhelming)
set.seed(2)
accidents_cluster_kmeans2 <- kmeans(accidents_cluster_scale2, 8, nstart=25)

# Visualize the clustering results
fviz_cluster(accidents_cluster_kmeans2, data = accidents_cluster_scale2, main="Cluster2: Days, Time & COVID")

# Take a look at the Cluster profiles to see how the clusters correspond with data:  
cluster_profiles2 <- aggregate(accidents_cluster_scale2, by=list(accidents_cluster_kmeans2$cluster), FUN=mean)
cluster_profiles2

# of data in each cluster
table(accidents_cluster_kmeans2$cluster)

# Cluster Means:
cluster_means2 <- aggregate(accidents_cluster2, by=list(accidents_cluster_kmeans2$cluster), FUN=mean)
cluster_means2

## ANALYSIS ####
# This is really interesting.  So the clusters are following the days of the week for the most part, with the exception of 
# Cluster 8 which is mostly Overnight across the different days of the week.  This leads me to believe that something is 
# Occurring on Overnight compared to the other data.
# Also, look at COVID's mean, it's around 0.5 on all clusters.  Since, this data is either 0 or 1, this leads me to believe
# that Pre-Covid vs After Covid doesn't really matter in regards to predicting accidents.


#### Running Stepwise Analyis on Cluster 2a #######

# I want accidents_clean$InjuriesDeaths to be my response variable, since it takes 
# into account all Injuries and Deaths

# To do this I need to rerun the data in cluster 2 with Death & Injuries col so that
# I have the right lengths when running regression.  I am also going to remove COVID, since I do not
# feel like it makes a difference. 


accidents_cluster2a <- accidents_clean[c("Morning","Midday","Afternoon","Evening", "Overnight","Sunday","Monday","Tuesday","Wednesday",
                                         "Thursday","Friday","Saturday","InjuriesDeaths")]
# Scale the data
accidents_cluster_scale2a <- scale(accidents_cluster2a)
# Determine the optimal number of clusters using the elbow method
wss <- sapply(1:15, function(k){kmeans(accidents_cluster_scale2a, k, nstart=15)$tot.withinss})
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")


# Perform k-means clustering with 8 clusters (Line chart goes straight down, so do not want to many clusters, 
# 8 Clusters does not seem overwhelming)
set.seed(22)
accidents_cluster_kmeans2a <- kmeans(accidents_cluster_scale2a, 8, nstart=25)
# Visualize the clustering results
fviz_cluster(accidents_cluster_kmeans2a, data = accidents_cluster_scale2a)

# Take a look at the Cluster profiles to see how the clusters correspond with data:  
cluster_profiles2a <- aggregate(accidents_cluster_scale2a, by=list(accidents_cluster_kmeans2a$cluster), FUN=mean)
cluster_profiles2a

# of data in each cluster
table(accidents_cluster_kmeans2a$cluster)

# Cluster Means:
cluster_means2a <- aggregate(accidents_cluster2a, by=list(accidents_cluster_kmeans2a$cluster), FUN=mean)
cluster_means2a

# Analysis:
# Looking at this data suggests that the highest probability time of being involved in an Injury or #Death by vehicle occurs during Saturday or Sunday at night, 7pm - 6am as demonstrated by #cluster 6.  Oddly, this is also the least likely time to be in an accident, as this cluster has the #lowest amount of data in it.



### NOW LET'S TRY STEPWISE  ######
# Create a list to store the results for each cluster group
stepwise_results <- list()

# Loop through each cluster group
for (i in 1:8) {
  # Subset the data for the i-th cluster group
  cluster_data <- subset(accidents_cluster2a, accidents_cluster_kmeans2a$cluster == i)
  
  # Perform stepwise regression on the cluster data
  stepwise_model <- step(lm(InjuriesDeaths ~ ., data = cluster_data))
  
  # Store the results for the i-th cluster group in the list
  stepwise_results[[paste0("Group", i)]] <- summary(stepwise_model)
}

# View the results for the cluster groups. Recommend viewing one by one
stepwise_results$Group1
stepwise_results$Group2
stepwise_results$Group3
stepwise_results$Group4
stepwise_results$Group5
stepwise_results$Group6
stepwise_results$Group7
stepwise_results$Group8


### Analysis
# One thing consistent with the data is that there are low R^2 values on all of the stepwise       
# results.  This isn’t necessarily bad.  While most may think that this may be a bad model.  It is 
# entirely possible that there is just a greater amount of variation in this dataset.  
# People are hard to predict.  Tesla hasn’t quite perfected that self driving thing, so people are
# still mostly driving cars.  
# We can still use this data to interpret relationships in the variables.
# However, After running a lot of datasets stepwise, it didn’t make such an impact that I wanted
# to focus on it.  I have removed the stepwise from other clusters that I had ran.

######## CLUSTER 3:  Morning, Deaths & Injuries #############

#This is just a countdown:  "Midday","Afternoon","Evening", "Overnight"
accidents_cluster3 <- accidents_clean[c("Morning","PInjuries","PDeaths","CInjuries", "CDeaths","MInjuries" ,"MDeaths")]
# Scale the data
accidents_cluster_scale3 <- scale(accidents_cluster3)
# Determine the optimal number of clusters using the elbow method
wss <- sapply(1:10, function(k){kmeans(accidents_cluster_scale3, k, nstart=10)$tot.withinss})
plot(1:10, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares", main="Cluster3 Elbow: Time, Deaths & Injuries")


# Perform k-means clustering with 6 clusters 
set.seed(3)
accidents_cluster_kmeans3 <- kmeans(accidents_cluster_scale3, 6, nstart=25)
# Visualize the clustering results
fviz_cluster(accidents_cluster_kmeans3, data = accidents_cluster_scale3, main="Cluster3: Morning Deaths & Injuries")

# Take a look at the Cluster profiles to see how the clusters correspond with data:  
cluster_profiles3 <- aggregate(accidents_cluster_scale3, by=list(accidents_cluster_kmeans3$cluster), FUN=mean)
cluster_profiles3

# of data in each cluster
table(accidents_cluster_kmeans3$cluster)

# Cluster Means:
cluster_means3 <- aggregate(accidents_cluster3, by=list(accidents_cluster_kmeans3$cluster), FUN=mean)
cluster_means3

##### Analysis:  I ran a few different versions of the model above.  In the end I found better data
##### when looking at Deaths and Injuries separately.  Rather than show a bunch of failed code,
##### Lets take a look at those next.

######## CLUSTER 4 : SEPERATE INJURIES by DAY:  MONDAY   #############

accidents4 <- accidents_clean[c("Morning", "Midday", "Afternoon", "Evening", "Overnight", 
                                 "Monday",  
                                 "Injuries_Num",   "Ped_Injury", 
                                 "Cyc_Injury",  "Moto_Injjury")]



# get rid o 0 value in Injuries
accidents_cluster4  = accidents4[accidents4$Injuries != 0,]
#check
accidents_cluster4



# Scale the data
accidents_cluster_scale4 <- scale(accidents_cluster4)
# Determine the optimal number of clusters using the elbow method
wss <- sapply(1:10, function(k){kmeans(accidents_cluster_scale4, k, nstart=10)$tot.withinss})
plot(1:10, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares", main="Cluster4 Elbow: All Injuries Monday")

# Perform k-means clustering with 4 clusters 
set.seed(4)
accidents_cluster_kmeans4 <- kmeans(accidents_cluster_scale4, 4, nstart=25)
# Visualize the clustering results
fviz_cluster(accidents_cluster_kmeans4, data = accidents_cluster_scale4, main="Cluster4 All Injuries Monday")

# Take a look at the Cluster profiles to see how the clusters correspond with data:  
cluster_profiles4 <- aggregate(accidents_cluster_scale4, by=list(accidents_cluster_kmeans4$cluster), FUN=mean)
cluster_profiles4

# of data in each cluster
table(accidents_cluster_kmeans4$cluster)

# Cluster Means:
cluster_means4 <- aggregate(accidents_cluster4, by=list(accidents_cluster_kmeans4$cluster), FUN=mean)
cluster_means4



######## CLUSTER 5: SEPERATE INJURIES by DAY:  Tuesday   #############

accidents5 <- accidents_clean[c("Morning", "Midday", "Afternoon", "Evening", "Overnight", 
                                 "Tuesday",  
                                 "Injuries_Num",   "Ped_Injury", 
                                 "Cyc_Injury",  "Moto_Injjury")]



# get rid o 0 value in Injuries
accidents_cluster5  = accidents5[accidents5$Injuries != 0,]
#check
accidents_cluster5

# Scale the data
accidents_cluster_scale5 <- scale(accidents_cluster5)
# Determine the optimal number of clusters using the elbow method
wss <- sapply(1:10, function(k){kmeans(accidents_cluster_scale5, k, nstart=10)$tot.withinss})
plot(1:10, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares", main="Cluster5 Elbow: All Injuries Tuesday")

# Perform k-means clustering with 4 clusters 
set.seed(5)
accidents_cluster_kmeans5 <- kmeans(accidents_cluster_scale5, 4, nstart=25)
# Visualize the clustering results
fviz_cluster(accidents_cluster_kmeans5, data = accidents_cluster_scale5, main="Cluster5 All Injuries Tuesday")

# Take a look at the Cluster profiles to see how the clusters correspond with data:  
cluster_profiles5 <- aggregate(accidents_cluster_scale5, by=list(accidents_cluster_kmeans5$cluster), FUN=mean)
cluster_profiles5

# of data in each cluster
table(accidents_cluster_kmeans5$cluster)

# Cluster Means:
cluster_means5 <- aggregate(accidents_cluster5, by=list(accidents_cluster_kmeans5$cluster), FUN=mean)
cluster_means5



######## CLUSTER 6: SEPERATE INJURIES by DAY:  Wednesday   #############

accidents6 <- accidents_clean[c("Morning", "Midday", "Afternoon", "Evening", "Overnight", 
                                 "Wednesday",  
                                 "Injuries_Num",   "Ped_Injury", 
                                 "Cyc_Injury",  "Moto_Injjury")]



# get rid o 0 value in Injuries
accidents_cluster6  = accidents6[accidents6$Injuries != 0,]
#check
accidents_cluster6


# Scale the data
accidents_cluster_scale6 <- scale(accidents_cluster6)
# Determine the optimal number of clusters using the elbow method
wss <- sapply(1:10, function(k){kmeans(accidents_cluster_scale6, k, nstart=10)$tot.withinss})
plot(1:10, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares", main="Cluster10 Elbow: All Injuries Wednesday")

# Perform k-means clustering with 4 clusters 
set.seed(6)
accidents_cluster_kmeans6 <- kmeans(accidents_cluster_scale6, 4, nstart=25)
# Visualize the clustering results
fviz_cluster(accidents_cluster_kmeans6, data = accidents_cluster_scale6, main="Cluster6 All Injuries Wednesday")

# Take a look at the Cluster profiles to see how the clusters correspond with data:  
cluster_profiles6 <- aggregate(accidents_cluster_scale6, by=list(accidents_cluster_kmeans6$cluster), FUN=mean)
cluster_profiles6

# of data in each cluster
table(accidents_cluster_kmeans6$cluster)

# Cluster Means:
cluster_means6 <- aggregate(accidents_cluster6, by=list(accidents_cluster_kmeans6$cluster), FUN=mean)
cluster_means6

######## CLUSTER 7: SEPERATE INJURIES by DAY:  Thursday   #############

accidents7 <- accidents_clean[c("Morning", "Midday", "Afternoon", "Evening", "Overnight", 
                                 "Thursday",  
                                 "Injuries_Num",   "Ped_Injury", 
                                 "Cyc_Injury",  "Moto_Injjury")]



# get rid o 0 value in Injuries
accidents_cluster7  = accidents7[accidents7$Injuries != 0,]
#check
accidents_cluster7


# Scale the data
accidents_cluster_scale7 <- scale(accidents_cluster7)
# Determine the optimal number of clusters using the elbow method
wss <- sapply(1:10, function(k){kmeans(accidents_cluster_scale7, k, nstart=10)$tot.withinss})
plot(1:10, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares", main="Cluster7 Elbow: All Injuries Thursday")

# Perform k-means clustering with 4 clusters 
set.seed(7)
accidents_cluster_kmeans7 <- kmeans(accidents_cluster_scale7, 4, nstart=25)
# Visualize the clustering results
fviz_cluster(accidents_cluster_kmeans7, data = accidents_cluster_scale7, main="Cluster7 All Injuries Thursday")

# Take a look at the Cluster profiles to see how the clusters correspond with data:  
cluster_profiles7 <- aggregate(accidents_cluster_scale7, by=list(accidents_cluster_kmeans7$cluster), FUN=mean)
cluster_profiles7

# of data in each cluster
table(accidents_cluster_kmeans7$cluster)

# Cluster Means:
cluster_means7 <- aggregate(accidents_cluster7, by=list(accidents_cluster_kmeans7$cluster), FUN=mean)
cluster_means7


######## CLUSTER 8: SEPERATE INJURIES by DAY:  Friday   #############

accidents8 <- accidents_clean[c("Morning", "Midday", "Afternoon", "Evening", "Overnight", 
                                 "Friday",  
                                 "Injuries_Num",   "Ped_Injury", 
                                 "Cyc_Injury",  "Moto_Injjury")]



# get rid o 0 value in Injuries
accidents_cluster8  = accidents8[accidents8$Injuries != 0,]
#check
accidents_cluster8



# Scale the data
accidents_cluster_scale8 <- scale(accidents_cluster8)
# Determine the optimal number of clusters using the elbow method
wss <- sapply(1:10, function(k){kmeans(accidents_cluster_scale8, k, nstart=10)$tot.withinss})
plot(1:10, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares", main="Cluster10 Elbow: All Injuries Friday")

# Perform k-means clustering with 4 clusters 
set.seed(8)
accidents_cluster_kmeans8 <- kmeans(accidents_cluster_scale8, 4, nstart=25)
# Visualize the clustering results
fviz_cluster(accidents_cluster_kmeans8, data = accidents_cluster_scale8, main="Cluster10 All Injuries Friday")

# Take a look at the Cluster profiles to see how the clusters correspond with data:  
cluster_profiles8 <- aggregate(accidents_cluster_scale8, by=list(accidents_cluster_kmeans8$cluster), FUN=mean)
cluster_profiles8

# of data in each cluster
table(accidents_cluster_kmeans8$cluster)

# Cluster Means:
cluster_means8 <- aggregate(accidents_cluster8, by=list(accidents_cluster_kmeans8$cluster), FUN=mean)
cluster_means8

######## CLUSTER 9: SEPERATE INJURIES by DAY:  Saturday   #############

accidents9 <- accidents_clean[c("Morning", "Midday", "Afternoon", "Evening", "Overnight", 
                                 "Saturday",  
                                 "Injuries_Num",   "Ped_Injury", 
                                 "Cyc_Injury",  "Moto_Injjury")]

# get rid o 0 value in Injuries
accidents_cluster9  = accidents9[accidents9$Injuries != 0,]
#check
accidents_cluster9

# Scale the data
accidents_cluster_scale9 <- scale(accidents_cluster9)
# Determine the optimal number of clusters using the elbow method
wss <- sapply(1:10, function(k){kmeans(accidents_cluster_scale9, k, nstart=10)$tot.withinss})
plot(1:10, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares", main="Cluster10 Elbow: All Injuries Saturday")

# Perform k-means clustering with 4 clusters 
set.seed(9)
accidents_cluster_kmeans9 <- kmeans(accidents_cluster_scale9, 4, nstart=25)
# Visualize the clustering results
fviz_cluster(accidents_cluster_kmeans9, data = accidents_cluster_scale9, main="Cluster10 All Injuries Saturday")

# Take a look at the Cluster profiles to see how the clusters correspond with data:  
cluster_profiles9 <- aggregate(accidents_cluster_scale9, by=list(accidents_cluster_kmeans9$cluster), FUN=mean)
cluster_profiles9

# of data in each cluster
table(accidents_cluster_kmeans9$cluster)

# Cluster Means:
cluster_means9 <- aggregate(accidents_cluster9, by=list(accidents_cluster_kmeans9$cluster), FUN=mean)
cluster_means9

########### CLUSTER 10: SEPERATE INJURIES by DAY:  SUNDAY ####################

accidents10 <- accidents_clean[c("Morning", "Midday", "Afternoon", "Evening", "Overnight", 
                                 "Sunday",  
                                 "Injuries_Num",   "Ped_Injury", 
                                 "Cyc_Injury",  "Moto_Injjury")]

# get rid o 0 value in Injuries
accidents_cluster10  = accidents10[accidents10$Injuries != 0,]
#check
accidents_cluster10

# Scale the data
accidents_cluster_scale10 <- scale(accidents_cluster10)
# Determine the optimal number of clusters using the elbow method
wss <- sapply(1:10, function(k){kmeans(accidents_cluster_scale10, k, nstart=10)$tot.withinss})
plot(1:10, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares", main="Cluster10 Elbow: All Injuries Sunday")

# Perform k-means clustering with 4 clusters 
set.seed(10)
accidents_cluster_kmeans10 <- kmeans(accidents_cluster_scale10, 4, nstart=25)
# Visualize the clustering results
fviz_cluster(accidents_cluster_kmeans10, data = accidents_cluster_scale10, main="Cluster10 All Injuries Sunday")

# Take a look at the Cluster profiles to see how the clusters correspond with data:  
cluster_profiles10 <- aggregate(accidents_cluster_scale10, by=list(accidents_cluster_kmeans10$cluster), FUN=mean)
cluster_profiles10

# of data in each cluster
table(accidents_cluster_kmeans10$cluster)

# Cluster Means:
cluster_means10 <- aggregate(accidents_cluster10, by=list(accidents_cluster_kmeans10$cluster), FUN=mean)
cluster_means10



##################  DEATH DATA FOR EACH DAY  ############################
###########  CLUSTER 11: SEPERATE Deaths by DAY:  Monday

accidents11 <- accidents_clean[c("Morning", "Midday", "Afternoon", "Evening", "Overnight", 
                                 "Monday",  
                                 "Death_Num",   "Ped_Death", 
                                 "Cyc_Death",  "Moto_Death")]

# get rid o 0 value in Deaths
accidents_cluster11  = accidents11[accidents11$Death != 0,]
#check
accidents_cluster11

# Scale the data
accidents_cluster_scale11 <- scale(accidents_cluster11)
# Determine the optimal number of clusters using the elbow method
wss <- sapply(1:10, function(k){kmeans(accidents_cluster_scale11, k, nstart=10)$tot.withinss})
plot(1:10, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares", main="Cluster11 Elbow: All Deaths Monday")

# Perform k-means clustering with 4 clusters 
set.seed(11)
accidents_cluster_kmeans11 <- kmeans(accidents_cluster_scale11, 4, nstart=25)
# Visualize the clustering results
fviz_cluster(accidents_cluster_kmeans11, data = accidents_cluster_scale11, main="Cluster11 All Deaths Monday")

# Take a look at the Cluster profiles to see how the clusters correspond with data:  
cluster_profiles11 <- aggregate(accidents_cluster_scale11, by=list(accidents_cluster_kmeans11$cluster), FUN=mean)
cluster_profiles11

# of data in each cluster
table(accidents_cluster_kmeans11$cluster)

# Cluster Means:
cluster_means11 <- aggregate(accidents_cluster11, by=list(accidents_cluster_kmeans11$cluster), FUN=mean)
cluster_means11

############### CLUSTER 12: SEPERATE Deaths by DAY:  Tuesday #####################

accidents12 <- accidents_clean[c("Morning", "Midday", "Afternoon", "Evening", "Overnight", 
                                 "Tuesday",  
                                 "Death_Num",   "Ped_Death", 
                                 "Cyc_Death",  "Moto_Death")]



# get rid o 0 value in Deaths
accidents_cluster12  = accidents12[accidents12$Death != 0,]
#check
accidents_cluster12

# Scale the data
accidents_cluster_scale12 <- scale(accidents_cluster12)
# Determine the optimal number of clusters using the elbow method
wss <- sapply(1:10, function(k){kmeans(accidents_cluster_scale12, k, nstart=10)$tot.withinss})
plot(1:10, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares", main="Cluster12 Elbow: All Deaths Tuesday")

# Perform k-means clustering with 4 clusters 
set.seed(12)
accidents_cluster_kmeans12 <- kmeans(accidents_cluster_scale12, 4, nstart=25)
# Visualize the clustering results
fviz_cluster(accidents_cluster_kmeans12, data = accidents_cluster_scale12, main="Cluster12 All Deaths Tuesday")

# Take a look at the Cluster profiles to see how the clusters correspond with data:  
cluster_profiles12 <- aggregate(accidents_cluster_scale12, by=list(accidents_cluster_kmeans12$cluster), FUN=mean)
cluster_profiles12

# of data in each cluster
table(accidents_cluster_kmeans12$cluster)

# Cluster Means:
cluster_means12 <- aggregate(accidents_cluster12, by=list(accidents_cluster_kmeans12$cluster), FUN=mean)
cluster_means12

############### CLUSTER 13: SEPERATE Deaths by DAY:  Wednesday #####################

accidents13 <- accidents_clean[c("Morning", "Midday", "Afternoon", "Evening", "Overnight", 
                                 "Wednesday",  
                                 "Death_Num",   "Ped_Death", 
                                 "Cyc_Death",  "Moto_Death")]


# get rid o 0 value in Deaths
accidents_cluster13  = accidents13[accidents13$Death != 0,]
#check
accidents_cluster13

# Scale the data
accidents_cluster_scale13 <- scale(accidents_cluster13)
# Determine the optimal number of clusters using the elbow method
wss <- sapply(1:10, function(k){kmeans(accidents_cluster_scale13, k, nstart=10)$tot.withinss})
plot(1:10, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares", main="Cluster13 Elbow: All Deaths Wednesday")

# Perform k-means clustering with 4 clusters 
set.seed(13)
accidents_cluster_kmeans13 <- kmeans(accidents_cluster_scale13, 4, nstart=25)
# Visualize the clustering results
fviz_cluster(accidents_cluster_kmeans13, data = accidents_cluster_scale13, main="Cluster13 All Deaths Wednesday")

# Take a look at the Cluster profiles to see how the clusters correspond with data:  
cluster_profiles13 <- aggregate(accidents_cluster_scale13, by=list(accidents_cluster_kmeans13$cluster), FUN=mean)
cluster_profiles13

# of data in each cluster
table(accidents_cluster_kmeans13$cluster)

# Cluster Means:
cluster_means13 <- aggregate(accidents_cluster13, by=list(accidents_cluster_kmeans13$cluster), FUN=mean)
cluster_means13


############### CLUSTER 14: SEPERATE Deaths by DAY:  Thursday #####################

accidents14 <- accidents_clean[c("Morning", "Midday", "Afternoon", "Evening", "Overnight", 
                                 "Thursday",  
                                 "Death_Num",   "Ped_Death", 
                                 "Cyc_Death",  "Moto_Death")]

# get rid o 0 value in Deaths
accidents_cluster14  = accidents14[accidents14$Death != 0,]
#check
accidents_cluster14

# Scale the data
accidents_cluster_scale14 <- scale(accidents_cluster14)
# Determine the optimal number of clusters using the elbow method
wss <- sapply(1:10, function(k){kmeans(accidents_cluster_scale14, k, nstart=10)$tot.withinss})
plot(1:10, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares", main="Cluster14 Elbow: All Deaths Thursday")

# Perform k-means clustering with 4 clusters 
set.seed(14)
accidents_cluster_kmeans14 <- kmeans(accidents_cluster_scale14, 4, nstart=25)
# Visualize the clustering results
fviz_cluster(accidents_cluster_kmeans14, data = accidents_cluster_scale14, main="Cluster14 All Deaths Thursday")

# Take a look at the Cluster profiles to see how the clusters correspond with data:  
cluster_profiles14 <- aggregate(accidents_cluster_scale14, by=list(accidents_cluster_kmeans14$cluster), FUN=mean)
cluster_profiles14

# of data in each cluster
table(accidents_cluster_kmeans14$cluster)

# Cluster Means:
cluster_means14 <- aggregate(accidents_cluster14, by=list(accidents_cluster_kmeans14$cluster), FUN=mean)
cluster_means14

############### CLUSTER 15: SEPERATE Deaths by DAY:  Friday #####################

accidents15 <- accidents_clean[c("Morning", "Midday", "Afternoon", "Evening", "Overnight", 
                                 "Friday",  
                                 "Death_Num",   "Ped_Death", 
                                 "Cyc_Death",  "Moto_Death")]

# get rid o 0 value in Deaths
accidents_cluster15  = accidents15[accidents15$Death != 0,]
#check
accidents_cluster15

# Scale the data
accidents_cluster_scale15 <- scale(accidents_cluster15)
# Determine the optimal number of clusters using the elbow method
wss <- sapply(1:10, function(k){kmeans(accidents_cluster_scale15, k, nstart=10)$tot.withinss})
plot(1:10, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares", main="Cluster15 Elbow: All Deaths Friday")

# Perform k-means clustering with 4 clusters 
set.seed(15)
accidents_cluster_kmeans15 <- kmeans(accidents_cluster_scale15, 4, nstart=25)
# Visualize the clustering results
fviz_cluster(accidents_cluster_kmeans15, data = accidents_cluster_scale15, main="Cluster15 All Deaths Friday")

# Take a look at the Cluster profiles to see how the clusters correspond with data:  
cluster_profiles15 <- aggregate(accidents_cluster_scale15, by=list(accidents_cluster_kmeans15$cluster), FUN=mean)
cluster_profiles15

# of data in each cluster
table(accidents_cluster_kmeans15$cluster)

# Cluster Means:
cluster_means15 <- aggregate(accidents_cluster15, by=list(accidents_cluster_kmeans15$cluster), FUN=mean)
cluster_means15


############### CLUSTER 16: SEPERATE Deaths by DAY:  Saturday Deaths #####################

accidents16 <- accidents_clean[c("Morning", "Midday", "Afternoon", "Evening", "Overnight", 
                                 "Saturday",  
                                 "Death_Num",   "Ped_Death", 
                                 "Cyc_Death",  "Moto_Death")]



# get rid o 0 value in Deaths
accidents_cluster16  = accidents16[accidents16$Death != 0,]
#check
accidents_cluster16

# Scale the data
accidents_cluster_scale16 <- scale(accidents_cluster16)
# Determine the optimal number of clusters using the elbow method
wss <- sapply(1:10, function(k){kmeans(accidents_cluster_scale16, k, nstart=10)$tot.withinss})
plot(1:10, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares", main="Cluster16 Elbow: All Deaths Saturday")

# Perform k-means clustering with 4 clusters 
set.seed(16)
accidents_cluster_kmeans16 <- kmeans(accidents_cluster_scale16, 4, nstart=25)
# Visualize the clustering results
fviz_cluster(accidents_cluster_kmeans16, data = accidents_cluster_scale16, main="Cluster16 All Deaths Saturday")

# Take a look at the Cluster profiles to see how the clusters correspond with data:  
cluster_profiles16 <- aggregate(accidents_cluster_scale16, by=list(accidents_cluster_kmeans16$cluster), FUN=mean)
cluster_profiles16

# of data in each cluster
table(accidents_cluster_kmeans16$cluster)

# Cluster Means:
cluster_means16 <- aggregate(accidents_cluster16, by=list(accidents_cluster_kmeans16$cluster), FUN=mean)
cluster_means16



############### CLUSTER 17: SEPERATE Deaths by DAY:  Sunday  #####################

accidents17 <- accidents_clean[c("Morning", "Midday", "Afternoon", "Evening", "Overnight", 
                                 "Sunday",  
                                 "Death_Num",   "Ped_Death", 
                                 "Cyc_Death",  "Moto_Death")]

# get rid o 0 value in Deaths
accidents_cluster17  = accidents17[accidents17$Death != 0,]
#check
accidents_cluster17

# Scale the data
accidents_cluster_scale17 <- scale(accidents_cluster17)
# Determine the optimal number of clusters using the elbow method
wss <- sapply(1:10, function(k){kmeans(accidents_cluster_scale17, k, nstart=10)$tot.withinss})
plot(1:10, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares", main="Cluster17 Elbow: All Deaths Sunday")

# Perform k-means clustering with 4 clusters 
set.seed(17)
accidents_cluster_kmeans17 <- kmeans(accidents_cluster_scale17, 4, nstart=25)
# Visualize the clustering results
fviz_cluster(accidents_cluster_kmeans17, data = accidents_cluster_scale17, main="Cluster17 All Deaths Sunday")

# Take a look at the Cluster profiles to see how the clusters correspond with data:  
cluster_profiles17 <- aggregate(accidents_cluster_scale17, by=list(accidents_cluster_kmeans17$cluster), FUN=mean)
cluster_profiles17

# of data in each cluster
table(accidents_cluster_kmeans17$cluster)

# Cluster Means:
cluster_means17 <- aggregate(accidents_cluster17, by=list(accidents_cluster_kmeans17$cluster), FUN=mean)
cluster_means17


###### Code that didn't work, saving cluster, regression and stepwise  #####
###### Leaving commented out, as we were unable to make relevant observations ###
###### Feel free to highlight till the end of the document and undue # via (shift+ctrl+c)######

# ######## CLUSTER 8: Run regression on 2a  #############
# 
# accidents_cluster2a <- accidents_clean[c("Morning","Midday","Afternoon","Evening", "Overnight","Sunday","Monday","Tuesday","Wednesday",
#                                          "Thursday","Friday","Saturday","InjuriesDeaths")]
# 
# # Scale the data
# accidents_cluster_scale2a <- scale(accidents_cluster2a)
# 
# # Determine the optimal number of clusters using the elbow method
# wss <- sapply(1:15, function(k){kmeans(accidents_cluster_scale2a, k, nstart=15)$tot.withinss})
# plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
# 
# # Perform k-means clustering with 8 clusters (Line chart goes straight down, so do not want to many clusters,
# # 10 Clusters does not seem overwhelming)
# set.seed(22)
# accidents_cluster_kmeans2a <- kmeans(accidents_cluster_scale2a, 8, nstart=25)
# 
# # Visualize the clustering results
# fviz_cluster(accidents_cluster_kmeans2a, data = accidents_cluster_scale2a)
# 
# # Take a look at the Cluster profiles to see how the clusters correspond with data:
# cluster_profiles2a <- aggregate(accidents_cluster_scale2a, by=list(accidents_cluster_kmeans2a$cluster), FUN=mean)
# cluster_profiles2a
# 
# # of data in each cluster
# table(accidents_cluster_kmeans2a$cluster)
# 
# # Cluster Means:
# cluster_means2a <- aggregate(accidents_cluster2a, by=list(accidents_cluster_kmeans2a$cluster), FUN=mean)
# cluster_means2a
# 
# #### Attempt at Regression on clusters #######
# # Add a new column with the cluster assignments
# accidents_cluster2a$cluster <- as.factor(accidents_cluster_kmeans2a$cluster)
# 
# # Check the structure of the data
# str(accidents_cluster2a)
# 
# 
# # Fit a linear regression model with cluster as a factor variable
# model <- lm(InjuriesDeaths ~ cluster + Morning + Midday + Afternoon + Evening + Sunday + Monday + Tuesday + Wednesday + Thursday + Friday + Saturday, data = accidents_cluster2a)
# 
# # View the summary of the model
# summary(model)
# 
# ######Try stepwise on the regression:
# accidents_stepwise <- step(lm(InjuriesDeaths ~ ., data = accidents_cluster2a), direction = "both")
# 
# # Print summary of the final model
# summary(accidents_stepwise)
