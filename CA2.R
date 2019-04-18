#Open Libraries
library(readr)
library(DataCombine)
library(tidyr)
library(dplyr)
library(mice)
library(LearnBayes)
library(tidyverse)

getwd()

#Importing the dataset
NIPostcodes <- read.csv("NIPostcodes.csv", header = FALSE)


# Section - 1

#A) The structure of the dataframe and showing first 10 rows containing all of the  NIPostcode data. 

dim(NIPostcodes)
# Structure of the dataframe
str(NIPostcodes)
# Display first 10 rows
head(NIPostcodes, 10)


# B) Adding suitable title for each attribute of the data

names(NIPostcodes) <- c("Organisation Name","Sub-building Name","Building Name","Number","Primary Thorfare",
                        "Alt Thorfare","Secondary Thorfare","Locality","Townland","Town","County", 
                        "PostCode", "x-coordinates", "y-coordinates", "Primary Key (identifier)")

# C.Handling the missing values, merging columns
# Reordering columns

# NIPostcodes <- NIPostcodes[,c(4, 1:3, 5:15)]
# 
# NIPostcodes$Address <- unite(NIPostcodes, Address, 1:7, sep = " ", remove = TRUE)
str(NIPostcodes)


# D) Showing the total number and mean missing values for each columns in the post code data frame

summary(NIPostcodes)

column_mean <- colMeans(is.na(NIPostcodes))
sapply(NIPostcodes, function(x) sum(is.na(x)))

# md.pattern(NIPostcodes)

# NIPostcodes <- NIPostcodes[,c(8:16)]

#E) Modifying the county attribute to categorising factor

NIPostcodes$County <- as.factor(NIPostcodes$County)
str(NIPostcodes)


#F) Moving the  primary key identifier to the start of the dataset

NIPostcodes <- NIPostcodes[,c(15, 1:14)]
str(NIPostcodes)

#G) Creating new dataset called Limavady

Limavady_data <- data.frame(NIPostcodes$Locality, NIPostcodes$Townland, NIPostcodes$Town)
names(Limavady_data) <- c("Locality", "Townland", "Town")
Limavady_final <- filter(Limavady_data, Locality == 'LIMAVADY' | Townland == 'LIMAVADY' | Town == 'LIMAVADY')
str(Limavady_final)


#H) saving the modified NIPostcode dataset in a csv file called CleanNIPostcodeData

write.csv(Limavady_final, file = "Limavady.csv")
write.csv(NIPostcodes, "CleanNIPostcodeData.csv")
str("CleanNIPostcodeData.csv")


# SECTION - 2

#A) Amalgamating all of the crime data into one dataset

getwd()

# folder <- "NI Crime Data/"
# List_of_files <- list.files(path=folder, pattern=".csv$")
# 
# AllNICrimeData <- do.call("rbind", lapply(List_of_files, function(x), read.csv(paste(folder,x, sep=''), stringsAsFactors = FALSE)))


getwd()

folder <- "./NI Crime Data/"
List_of_files <- list.files(path=folder, pattern="*.csv")

AllNICrimeData <- 
  do.call("rbind", 
          lapply(List_of_files, 
                 function(x) 
                   read.csv(paste(folder, x, sep=''), 
                            stringsAsFactors = FALSE)))


#B) Modifying the structure of the newly created AllNICrimeData

Mod_Crime <- AllNICrimeData[,c(2,5:7,10)]
str(AllNICrimeData)

#C) Factorising the Crime type attributes

Mod_Crime$Crime.type <- as.factor(Mod_Crime$Crime.type)

#D) modifying the AllNICrimeData dataset and modifying the resultant empty location attributes with suitable identifier

Mod_Crime$Location <- gsub(".*On or near", "", Mod_Crime$Location)
Mod_Crime[Mod_Crime == " "] <- NA
Mod_Crime <- na.omit(Mod_Crime)

#E) Functions for choosing 1000 random samples
random_crime_sample = Mod_Crime[sample(nrow(Mod_Crime), "1000"), ]
random_crime_sample$Location <- toupper(random_crime_sample$Location)
random_crime_sample$Location <- trimws(random_crime_sample$Location)

postcode_dataframe_copy <- NIPostcodes
postcode_dataframe_copy <- NIPostcodes[,c(6,13)]
postcode_dataframe_copy[postcode_dataframe_copy == ""] <- NA
postcode_dataframe_copy <- na.omit(postcode_dataframe_copy)
postcode_dataframe_copy <- postcode_dataframe_copy %>% group_by(`Primary Thorfare`, PostCode) %>% mutate(Index= 1:n()) %>% arrange(desc(Index))
postcode_dataframe_copy <- postcode_dataframe_copy[!duplicated(postcode_dataframe_copy$`Primary Thorfare`),]
find_a_postcode <- function(Loc){
random_crime_sample <- merge(x = random_crime_sample, y = postcode_dataframe_copy, by.x= c(Loc), by.y = c("Primary Thorfare"), all.x = TRUE)
random_crime_sample <- na.omit(random_crime_sample)
}

random_crime_sample <- as.data.frame(lapply("Location", find_a_postcode))
random_crime_sample <- random_crime_sample[,c(1:6)]
#F.Append the data output from your find_a_postcode function to the random_crime_sample 

write.csv(random_crime_sample, 'random_crime_sample.csv', row.names = F)


#G.Updating the random sample with certain attributes
#creating another data frame called chart_data and sorting by postcode BT1

update_crime_sample <- random_crime_sample
chart_data <- update_crime_sample[grep("BT1", update_crime_sample$PostCode),]

summary(chart_data)


#H.Creating a bar plot of the crime type from the chart_data data frame
count <- table(chart_data$Crime.type)
# count <- count[(count$Freq > 0),]
# count <- as.data.frame.matrix(count)
# view(count)
# count <- count[,c[3:4]]
barplot(count,main = "crime category" , xlab = "crime types", ylab ="crime types count" , ylim = c(0, 100), beside = T )

# 