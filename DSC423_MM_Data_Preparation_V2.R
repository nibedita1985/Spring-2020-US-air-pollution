#================================= TITLE BLOCK ================================
#
# DSC 423 Data Analysis Project
#
# Mongollon Monsters Exploratory Analysis
#
# Data = pollution_us_2000_2016.csv
#
# Performed By: Zach Paquin - 5/8/2020
#
# This file walks through the steps taken to prepare the data, and a breif
# analysis of the prepared data. The analysis for Week 4 for the group submission
# is found here too.


#============================== DATA PREPARATION ==============================

# In order to analyze the data, a few perparation steps must be performed first.
# The following steps were performed to prepare the data:
#
# 1. Rows containing NA values were removed 
# 2. Unncessary columns were removed 
# 3. The Region field was added to the data 
# 4. Add Time of Day field
# 5. Add Season Field


#------------------------- 1. REMOVE ROWS WITH NA VALUES ----------------------

# This removes NA values from the original file and saves the subset as
# ProjectData

ProjectData <- pollution_us_2000_2016[complete.cases(pollution_us_2000_2016),]


#------------------------- 2. REMOVE UNNECESSARY COLUMNS ----------------------

# Remove the first five columns and UOM columns

ProjectData$X <- NULL
ProjectData$State.Code <- NULL
ProjectData$County.Code <- NULL
ProjectData$Site.Num <- NULL 
ProjectData$Address <- NULL
ProjectData$NO2.Units <- NULL
ProjectData$O3.Units <- NULL
ProjectData$SO2.Units <- NULL
ProjectData$CO.Units <- NULL

# This was not tested as you can visually see the length decrease as you 
# execute the commands.

# Change 1st max hour column names to add UOM

colnames(ProjectData)

names(ProjectData)[names(ProjectData) == "NO2.1st.Max.Value"] <- "NO2.1st.Max.Value.ppb"
names(ProjectData)[names(ProjectData) == "O3.1st.Max.Value"] <- "O3.1st.Max.Value.ppm"
names(ProjectData)[names(ProjectData) == "SO2.1st.Max.Value"] <- "SO2.1st.Max.Value.ppb"
names(ProjectData)[names(ProjectData) == "CO.1st.Max.Value"] <- "CO.1st.Max.Value.ppm"

#------------------------- 3. ADDING REGION FIELD -----------------------------

# Now to add the Region field
#
# To do this, I initialized an empty vector (Region). I then made a for loop
# to go through each state, and based on the criteria below, the region was 
# assigned and appened to Region. The criteria for assigning a region was
# taken from the Agricultural Research Service (ARS). The regions are as 
# follows:
#
# Midwest      = MN, IA, MO, IL, KY, IN, WI, MI, OH
# Northeast    = ME, NH, VT, NY, MA, CT, RI, PA, NJ, VA, WV, DE, MD, DC
# Southeast    = NC, SC, TN, AR, LA, MS, AL, FL, GA
# Plains       = MT, ND, SD, WY, NE, CO, NM, KS, OK, TX
# Pacific West = WA, OR, ID, UT, AZ, CA, NV, AK, HI

Region <- vector()

for (i in ProjectData$State) {
  
  if (i == "Minnesota" | i == "Iowa" | i == "Missouri" | i == "Illinois" | i == "Kentucky" | i == "Indiana"
      | i == "Wisconsin" | i == "Michigan" | i == "Ohio") {
    
    ireg <- "Midwest"
    Region <- append(Region,ireg)
    
  } else {
  if (i == "Maine" | i == "New Hampshire" | i == "Vermont" | i == "New York" | i == "Massachusetts" | 
      i == "Connecticut" | i == "Rhode Island" | i == "Pennsylvania" | i == "New Jersey" | i == 
      "Virginia" | i == "West Virginia" | i == "Deleware" | i == "Maryland" | i == "DC") {
    
    ireg <- "Northeast"
    Region <- append(Region,ireg)
    
  } else {
    if (i == "North Carolina" | i == "South Carolina" | i == "Tennessee" | i == "Arkansas"
        | i == "Louisana" | i == "Mississippi" | i == "Alabama" | i == "Florida"
        | i == "Georgia") {
      
      ireg <- "Southeast"
      Region <- append(Region,ireg)
      
    } else {
      if (i == "Montana" | i == "North Dakota" | i == "South Dakota" | i == "Wyoming" 
          | i == "Nebraska" | i == "Colorado" | i == "New Mexico" | i == "Kansas" 
          | i == "Oklahoma" | i == "Texas") {
        
        ireg <- "Plains"
        Region <- append(Region,ireg)
        
      } else {
        
      ireg <- "Pacific West"
      Region <- append(Region,ireg)
      
      }
    }
  }
} 

} 

# Now to add a column to ProjectData containing the data in Region

ProjectData$Region <- Region

# Testing of for loop
#
# A subset of ProjectData was created with 3 states from each region
#
# Midwest Test States      = WI, IL, MN
# Northeast Test States    = NY, PA, MD
# Southeast Test States    = TN, LA, FL
# Plains Test States       = TX, MT, SD
# Pacific West Test States = CA, WA, UT

Midwest <- subset(ProjectData, ProjectData$State == "Wisconsin")
head(Midwest)

Midwest <- subset(ProjectData, ProjectData$State == "Illinois")
head(Midwest)

Midwest <- subset(ProjectData, ProjectData$State == "Minnesota")
head(Midwest)

Northeast <- subset(ProjectData, ProjectData$State == "New York")
head(Northeast)

Northeast <- subset(ProjectData, ProjectData$State == "Pennsylvania")
head(Northeast)

Northeast <- subset(ProjectData, ProjectData$State == "Maryland")
head(Northeast)

Southeast <- subset(ProjectData, ProjectData$State == "Tennessee")
head(Southeast)

Southeast <- subset(ProjectData, ProjectData$State == "Louisana")
head(Southeast)

Southeast <- subset(ProjectData, ProjectData$State == "Florida")
head(Southeast)

Plains <- subset(ProjectData, ProjectData$State == "Texas")
head(Plains)

Plains <- subset(ProjectData, ProjectData$State == "Montana")
head(Plains)

Plains <- subset(ProjectData, ProjectData$State == "South Dakota")
head(Plains)

PacificWest <- subset(ProjectData, ProjectData$State == "California")
head(PacificWest)

PacificWest <- subset(ProjectData, ProjectData$State == "Washington")
head(PacificWest)

PacificWest <- subset(ProjectData, ProjectData$State == "Utah")
head(PacificWest)


#---------------------------- 4. ADD TIME OF DAY FIELD ------------------------

# Using the 1st max hour field for each pollutant, I figured we could transform
# those columns into some categorical variables

# If 1st max hour is (range) Time Of Day = <TOD>
#
# 0-5 = Night
# 6-11 = Morning
# 12-17 = Afternoon
# 18-23 = Evening

NO2TOD <- vector()
  
for (i in ProjectData$NO2.1st.Max.Hour) {
  if (i == 0 | i == 1 | i == 2 | i == 3 | i == 4 | i == 5) {
    
    iTOD <- "Night"
    NO2TOD <- append(NO2TOD, iTOD)
    
  } else {
    if (i == 6 | i == 7 | i == 8 | i == 9 | i == 10 | i == 11) {
      
      iTOD <- "Morning"
      NO2TOD <- append(NO2TOD, iTOD)
      
    } else {
      if (i == 12 | i == 13 | i == 14 | i == 15 | i == 16 | i == 17) {
        
        iTOD <- "Afternoon"
        NO2TOD <- append(NO2TOD, iTOD)
        
      } else {
        
        iTOD <- "Evening"
        NO2TOD <- append(NO2TOD, iTOD)
      }
    }
  }
}

ProjectData$NO2.TOD <- NO2TOD

# O3 TOD field

O3TOD <- vector()

for (i in ProjectData$O3.1st.Max.Hour) {
  if (i == 0 | i == 1 | i == 2 | i == 3 | i == 4 | i == 5) {
    
    iTOD <- "Night"
    O3TOD <- append(O3TOD, iTOD)
    
  } else {
    if (i == 6 | i == 7 | i == 8 | i == 9 | i == 10 | i == 11) {
      
      iTOD <- "Morning"
      O3TOD <- append(O3TOD, iTOD)
      
    } else {
      if (i == 12 | i == 13 | i == 14 | i == 15 | i == 16 | i == 17) {
        
        iTOD <- "Afternoon"
        O3TOD <- append(O3TOD, iTOD)
        
      } else {
        
        iTOD <- "Evening"
        O3TOD <- append(O3TOD, iTOD)
        
      }
    }
  }
}

ProjectData$O3.TOD <-O3TOD

# SO2 TOD Field

SO2TOD <- vector()

for (i in ProjectData$SO2.1st.Max.Hour) {
  if (i == 0 | i == 1 | i == 2 | i == 3 | i == 4 | i == 5) {
    
    iTOD <- "Night"
    SO2TOD <- append(SO2TOD, iTOD)
    
  } else {
    if (i == 6 | i == 7 | i == 8 | i == 9 | i == 10 | i == 11) {
      
      iTOD <- "Morning"
      SO2TOD <- append(SO2TOD, iTOD)
      
    } else {
      if (i == 12 | i == 13 | i == 14 | i == 15 | i == 16 | i == 17) {
        
        iTOD <- "Afternoon"
        SO2TOD <- append(SO2TOD, iTOD)
        
      } else {
        
        iTOD <- "Evening"
        SO2TOD <- append(SO2TOD, iTOD)
        
      }
    }
  }
}

ProjectData$SO2.TOD <- SO2TOD

# CO TOD Field

COTOD <- vector()

for (i in ProjectData$CO.1st.Max.Hour) {
  if (i == 0 | i == 1 | i == 2 | i == 3 | i == 4 | i == 5) {
    
    iTOD <- "Night"
    COTOD <- append(COTOD, iTOD)
    
  } else {
    if (i == 6 | i == 7 | i == 8 | i == 9 | i == 10 | i == 11) {
      
      iTOD <- "Morning"
      COTOD <- append(COTOD, iTOD)
      
    } else {
      if (i == 12 | i == 13 | i == 14 | i == 15 | i == 16 | i == 17) {
        
        iTOD <- "Afternoon"
        COTOD <- append(COTOD, iTOD)
        
      } else {
        
        iTOD <- "Evening"
        COTOD <- append(COTOD, iTOD)
        
      }
    }
  }
}

ProjectData$CO.TOD <- COTOD


#---------------------------- 5. ADD SEASON FIELD -----------------------------

# This adds the season based on the month of the reading. The season field
# criteria are:
#
# If month is (range) season = <season>
#
# 12, 1, 2 (Dec, Jan, Feb) = Winter
# 3, 4, 5 (Mar, Apr, May) = Sprint
# 6, 7, 8 (Jun, Jul, Aug) = Summer
# 9, 10, 11 (Sep, Oct, Nov) = Fall


ProjectData$Month <- format(as.Date(ProjectData$Date.Local), "%m")

season <- vector()

for (i in ProjectData$Month) {
  if (i == "12" | i == "01" | i == "02") {
    
    iseason <- "Winter"
    season <- append(season, iseason)
    
  } else {
    if (i == "03" | i == "04" | i == "05") {
      
      iseason <- "Spring"
      season <- append(season, iseason)
      
    } else {
      if (i == "06" | i == "07" | i == "08") {
        
        iseason <- "Summer"
        season <- append(season, iseason)
        
      } else {
        
        iseason <- "Fall"
        season <- append(season, iseason)
      }
    }
  }
}

ProjectData$Season <- season 
ProjectData$Month <- NULL
