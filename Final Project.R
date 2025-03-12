# This downloads the libraries needed for this project
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(fastDummies)
library(Hmisc)
library(corrplot)

# This reads in both of the sheets in the excel data
RawData <- read_xlsx("R/STA 6233/Project/RawData.xlsx", sheet = "Raw-Data")
Calendar <- read_xlsx("R/STA 6233/Project/RawData.xlsx", sheet = "Calendar")
RawData$'Receipt Date' <- as.Date(RawData$'Receipt Date')
RawData$`PO Download Date` <- as.Date(RawData$`PO Download Date`)
RawData$`Ship Date` <- as.Date(RawData$`Ship Date`)
Calendar$End_date <- as.Date(Calendar$End_date)

# This checks the values of the different columns to see if there are any odd values
names(RawData)
sort(unique(RawData$LOB))
sort(unique(RawData$Origin))
sort(unique(RawData$`Ship Mode`))

# These statements create 2 new columns in the RawData dataset
Receipt <- RawData$`Receipt Date`
RawData <- RawData %>%
  mutate(Quarter = Calendar$Quarter[findInterval(Receipt, Calendar$End_date)])
RawData$Year <- ifelse(Receipt <= Calendar$End_date[2] & Receipt >= Calendar$Start_Date[1], Calendar$Year[2], Calendar$Year[3])

# These statements calculate the In-Transit and Manufacturing Lead Times
leadTimes <- function(df) {
  df <- df %>%
    mutate(
      `Manufacturing Lead Time` = as.numeric(difftime(`Ship Date`, `PO Download Date`, units = "days")),
      `In-transit Lead Time` = as.numeric(difftime(`Receipt Date`, `Ship Date`, units = "days"))
    )
  return(df)
}
RawData <- leadTimes(RawData)

# This function filters out rows with empty values and duplicates
remove_na <- function(df){
  df <- na.omit(df)
}
RawData_clean <- remove_na(RawData)

# Since many of the Manufacturing and In-transit values were negative, all of the values were replaced with the median value
RawData_clean$'Manufacturing Lead Time'[RawData_clean$'Manufacturing Lead Time' < 0 | RawData_clean$'Manufacturing Lead Time' > 100 | is.na(RawData_clean$'Manufacturing Lead Time')] <- round(mean(RawData_clean$'Manufacturing Lead Time', na.rm = TRUE), 0)
RawData_clean$'In-transit Lead Time'[RawData_clean$'In-transit Lead Time' < 0 | is.na(RawData_clean$'In-transit Lead Time')] <- round(mean(RawData_clean$'In-transit Lead Time', na.rm = TRUE), 0)

# This function creates dummy variables which are used to see if a categorical event occurs or not
RawData_clean <- dummy_cols(RawData_clean, select_columns = c("LOB", "Origin", "Ship Mode", "Quarter"))

# Correlation comparing the predictors with In-transit Lead Time
clean_num <- RawData_clean %>%
  select('In-transit Lead Time':Quarter_Q4) %>%
  arrange(desc(`In-transit Lead Time`))
matrixRawData <- cor(clean_num)
corrplot(matrixRawData, method = "square", addCoef.col = 'black', title = "Figure 1: Correlation Between In-transit Lead Time and Predictors", mar=c(0,0,1,0))

corrRawData <- sort(matrixRawData["In-transit Lead Time", ], decreasing = TRUE)
print("Figure 2: Relationship Between In-transit Lead Time With Each Predictor")
print(corrRawData)

# This statement only looks at Site D for the data
# Site D is only on GROUND and only manufactures Product B
siteD <- RawData_clean %>%
  filter(Origin == 'Site D')
sort(unique(siteD$LOB))
sort(unique(siteD$`Ship Mode`))
# This only shows data for Site A as they're the only Site that makes Products A and C
siteA <- RawData_clean %>%
  filter(Origin == 'Site A')

# These create separate data frames for each product
# productA and productC show that Products A and C are only manufactured and transported from site A
# productB is information regarding Product B, which is manufactured at all 4 sites
productA <- RawData_clean %>%
  filter(LOB == 'Product A')
sort(unique(productA$Origin))
productC <- RawData_clean %>%
  filter(LOB == 'Product C')
sort(unique(productC$Origin))
productB <- RawData_clean %>%
  filter(LOB == 'Product B')
sort(unique(productB$Origin))

# Plots comparing In-transit time with the predictors
ggplot(RawData_clean, aes(x = `Manufacturing Lead Time`, y = `In-transit Lead Time`, color = `Ship Mode`)) +
  geom_point() +
  labs(x = "Manufacturing Lead Time", y = "In-transit Lead Time", title = "Figure 3: In-transit vs Manufacturing Lead Times by Ship Mode")

# These plots show how many products are manufactured and transported from each site
# This plot shows how many products come from each site
ggplot(RawData_clean, aes(x = Origin, fill = LOB)) +
  geom_bar() +
  labs(title = "Figure 4: Number of Products From Each Site")

#This compares the times it takes for Product B to be delivered from each site
ggplot(productB, aes(x = `Origin`, y = `In-transit Lead Time`, color = `Origin`)) +
  geom_boxplot() +
  labs(x = "Origin", y = "In-transit Lead Time", title = "Figure 5: In-transit Lead Time Per Site for Product B")

# This compares the in-transit times for each product at site A
ggplot(siteA, aes(x = `LOB`, y = `In-transit Lead Time`, color = `LOB`)) +
  geom_boxplot() +
  labs(x = "LOB", y = "In-transit Lead Time", title = "Figure 6: In-transit Lead Time Per Product from Site A")

# This compares how fast each ship mode takes for product A
ggplot(productA, aes(x = `Ship Mode`, y = `In-transit Lead Time`, color = `Ship Mode`)) +
  geom_boxplot() +
  labs(x = "Ship Mode", y = "In-transit Lead Time", title = "Figure 7: In-transit Lead Time Per Ship Mode for Product A")

# This compares how fast each ship mode takes for product C
ggplot(productC, aes(x = `Ship Mode`, y = `In-transit Lead Time`, color = `Ship Mode`)) +
  geom_boxplot() +
  labs(x = "Ship Mode", y = "In-transit Lead Time", title = "Figure 8: In-transit Lead Time Per Ship Mode for Product C")
