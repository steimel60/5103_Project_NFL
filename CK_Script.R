library(tidyverse)
library(lubridate)

#### Data Preparation ####

## Removing NA Rows From Quarter Column
data_2013 <- data_2013 %>% 
  drop_na(Quarter)

## Removing NA's from Date Column for Easier Data Conversion
data_2013 <- data_2013 %>% 
  drop_na(GameDate)

## Converting relevant columns to appropriate column types
data_2013 <- transform(data_2013, GameId = as.numeric(GameId),
                       Quarter = as.numeric(Quarter),
                       Minute = as.numeric(Minute), Second= as.numeric(Second),
                       Down = as.numeric(Down), ToGo = as.numeric(ToGo),
                       YardLine = as.numeric(YardLine), SeriesFirstDown = as.numeric(SeriesFirstDown),
                       NextScore = as.numeric(NextScore), TeamWin = as.numeric(TeamWin),
                       SeasonYear = as.numeric(SeasonYear), Yards = as.numeric(Yards),
                       IsRush = as.numeric(IsRush), IsPass = as.numeric(IsPass),
                       IsIncomplete = as.numeric(IsIncomplete), IsSack = as.numeric(IsSack),
                       IsChallenge = as.numeric(IsChallenge), IsTouchdown = as.numeric(IsTouchdown),
                       IsChallengeReversed = as.numeric(IsChallengeReversed),
                       IsMeasurement = as.numeric(IsMeasurement), IsInterception = as.numeric(IsInterception),
                       IsFumble = as.numeric(IsFumble), IsPenalty = as.numeric(IsPenalty), IsTwoPointConversion = as.numeric(IsTwoPointConversion),
                       IsTwoPointConversionSuccessful = as.numeric(IsTwoPointConversionSuccessful), YardLineFixed = as.numeric(YardLineFixed),
                       IsPenaltyAccepted = as.numeric(IsPenaltyAccepted), IsNoPlay = as.numeric(IsNoPlay), PenaltyYards = as.numeric(PenaltyYards))

## Turning Date Column Into column type (Date)
data_2013$GameDate <- ymd(data_2013$GameDate)

#### Character Data Quality Report #### 

## Selecting variables based on column type
data_2013_character_variables <- data_2013 %>% 
  select_if(is.character)

data_2013_numeric_variables <- data_2013 %>% 
  select_if(is.numeric)

## Calculating empty observations
data_2013_chr_observations <- rbind(
        sum(data_2013$OffenseTeam != ""),
        sum(data_2013$DefenseTeam != ""),
        sum(data_2013$X != ""),
        sum(data_2013$X.1 != ""),
        sum(data_2013$Description != ""),
        sum(data_2013$X.2 != ""),
        sum(data_2013$X.3 != ""),
        sum(data_2013$Formation != ""),
        sum(data_2013$PlayType != ""),
        sum(data_2013$PassType != ""),
        sum(data_2013$Challenger != ""),
        sum(data_2013$RushDirection != ""),
        sum(data_2013$YardLineDirection != ""),
        sum(data_2013$PenaltyTeam != ""),
        sum(data_2013$PenaltyType != ""))

## Creating vector with column names
data_2013_variables <- c(colnames(data_2013_character_variables))

## Binding no observations with column name and cleaning table
N_Chr <- cbind(data_2013_variables, data_2013_chr_observations)
N_Chr <- as.data.frame(N_Chr)
N_Chr <- transform(N_Chr, Chr_Observations = as.numeric(V2))
N_Chr <- N_Chr[,-2]

## Calcuating unique variables across all columns
Unique_Chr_Variables <- rapply(data_2013_character_variables,function(x)length(unique(x)))

## Renaming and calculating missing percentages for final table
Character_Data_Report <- N_Chr %>% 
  rename(N = Chr_Observations) %>% 
  cbind(Unique_Chr_Variables) %>% 
  mutate(Missing = (17448 - N),
         Missing_Pct = (100 * (Missing / 17448)),
         Missing_Pct = round(Missing_Pct, digits = 1),
         Unique_Pct = ((Unique_Chr_Variables / N) * 100),
         Unique_Pct = round(Unique_Pct, digits = 1)) 


## Checking to see what is in these random columns
X <- data_2013$X
X1 <- data_2013$X.1
X2 <- data_2013$X.2
X3 <- data_2013$X.3
X_Columns <- as.data.frame(cbind(X, X1, X2, X3))
X_Columns <- X_Columns[!apply(X_Columns == "", 1, all),]

#### Numeric Data Quality Report

Numeric_NA <- colSums(is.na(data_2013_numeric_variables))
Numeric_Colnames <- colnames(data_2013_numeric_variables)
Numeric_Rows <- rep(17448, 29)
N_Nur <- cbind(Numeric_Colnames, Numeric_Rows, Numeric_NA)
N_Nur <- as.data.frame(N_Nur)
N_Nur <- transform(N_Nur, Numeric_NA = as.numeric(Numeric_NA),
                   Numeric_Rows = as.numeric(Numeric_Rows))

Unique_Nur_Variables <- rapply(data_2013_numeric_variables,function(x)length(unique(x)))

Numeric_Data_Report <- N_Nur %>% 
  cbind(Unique_Nur_Variables) %>% 
  mutate(Missing_Pct = ((Numeric_NA/Numeric_Rows) * 100),
         Unique_Pct = ((Unique_Nur_Variables / Numeric_Rows) * 100),
         Missing_Pct = round(Missing_Pct, digits = 1),
         Unique_Pct = round(Unique_Pct, digits = 1)) %>% 
  dplyr::select(-Numeric_Colnames) %>% 
  rename(N = Numeric_Rows,
         Missing = Numeric_NA,
         "Unique Values" = Unique_Nur_Variables,
         "Missing Percentage" = Missing_Pct,
         "Unique Percentage" = Unique_Pct)


