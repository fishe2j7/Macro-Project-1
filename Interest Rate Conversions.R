library(dplyr)
library(readr)
library(lubridate)

#Converting interest rate data to yearly averages

#German Central Bank Interests Rates pre-1999 (Before the Creation of ECB)

GERITRRATESPRE1999 = read_csv("C:/Users/fishe/OneDrive/Desktop/MACRO THEORY/Interest Rate Data/GERITRRATESPRE1999.csv")

GERITRRATESPRE1999 = GERITRRATESPRE1999 %>%
  mutate(Year = substr(YearMonth, 1, 4))

yearly_GERITRRATESPRE1999 = GERITRRATESPRE1999 %>%
  group_by(Year) %>%
  summarise(AverageItrRate = mean(ItrRate, na.rm = TRUE))

write.csv(yearly_GERITRRATESPRE1999, "C:/Users/fishe/OneDrive/Desktop/MACRO THEORY/Interest Rate Data/yearly_GERITRRATESPRE1999.csv")

#German Interest Rates post-1999 (ECB rates)

GERITRRATESPOST1999 = read.csv("C:/Users/fishe/OneDrive/Desktop/MACRO THEORY/Interest Rate Data/GERITRRATESPOSTJAN11999.csv")

GERITRRATESPOST1999 = GERITRRATESPOST1999 %>%
  mutate(YearMonth = dmy(YearMonth))

GERITRRATESPOST1999 = GERITRRATESPOST1999 %>% 
  mutate(Year = year(YearMonth))

yearly_GERITRRATESPOST1999 = GERITRRATESPOST1999 %>%
  group_by(Year) %>%
  summarise(AverageItrRate = mean(ItrRate, na.rm = TRUE))

write.csv(yearly_GERITRRATESPOST1999, "C:/Users/fishe/OneDrive/Desktop/MACRO THEORY/Interest Rate Data/yearly_GERITRRATESPOST1999.csv")

#Japanese Interest Rates

JAPInterestRates = read_csv("C:/Users/fishe/OneDrive/Desktop/MACRO THEORY/Interest Rate Data/Japan Interest Rates.csv")

JAPInterestRates = JAPInterestRates %>%
  mutate(Year = substr(`Series code`, 1, 4))

yearly_JAPInterestRates = JAPInterestRates %>%
  group_by(Year) %>%
  summarise(AverageItrRate = mean(`IR01'MADR1M`, na.rm = TRUE))

write.csv(yearly_JAPInterestRates, "C:/Users/fishe/OneDrive/Desktop/MACRO THEORY/Interest Rate Data/yearly_JAPInterestRates.csv")

#US FED FUNDS Rate

USInterestRates = read_csv("C:/Users/fishe/OneDrive/Desktop/MACRO THEORY/Interest Rate Data/FEDFUNDS.csv")

USInterestRates = USInterestRates %>%
  mutate(Year = substr(observation_date,1,4))

yearly_USInterestRates = USInterestRates %>%
  group_by(Year) %>%
  summarise(AverageItrRate = mean(FEDFUNDS, na.rm = TRUE))

write.csv(yearly_USInterestRates, "C:/Users/fishe/OneDrive/Desktop/MACRO THEORY/Interest Rate Data/yearly_USInterestRates.csv")
