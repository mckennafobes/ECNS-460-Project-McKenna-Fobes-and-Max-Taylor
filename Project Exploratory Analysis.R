rm(list=ls())
library(tidyverse)
library(dplyr)
library(lubridate)
library(reshape2)

# load in data
bank_failures = read_csv("Bank_Failures_1948-2023.csv")
indicators = read_csv("Monthly_Indicators.csv")

# removing dates with no data
bank_failures = bank_failures|>
  mutate(FAILDATE = as.Date(FAILDATE, format = "%m/%d/%Y")) |>
  arrange(FAILDATE)
bank_failures = bank_failures[-c(1:20),]
indicators = indicators[-c(1:78),]

indicators_years = indicators # for a graph below
bank_failures_years = bank_failures # for a graph below

# get dates into month/year format so they can be merged
bank_failures$DATE <- format(bank_failures$FAILDATE, "%m/%Y")
bank_failures_years$DATE <- format(bank_failures_years$FAILDATE, "%Y") # for a graph below

indicators$DATE = as.Date(indicators$DATE, format = "%m/%d/%Y")
indicators_years$DATE = as.Date(indicators_years$DATE, format = "%m/%d/%Y") # for a graph below
indicators$DATE = format(indicators$DATE, "%m/%Y")
indicators_years$DATE = format(indicators_years$DATE, "%Y")# for a graph below

# merge data sets
data = bank_failures|>
  left_join(indicators, by = 'DATE')

# Get rid of rows that are unneeded
banks_indicators = data[-c(1,5,6)]

# rename all columns for understanding
colnames(banks_indicators) =  c("Bank Class", "Location", "Estimated Loss", "ID", 
                             "Bank Name", "Total Asset", "Resolution Type", 
                             "Transaction Type", "Insurance Fund", "Date", 
                             "Unemployment Rate", "CPI Percent Change", 
                             "Interest Rate", "Federal Funds Rate")

# converting the numbers that are in character to numbers
banks_indicators[,c(3,4,6,11:14)] = apply(banks_indicators[,c(3,4,6,11:14)], 2, as.numeric)

# splitting location variable in to city, state
banks_indicators = extract(banks_indicators, Location, c("City", "State"), 
        regex = "(.+) (\\w+)")

# remove commas from city variable and make final data set
banks_indicators_final = banks_indicators|>
  mutate(City = str_replace_all(banks_indicators$City, ",", ""))

# EXPLORATORY ##################################################################
# count of banks per state
state_counts = banks_indicators_final |>
  group_by(State)|>
  count()|>
  arrange(n)
state_counts$State = factor(state_counts$State, levels = state_counts$State)

# cleveland dot plot of bank failures per state
state_counts |>
  ggplot(aes(n,State)) +
    geom_point(size=2) +
    scale_x_log10() +
    theme(text = element_text(size=5.5))+
    labs(y= "State", x = "Number of Banks Failed (Log Scale)")+
    ggtitle("Bank Failures Per State")

# count of banks per year
yrs_n = bank_failures_years|>
  group_by(DATE)|>
  count()

# line graph of bank failures over time
yrs_n|>
  ggplot(aes(DATE, log(n), group=1)) +
  geom_line()+ 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_discrete(breaks=seq(min(yrs_n$DATE), max(yrs_n$DATE), 5))+
  labs(y= "Number of Banks Failed (Log Scale)", x = "Year")+
  ggtitle("Bank Failures Over Time")

# Just so much nonsense to get the variables ready
monthly_tot = banks_indicators_final |>
  group_by(Date)|>
  count()
monthly_tot = monthly_tot|>
  mutate(DATE = Date)
monthly_tot = monthly_tot[-1]

monthly_tot = monthly_tot |> arrange(my(monthly_tot$DATE))
month_indic = filter(indicators, indicators$DATE %in% monthly_tot$DATE)
both = monthly_tot|>
  left_join(month_indic, by = 'DATE')
both[,c(1,3:6)] = apply(both[,c(1,3:6)], 2, as.numeric)
both = both[-c(418,419),]
colnames(both) = c("Bank Failures", "Date", "unemployment Rate", "Percent Change CPI", "Interest Rate", "Federal Funds Rate")
corro = round(cor(both[c(1,3:6)]),2)
melted_corro = melt(corro)

ggplot(data = melted_corro, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  geom_text(aes(Var2, Var1, label = value), color = "White", size = 2)+
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle("Correlation Heatmap Between Variables")