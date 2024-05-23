####################################################################################################
# Project: Data Wrangling (vaccination rate vs # of days since pandemic started)                   #
# Author: Brian Y.                                                                                 #
####################################################################################################

# Load libraries
library(tidyverse)

# Set up work directory
setwd("c:/users/pcc/desktop")

# Load datasets
gdp <- read_csv("gdp.csv")
demographics <- read_csv("demographics.csv")
vaccination <- read_csv("vaccination.csv")

# Manipulate data
gdp <- gdp %>% rename("Country_Code" = "Country Code", "GDP" = "2020") %>% filter(is.na(GDP) == FALSE) %>% select(Country_Code, GDP)
demographics <- demographics %>% rename("Country_Code" = "Country Code", "Series_Code" = "Series Code") %>% filter(is.na(YR2015) == FALSE & Series_Code != "SP.POP.TOTL.MA.IN" & Series_Code != "SP.POP.TOTL.FE.IN", Series_Code != "SP.POP.TOTL") %>% separate(col = Series_Code, into = c("Series_Code", NA), sep = ".MA") %>% separate(col = Series_Code, into = c("Series_Code", NA), sep = ".FE") %>% group_by(Country_Code, Series_Code) %>% summarize(YR2015 = sum(YR2015)) %>% pivot_wider(names_from = Series_Code, values_from = YR2015) %>% na.omit()
vaccination <- vaccination %>% filter(is.na(Population) == FALSE, is.na(Province_State) == TRUE) %>% pivot_longer(cols = contains(c("2020", "2021", "2022")), names_to = "Date", values_to = "Shots", values_drop_na = TRUE) %>% filter(Shots != 0) %>% mutate(vacRate = Shots / Population) %>% group_by(iso3) %>% mutate(daysSinceStart = order(Shots)) %>% arrange(iso3, daysSinceStart) %>% select(iso3, Country_Region, vacRate, Shots, Population, daysSinceStart)

# Build main table
main_table <- vaccination %>% inner_join(gdp, by = c(iso3 = "Country_Code")) %>% inner_join(demographics, by = c(iso3 = "Country_Code"))

# Build table 1
table1 <- main_table %>% group_by(iso3) %>% summarize(mostRecentDayCount = max(daysSinceStart)) %>% inner_join(main_table) %>% group_by(iso3) %>% filter(daysSinceStart == mostRecentDayCount)

# Plot "Vaccination Rate vs # of Days Since Start"
ggplot(data = table1) + geom_point(mapping = aes(x = mostRecentDayCount, y = vacRate)) + scale_x_continuous("daysSinceStart") + labs(title = "Vaccination Rate vs # of Days Since Start")

# Build table 2
table2 <- main_table %>% mutate(POP.0014.PROP = SP.POP.0014 / Population, POP.1564.PROP = SP.POP.1564 / Population, POP.65UP.PROP = SP.POP.65UP / Population, POP.80UP.PROP = SP.POP.80UP / Population, URB.TOTL.PROP = SP.URB.TOTL / Population)

# Build models
single_var_models <- data.frame(Variable = c("Population", "daysSinceStart", "GDP", "SP.DYN.AMRT", "SP.DYN.LE00.IN", "POP.0014.PROP", "POP.1564.PROP", "POP.65UP.PROP", "POP.80UP.PROP", "URB.TOTL.PROP"), R_Squared = c(0.000521, 0.5801, 0.01203, 0.1737, 0.2091, 0.1647, 0.1315, 0.1083, 0.1076, 0.1332)) %>% arrange(R_Squared)
model1 <- lm(data = table2, vacRate ~ Population + GDP + URB.TOTL.PROP)
model2 <- lm(data = table2, vacRate ~ SP.DYN.LE00.IN + SP.DYN.AMRT)
model3 <- lm(data = table2, vacRate ~ daysSinceStart + POP.1564.PROP)
model4 <- lm(data = table2, vacRate ~ daysSinceStart + POP.65UP.PROP)
model5 <- lm(data = table2, vacRate ~ daysSinceStart + SP.DYN.LE00.IN + SP.DYN.AMRT)
multi_var_models <- data.frame(Model = c("M1", "M2", "M3", "M4", "M5"), Variables = c("Population + GDP + URB.TOTL.PROP", "SP.DYN.LE00.IN + SP.DYN.AMRT", "daysSinceStart + POP.0014.PROP", "daysSinceStart + POP.1564.PROP", "daysSinceStart + POP.65UP.PROP"), R_Squared = c(0.1377, 0.2133, 0.7033, 0.6846, 0.6533))

# Plot "Model Comparison"
ggplot(data = multi_var_models) + geom_col(mapping = aes(x = Model, y = R_Squared)) + labs(title = "Model Comparison")