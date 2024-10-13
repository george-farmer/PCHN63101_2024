library(tidyverse)
library(groundhog)
groundhog.library('performance', '2023-11-23',tolerate.R.version='4.3.2')


crime <- read_csv("https://raw.githubusercontent.com/ajstewartlang/09_glm_regression_pt1/master/data/crime_dataset.csv")

crime_tidied <- crime %>%
  separate(col = "City, State", into = c("City", "State")) %>%
  rename(House_price = index_nsa) %>%
  rename(Violent_Crimes = "Violent Crimes")

crime_filtered <- filter(crime_tidied, Population < 2000000)

crime_filtered <- filter(crime_filtered, Year == 2015)
model2 <- lm(Violent_Crimes ~ Population, data = crime_filtered)

check_model(model2, check = c("qq"))
