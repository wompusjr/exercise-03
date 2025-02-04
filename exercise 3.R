library(tidyverse)
library(ggplot2)
f <- file.choose()
d <- read_csv(f, col_names = TRUE)
d
attach(d)
d$BSD <- (Body_mass_male_mean/Body_mass_female_mean)
d$sex_ratio <- (AdultMales/AdultFemale)
r <- sqrt(HomeRange_km2)/pi
d$diameter <- r*2
d$DI <- (DayLength_km/(diameter))
