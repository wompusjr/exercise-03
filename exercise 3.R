library(tidyverse)
library(ggplot2)
library(dplyr)
f <- file.choose()
d <- read_csv(f, col_names = TRUE)
d
attach(d)
d$BSD <- (Body_mass_male_mean/Body_mass_female_mean)
d$sex_ratio <- (AdultMales/AdultFemale)
r <- sqrt(HomeRange_km2)/pi
d$diameter <- r*2
d$DI <- (DayLength_km/(diameter))
##plotting
p <- ggplot(data = d, aes(x = log(Move), y = log(DayLength_km),
                          color = factor(Family)))
p <- p + xlab("log(Move)") + ylab("log(DayLength_km)")
p <- p + geom_point(na.rm = TRUE)
p <- p + theme(legend.position = "bottom", legend.title = element_blank())
p <- p + facet_wrap(~Family, ncol = 4)
p <- p + theme(legend.position = "none")
p
p2 <- ggplot(data = d, aes(x = log(MeanGroupSize), y = log(DayLength_km),
                          color = factor(Family)))
p2 <- p2 + xlab("log(Avg. Group Size)") + ylab("log(Day Range Length)")
p2 <- p2 + geom_point(na.rm = TRUE)
p2 <- p2 + theme(legend.position = "bottom", legend.title = element_blank())
p2 <- p2 + facet_wrap(~Family, ncol = 4)
p2 <- p2 + theme(legend.position = "none")
p2
p3 <- ggplot(data = d, aes(x = log(BSD), y = log(Canine_Dimorphism),
                           color = factor(Family)))
p3 <- p3 + xlab("log of Body Sex Dimorphism") + ylab("log or Canine Dimorphism")
p3 <- p3 + geom_point(na.rm = TRUE)
p3 <- p3 + theme(legend.position = "bottom", legend.title = element_blank())
p3 <- p3 + facet_wrap(~Family, ncol = 4)
p3 <- p3 + theme(legend.position = "none")
p3
##other stuff
d <- mutate(d, diet_strategy = ifelse(Fruit >= 50, "frugivore", ifelse(Leaves >= 50, "folivore",
                                                                   ifelse(Fruit < 50 & Leaves < 50, "omnivore", NA))))
dnoNA <- d |> drop_na(diet_strategy)
#plotting
p4 <- ggplot(data = dnoNA, aes(x = diet_strategy, y = log(MeanGroupSize))) #loging for convenience
p4 <- p4 + geom_boxplot(na.rm = TRUE)
p4 <- p4 + theme(axis.text.x = element_text(angle = 90))
p4 <- p4 + ylab("avg. group size")
p4
##piping code
mutate(d, Binomial = paste(Genus, Species, sep = " ")) %>%
  select(Binomial, Family, Brain_Size_Species_Mean, Body_mass_male_mean) %>%
  group_by(Family) %>%
  summarize(avgbrain = mean(Brain_Size_Species_Mean, na.rm = TRUE), avgbody = mean(Body_mass_male_mean,
                                                                          na.rm = TRUE)) %>%
  arrange(avgbrain)%>%
  print()
