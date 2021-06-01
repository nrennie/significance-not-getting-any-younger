#load packages
library(tidyverse)
library(extrafont)
library(cowplot)
library(countrycode)
library(patchwork)

#load data
world_data <- read.csv("WDIData.csv")
world_male <- filter(world_data, Indicator.Name == "Life expectancy at birth, male (years)")[,c(2,5:65)]
world_female <- filter(world_data, Indicator.Name == "Life expectancy at birth, female (years)")[,c(2,5:65)]
world_total <- filter(world_data, Indicator.Name == "Life expectancy at birth, total (years)")[,c(2,5:65)]

#define functions
test_exp <- function(y,x){
  mod <- lm(y~x)
  k <- mod$coefficients[2]
  confint(mod)
  return(paste(round(k, 2), " (", round(confint(mod)[2,1], 2), " , ", round(confint(mod)[2,2],2), ")", sep=""))
}

test_lower <- function(y,x){
  mod <- lm(y~x)
  k <- mod$coefficients[2]
  return(confint(mod)[2,1])
}

country_col <- function(country){
  if (country %in% countries){
    return("#762a83")
  } else{
    return("#a6dba0")
  }
}

################################################# UK OVERALL #######################################################

uk_total <- filter(world_total, Country.Code == "GBR")
uk_male <- filter(world_male, Country.Code == "GBR")
uk_female <- filter(world_female, Country.Code == "GBR")
plot_data <- tibble(year=1960:2020, uk_male=as.numeric(uk_male[2:62]), uk_female=as.numeric(uk_female[2:62]), uk_total=as.numeric(uk_total[2:62]))
long_data <- pivot_longer(plot_data, uk_male:uk_total, names_to="gender", values_to="life_exp")
p <- ggplot() +
  geom_line(data=long_data, mapping=aes(x=year, y=life_exp, col=gender)) +
  labs(title="Life expectancy in the United Kingdom", caption = "N. Rennie | Source: World Bank’s World Development Indicators", x="") +
  scale_colour_manual("", values=c("uk_female"="#af8dc3","uk_male"="#7fbf7b", "uk_total"="black"), labels=c("Female", "Male", "Overall")) +
  scale_y_continuous("Life expectancy (years)\n", breaks=seq(66,84,2), limits=c(66,84)) +
  scale_x_continuous("", breaks=seq(1970, 2025, 10), limits=c(1960,2025)) +
  geom_text(data=data.frame(x=2019, y=83.1, label="Female"), aes(x=x, y=y, label=label, colour="uk_female"), size=3, hjust=0, family="Times New Roman") +
  geom_text(data=data.frame(x=2019, y=79.5, label="Male"), aes(x=x, y=y, label=label, colour="uk_male"), size=3, hjust=0, family="Times New Roman") +
  geom_text(data=data.frame(x=2019, y=81.3, label="Overall"), aes(x=x, y=y, label=label, colour="uk_total"), size=3, hjust=0, family="Times New Roman") +
  coord_cartesian(expand = F) +
  theme_light() +
  theme(panel.background = element_rect(fill = "white", colour="white"),
        plot.background = element_rect(fill = "white", colour="white"),
        legend.background = element_rect(fill = "transparent"),
        plot.title = element_text(colour = "black", size=12, face="bold", hjust = 0, family="Times New Roman"),
        plot.subtitle = element_text(colour = "black", size=10, hjust = 0, family="Times New Roman"),
        plot.caption = element_text(colour = "black", size=10, hjust = 0, family="Times New Roman"),
        legend.position="none",
        legend.key = element_rect(colour = "white", fill="white"),
        plot.margin = unit(c(0.3, 0.5, 0.3, 0.3), "cm"), #top, right, bottom, left
        legend.spacing.x = unit(0.5,"cm"),
        legend.title = element_text(colour = "black", size=10, hjust = 0.5, family="Times New Roman"),
        legend.text = element_text(colour="black", size=10, hjust = 0),
        axis.title= element_text(colour = "black", size=10, family="Times New Roman"),
        axis.text=element_text(colour = "black", size=10, hjust = 0.5, family="Times New Roman"),
        axis.ticks=element_blank(),
        panel.grid.minor = element_blank()
  )
p
ggsave(p, filename = "Fig1.jpg",  bg = "transparent", height=2.5, width=5, unit="in")

################################################# UK TEST #######################################################

uk_total <- as.numeric(uk_total[2:62])
year <- 1960:2020
test_years <- 1969:2020
est_beta <- numeric(length(test_years))
upper_beta <- numeric(length(test_years))
lower_beta <- numeric(length(test_years))
for (i in 1:length(test_years)){
  x <- year[(which(year == test_years[i])-9):which(year == test_years[i])]
  y <- uk_total[(which(year == test_years[i])-9):which(year == test_years[i])]
  mod <- lm(y~x)
  est_beta[i] <- mod$coefficients[2]
  confint(mod)
  upper_beta[i] <- confint(mod)[2,2]
  lower_beta[i] <- confint(mod)[2,1]
}
plot_data <- tibble(year=1969:2020, est_beta=est_beta, upper_beta=upper_beta, lower_beta=lower_beta)
p <- ggplot() +
  geom_ribbon(data=plot_data, aes(ymin=est_beta, ymax=upper_beta, x=year), fill=alpha("#a6dba0", 0.7)) +
  geom_ribbon(data=plot_data, aes(ymin=lower_beta, ymax=est_beta, x=year), fill=alpha("#a6dba0", 0.7)) +
  geom_line(data=plot_data, mapping=aes(x=year, y=est_beta), colour="#00441b") +
  labs(title="Estimate of life expectancy trend in the United Kingdom", caption = "N. Rennie | Source: World Bank’s World Development Indicators", x="") +
  scale_y_continuous("Estimate of trend parameter", breaks=seq(0,0.35,0.05), limits=c(0,0.35)) +
  scale_x_continuous("", breaks=seq(1970, 2020, 10), limits=c(1960,2020)) +
  coord_cartesian(expand = F) +
  theme_light() +
  theme(panel.background = element_rect(fill = "white", colour="white"),
        plot.background = element_rect(fill = "white", colour="white"),
        legend.background = element_rect(fill = "white"),
        plot.title = element_text(colour = "black", size=12, face="bold", hjust = 0, family="Times New Roman"),
        plot.subtitle = element_text(colour = "black", size=10, hjust = 0, family="Times New Roman"),
        plot.caption = element_text(colour = "black", size=10, hjust = 0, family="Times New Roman"),
        legend.position="top",
        legend.key = element_rect(colour = "black", fill="black"),
        plot.margin = unit(c(0.3, 0.5, 0.3, 0.3), "cm"), #top, right, bottom, left
        legend.spacing.x = unit(0.5,"cm"),
        legend.title = element_text(colour = "black", size=10, hjust = 0.5, family="Times New Roman"),
        legend.text = element_text(colour="black", size=10, hjust = 0),
        axis.title= element_text(colour = "black", size=10, family="Times New Roman"),
        axis.text=element_text(colour = "black", size=10, hjust = 0.5, family="Times New Roman"),
        axis.ticks=element_blank(),
        panel.grid.minor = element_blank()
  )
p
ggsave(p, filename = "Fig2.jpg",  bg = "transparent", height=2.5, width=5, unit="in")

################################################# UK GENDER #######################################################

y <- as.numeric(uk_male[,53:62])
x <- 2011:2020
mod <- lm(y~x)
mod$coefficients[2]
confint(mod)
y <- as.numeric(uk_female[,53:62])
x <- 2011:2020
mod1 <- lm(y~x)
mod$coefficients[2]
confint(mod)

################################################# DEVELOPING #######################################################

least_dev <- c("NER", "CAF", "TCD", "SDN", "BDI")
least_all <- tibble(filter(world_data, Country.Code %in% least_dev & Indicator.Name %in% c("Life expectancy at birth, male (years)", "Life expectancy at birth, female (years)"))[,c(2:3,5:65)])
long_data <- pivot_longer(least_all, X1960:X2020, names_to="year", values_to="life_exp")
long_data$year <- as.numeric(substr(long_data$year, 2, 5))
long_data$Country.Code <- factor(long_data$Country.Code, levels=c("NER", "CAF", "TCD", "SDN", "BDI"), 
                                 labels=c("Niger", "CAR", "Chad", "Sudan", "Burundi"))
p1 <- ggplot(data=long_data, mapping=aes(x=year, y=life_exp, col=Indicator.Name)) +
  geom_line() +
  coord_cartesian(expand=F) +
  labs(title="Life expectancy in the five least developed countires", x="") +
  scale_colour_manual("", values=c("Life expectancy at birth, female (years)"="#af8dc3","Life expectancy at birth, male (years)"="#7fbf7b"), labels=c("Female", "Male")) +
  scale_y_continuous("Life expectancy\n(years)", breaks=seq(30,90,20), limits=c(30,90)) +
  scale_x_continuous("", breaks=seq(1970, 2020, 30)) +
  facet_grid(. ~ Country.Code) + 
  theme(plot.title = element_text(colour = "black", size=12, face="bold", hjust = 0, family="Times New Roman"),
        plot.subtitle = element_text(colour = "black", size=10, hjust = 0, family="Times New Roman"),
        plot.caption = element_text(colour = "black", size=10, hjust = 0, family="Times New Roman"),
        legend.position=c(0.09,0.92),
        strip.background =element_rect(fill="#7fbf7b"),
        strip.text = element_text(colour = 'black', size=8, family="Times New Roman"),
        axis.title= element_text(colour = "black", size=10, family="Times New Roman"),
        axis.title.x = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)),
        axis.text=element_text(colour = "black", size=10, hjust = 0.5, family="Times New Roman"),
        legend.key = element_rect(colour = "transparent", fill="transparent"),
        legend.key.height = unit(0.3, 'cm'),
        plot.margin = unit(c(0.2, 0.5, 0.2, 0.3), "cm"), #top, right, bottom, left
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_text(colour = "black", size=10, hjust = 0.5, family="Times New Roman"),
        legend.text = element_text(family="Times New Roman", colour="black", size=8, hjust = 0)
  )
p1  
most_dev <- c("NOR", "IRL", "CHE", "HKG", "ISL")
most_all <- tibble(filter(world_data, Country.Code %in% most_dev & Indicator.Name %in% c("Life expectancy at birth, male (years)", "Life expectancy at birth, female (years)"))[,c(2:3,5:65)])
long_data <- pivot_longer(most_all, X1960:X2020, names_to="year", values_to="life_exp")
long_data$year <- as.numeric(substr(long_data$year, 2, 5))
long_data$Country.Code <- factor(long_data$Country.Code, levels=c("NOR", "IRL", "CHE", "HKG", "ISL"), labels=c("Norway", "Ireland", "Switzerland", "Hong Kong", "Iceland"))
p2 <- ggplot(data=long_data, mapping=aes(x=year, y=life_exp, col=Indicator.Name)) +
  geom_line() +
  coord_cartesian(expand=F) +
  labs(title="Life expectancy in the five most developed countires", x="") +
  scale_colour_manual("", values=c("Life expectancy at birth, female (years)"="#af8dc3","Life expectancy at birth, male (years)"="#7fbf7b"), labels=c("Female", "Male")) +
  scale_y_continuous("Life expectancy\n(years)", breaks=seq(30,90,20), limits=c(30,90)) +
  scale_x_continuous("", breaks=seq(1970, 2020, 30)) +
  facet_grid(. ~ Country.Code) + 
  theme(plot.title = element_text(colour = "black", size=12, face="bold", hjust = 0, family="Times New Roman"),
        plot.subtitle = element_text(colour = "black", size=10, hjust = 0, family="Times New Roman"),
        plot.caption = element_text(colour = "black", size=10, hjust = 0, family="Times New Roman"),
        legend.position="none",
        strip.background =element_rect(fill="#7fbf7b"),
        strip.text = element_text(colour = 'black', size=8, family="Times New Roman"),
        axis.title.x = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)),
        axis.title= element_text(colour = "black", size=10, family="Times New Roman"),
        axis.text=element_text(colour = "black", size=10, hjust = 0.5, family="Times New Roman"),
        legend.key = element_rect(colour = "white", fill="white"),
        legend.key.height = unit(1, 'cm'),
        plot.margin = unit(c(0.2, 0.5, 0.2, 0.3), "cm"), #top, right, bottom, left
        legend.title = element_text(colour = "black", size=10, hjust = 0.5, family="Times New Roman"),
        legend.text = element_text(colour="black", size=8, hjust = 0)
  )
p2
p <- p1 + p2 + plot_layout(nrow = 2) +
  plot_annotation(
    caption = 'N. Rennie | Source: World Bank’s World Development Indicators') &
  theme(plot.caption = element_text(colour = "black", size=10, hjust = 0, family="Times New Roman"))
p
ggsave(p, filename = "Fig3.jpg",  bg = "transparent", height=4, width=5, unit="in")



################################################# DEVELOPING ANALYSIS ################################################

least_dev <- c("NER", "CAF", "TCD", "SDN", "BDI")
least_m <- tibble(filter(world_data, Country.Code %in% least_dev & Indicator.Name %in% c("Life expectancy at birth, male (years)"))[,c(2, 56:65)])
least_f <- tibble(filter(world_data, Country.Code %in% least_dev & Indicator.Name %in% c("Life expectancy at birth, female (years)"))[,c(2,56:65)])
test_exp(as.numeric(least_m[1,2:11]), 2011:2020)
test_exp(as.numeric(least_m[2,2:11]), 2011:2020)
test_exp(as.numeric(least_m[3,2:11]), 2011:2020)
test_exp(as.numeric(least_m[4,2:11]), 2011:2020)
test_exp(as.numeric(least_m[5,2:11]), 2011:2020)
test_exp(as.numeric(least_f[1,2:11]), 2011:2020)
test_exp(as.numeric(least_f[2,2:11]), 2011:2020)
test_exp(as.numeric(least_f[3,2:11]), 2011:2020)
test_exp(as.numeric(least_f[4,2:11]), 2011:2020)
test_exp(as.numeric(least_f[5,2:11]), 2011:2020)
most_dev <- c("NOR", "IRL", "CHE", "HKG", "ISL")
most_m <- tibble(filter(world_data, Country.Code %in% most_dev & Indicator.Name %in% c("Life expectancy at birth, male (years)"))[,c(2, 56:65)])
most_f <- tibble(filter(world_data, Country.Code %in% most_dev & Indicator.Name %in% c("Life expectancy at birth, female (years)"))[,c(2,56:65)])
test_exp(as.numeric(most_m[1,2:11]), 2011:2020)
test_exp(as.numeric(most_m[2,2:11]), 2011:2020)
test_exp(as.numeric(most_m[3,2:11]), 2011:2020)
test_exp(as.numeric(most_m[4,2:11]), 2011:2020)
test_exp(as.numeric(most_m[5,2:11]), 2011:2020)
test_exp(as.numeric(most_f[1,2:11]), 2011:2020)
test_exp(as.numeric(most_f[2,2:11]), 2011:2020)
test_exp(as.numeric(most_f[3,2:11]), 2011:2020)
test_exp(as.numeric(most_f[4,2:11]), 2011:2020)
test_exp(as.numeric(most_f[5,2:11]), 2011:2020)

################################################# WORLD #######################################################

world_recent <- tibble(world_total[, c(1, 53:62)])
d <- world_recent[rowSums(is.na(world_recent[,2:11])) != ncol(world_recent[,2:11]), ]
k <- apply(d[,2:11], 1, function(y) test_lower(as.numeric(y), x=2011:2020))
neg_growth <- filter(world_recent, Country.Code %in% unlist(d[which(k < 0),1]))
ages <- as.numeric(apply(neg_growth, 1, function(x) tail(x[2:11][!is.na(x[2:11])], n=1)))[2:12]
countries <- countrycode(unlist(neg_growth[,1]), origin = 'wb', destination = 'country.name')[2:12]
countries[2] <- "Curacao"
countries[10] <- "USA"
world <- map_data("world")
world_col <- sapply(world$region, function(x) country_col(x))
world$world_col <- world_col
world <- filter(world, region != "Antarctica")
p <- ggplot() + 
  geom_polygon(data=world, aes(x=long, y=lat, group=group, fill=I(world_col)), color = "black", size=0.3) + 
  labs(title="Countries where life expectancy growth has stalled", caption = "N. Rennie | Source: World Bank’s World Development Indicators", x="") +
  theme_light() +
  theme(panel.background = element_rect(fill = "white", colour="white"),
        plot.background = element_rect(fill = "white", colour="white"),
        legend.background = element_rect(fill = "white"),
        plot.title = element_text(colour = "black", size=12, face="bold", hjust = 0, family="Times New Roman"),
        plot.subtitle = element_text(colour = "black", size=10, hjust = 0, family="Times New Roman"),
        plot.caption = element_text(colour = "black", size=10, hjust = 0, family="Times New Roman"),
        legend.position="top",
        legend.key = element_rect(colour = "black", fill="black"),
        plot.margin = unit(c(0.3, 0.5, 0.3, 0.3), "cm"), #top, right, bottom, left
        legend.spacing.x = unit(0.5,"cm"),
        legend.title = element_text(colour = "black", size=10, hjust = 0.5, family="Times New Roman"),
        legend.text = element_text(colour="black", size=10, hjust = 0),
        axis.title= element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  )
p
ggsave(p, filename = "Fig4.jpg",  bg = "transparent", height=2.5, width=4.5, unit="in")
