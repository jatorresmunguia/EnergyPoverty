###################################################### 
################## ENLIGHT PROJECT ###################
###################################################### 

## Energy security and social justice in a post-COVID-19 world ##

### Packages ###
if(!require("stringr")) install.packages("stringr")
if(!require("foreign")) install.packages("foreign")
if(!require("readODS")) install.packages("readODS")
if(!require("plyr")) install.packages("plyr")
if(!require("dplyr")) install.packages("dplyr")
if(!require("gsubfn")) install.packages("gsubfn")
if(!require("DataEditR")) install.packages("DataEditR")
if(!require("reshape")) install.packages("reshape")
if(!require("devtools")) install.packages("devtools")
install_github("DillonHammill/rhandsontable")
#if(!require("sf")) install.packages("sf")
if(!require("stats")) install.packages("stats")
if(!require("tidyr")) install.packages("tidyr")
if(!require("ggplot2")) install.packages("ggplot2")
if(!require("plotly")) install.packages("plotly")
#if(!require("rnaturalearth")) install.packages("rnaturalearth")
if(!require("tidyr")) install.packages("tidyr")
if(!require("ggspatial")) install.packages("ggspatial")
if(!require("ggthemes")) install.packages("ggthemes")
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("forcats")) install.packages("forcats")
if(!require("readxl")) install.packages("readxl")
if(!require("plm")) install.packages("plm")
if(!require("fixest")) install.packages("fixest")
if(!require("lmtest")) install.packages("lmtest")

###
library("scales")
integer_breaks <- function(n = 5, ...) {
  breaker <- pretty_breaks(n, ...)
  function(x) {
    breaks <- breaker(x)
    breaks[breaks == floor(breaks)]
  }
}

### database energy ###
energymonth2 <- read_excel("MonthlyData.xlsx")
energymonth <- energymonth2

# delete duplicated measures 
energymonth <- energymonth2
energymonth <- energymonth %>% 
               group_by(Country, Category) %>% 
               summarise_each(funs(first(.[!is.na(.)])))
energymonth <- as.data.frame(energymonth)
levels(as.factor(energymonth$Country)) # 120 countries
levels(as.factor(energymonth$Category)) # 9 measures

energymonth[, "Number of months"] <- rowSums(energymonth[, c("March 2020", "April 2020", "May 2020", "June 2020", "July 2020", 
                                                             "August 2020", "September 2020", "October 2020", "November 2020",
                                                             "December 2020", "January 2021", "February 2021", "March 2021")], 
                                             na.rm = TRUE)

for(rows in 1:nrow(energymonth)){
  if(!any(energymonth[rows, c("March 2020", "April 2020", "May 2020", "June 2020", "July 2020", 
                              "August 2020", "September 2020", "October 2020", "November 2020",
                              "December 2020", "January 2021", "February 2021", "March 2021")] == 1, na.rm = TRUE)){
    next
  } else(
    energymonth[rows, "Months until first measure"] <- min(which(energymonth[rows, c("March 2020", "April 2020", "May 2020", "June 2020", "July 2020", 
                                                                                     "August 2020", "September 2020", "October 2020", "November 2020",
                                                                                     "December 2020", "January 2021", "February 2021", "March 2021")] == 1)-1)
  )
}

energylargemonth <- melt(energymonth[, c("Country", "Category", "iso2", "iso3", "Description", 
                                         "March 2020", "April 2020", "May 2020", "June 2020", "July 2020", 
                                         "August 2020", "September 2020", "October 2020", "November 2020",
                                         "December 2020", "January 2021", "February 2021", "March 2021")], 
                         id = c("Country", "Category", "Description", "iso2", "iso3"))

colnames(energylargemonth)[colnames(energylargemonth) %in% "variable"] <- "Date"

energymeasure <- cast(energylargemonth, Country+Date+iso2+iso3~Category)

energymeasure[, "Any Measure"] <- 0

for(rows in 1:nrow(energymeasure)){
  if(any(energymeasure[rows, c("Deferred payment arrangements", "Disconnection bans", "Discounts or subsidies for energy supply",
                               "Free energy supply", "Other measures", "Personalized payment arrangements", 
                               "Reconnection of supply", "Support for off-grid energy supplies", "Tariff adjustments or freezes")] == 1, na.rm = TRUE)){
    energymeasure[rows, "Any Measure"] <- 1
  }
  energymeasure[all(is.na(energymeasure[rows, c("Deferred payment arrangements", "Disconnection bans", "Discounts or subsidies for energy supply",
                                                "Free energy supply", "Other measures", "Personalized payment arrangements", 
                                                "Reconnection of supply", "Support for off-grid energy supplies", "Tariff adjustments or freezes")])), "Any Measure"] <- NA
}

energymeasure[, "Total Measures"] <- rowSums(energymeasure[, c("Deferred payment arrangements", "Disconnection bans", "Discounts or subsidies for energy supply",
                                                               "Free energy supply", "Other measures", "Personalized payment arrangements", 
                                                               "Reconnection of supply", "Support for off-grid energy supplies", "Tariff adjustments or freezes")], na.rm = TRUE)

energymeasurelong <- energymeasure %>% 
  pivot_longer(cols         = c("Deferred payment arrangements", "Disconnection bans", "Discounts or subsidies for energy supply",
                                "Free energy supply", "Personalized payment arrangements", "Reconnection of supply", 
                                "Support for off-grid energy supplies", "Tariff adjustments or freezes", "Other measures", 
                                "Any Measure", "Total Measures"),
               names_to     = "Category",
               values_to    = "YesNo")

energymeasurelong <- as.data.frame(energymeasurelong)

energymeasurelong$Date <- gsub(energymeasurelong$Date, pattern = "[.]", replacement = " ")

save(energymeasurelong, file = "energymeasurelong.RData")

energymeasurelong[is.na(energymeasurelong$YesNo), "YesNo"] <- 0

energymeasurelong$YesNo <- as.factor(energymeasurelong$YesNo)

### Maps ###
## GIS information ##
shpsf <- sf::read_sf("GeoData/world-administrative-boundaries.shp")
energymeasurelong$Date <- as.factor(energymeasurelong$Date)
levels(energymeasurelong$Date)
energymeasurelong$Date <- factor(energymeasurelong$Date,
                                 c("March 2020", "April 2020", "May 2020", "June 2020", "July 2020", 
                                   "August 2020", "September 2020", "October 2020", "November 2020",
                                   "December 2020", "January 2021", "February 2021", "March 2021"))

energymeasuremonth <- data.frame(iso3 = rep(levels(as.factor(shpsf$iso3)), each = length(levels(energymeasurelong$Date))*length(levels(as.factor(energymeasurelong$Category)))),
                                 Date = rep(levels(energymeasurelong$Date), times = length(levels(as.factor(shpsf$iso3)))*length(levels(as.factor(energymeasurelong$Category)))),
                                 Category = rep(levels(as.factor(energymeasurelong$Category)), times = length(levels(as.factor(shpsf$iso3)))*length(levels(energymeasurelong$Date))))

energymeasuremonth <- merge(energymeasuremonth, energymeasurelong, by = c("iso3", "Category", "Date"), all.x = TRUE)
#energymeasuremonth[is.na(energymeasuremonth$YesNo), "YesNo"] <- NA
summary(energymeasuremonth$YesNo)

energymeasuremonth$Date <- as.factor(energymeasuremonth$Date)

energymeasuremonth$Date <- factor(energymeasuremonth$Date,
                                  c("March 2020", "April 2020", "May 2020", "June 2020", "July 2020", 
                                    "August 2020", "September 2020", "October 2020", "November 2020",
                                    "December 2020", "January 2021", "February 2021", "March 2021"))

energymeasuremonth <- energymeasuremonth[!is.na(energymeasuremonth$iso3), ]
energymeasuremonth$iso3 <- as.factor(energymeasuremonth$iso3)
summary(energymeasuremonth)

energymeasuremonth$Category <- as.factor(energymeasuremonth$Category)
levels(energymeasuremonth$Category) <- gsub(levels(energymeasuremonth$Category), pattern = "-", replacement = " ")

# Joining diseases data with shapefile
for(measure in levels(factor(energymeasuremonth$Category))){
  assign(paste0("shpdata", gsub(measure, pattern = " ", replacement = "_")), merge(shpsf, energymeasuremonth[energymeasuremonth$Category == measure, ], by = "iso3", all = TRUE))
  }

### Map ###
energymonth[, "Months until first measure"] <- as.character(energymonth[, "Months until first measure"])

Months2First <- energymonth %>%
  group_by(Category) %>% 
  select(Country, Category, `Months until first measure`) %>%
  filter(!is.na(`Months until first measure`)) %>%
  group_by(Category, `Months until first measure`) %>%
  summarise(Months2 = n()) 

Months2First <- Months2First %>%
  left_join(Months2First %>%
              group_by(Category) %>%
              summarise(TotCountries = sum(Months2)), 
            by = "Category") %>%
  group_by(Category, Months2) %>%
  mutate(FirstMeasure = Months2/TotCountries)

Months2First <- as.data.frame(Months2First)
Months2First$Months <- as.numeric(Months2First$`Months until first measure`)

dataMonths <- data.frame(Months = rep(0:12, times = length(unique(Months2First$Category))),
                         Category = rep(unique(Months2First$Category), each = length(0:12)))

Months2First <- join(dataMonths, Months2First, by = c("Months", "Category"), match = "first")

Months2First[is.na(Months2First$FirstMeasure), "FirstMeasure"] <- 0

Months2First <- Months2First[order(Months2First$Category, Months2First$Months), ]

Months2First$CumSumCountries <- Months2First$FirstMeasure 

for(categories in unique(Months2First$Category)){
  for(months in 1:12){
    Months2First[Months2First$Category == categories & Months2First$Months == months, "CumSumCountries"] <- Months2First[Months2First$Category == categories & Months2First$Months == months-1, "CumSumCountries"] + Months2First[Months2First$Category == categories & Months2First$Months == months, "FirstMeasure"]
  }
}

Months2First$Category <- as.factor(Months2First$Category)

levels(Months2First$Category) <- gsub(levels(Months2First$Category), pattern = "-", replacement = " ")
Months2First$Category <- factor(Months2First$Category,
                                c("Deferred payment arrangements", "Disconnection bans", "Discounts or subsidies for energy supply",
                                  "Free energy supply", "Personalized payment arrangements", "Reconnection of supply", 
                                  "Support for off grid energy supplies", "Tariff adjustments or freezes", "Other measures"))

Months2First$Months <- rep(c("March 2020", "April 2020", "May 2020", "June 2020", "July 2020", 
                             "August 2020", "September 2020", "October 2020", "November 2020",
                             "December 2020", "January 2021", "February 2021", "March 2021"), 
                           times = length(unique(Months2First$Category)))

Months2First$Months <- factor(Months2First$Months, 
                              levels = c("March 2020", "April 2020", "May 2020", "June 2020", "July 2020", 
                                         "August 2020", "September 2020", "October 2020", "November 2020",
                                         "December 2020", "January 2021", "February 2021", "March 2021"))

Fig3 <- ggplot(Months2First, 
            aes(y = CumSumCountries, x = Months)) +
  geom_step(group = 1, size = 2, colour = "#56B4E9") + 
  scale_color_colorblind() + 
  ylab("Percentage of countries") +
  xlab("Months to first time adoption of measures") + 
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~ Category, ncol = 3) +
  #xlim(0, 12) + 
  #scale_x_discrete(labels=c("0" = "March 2020", "1" = "April 2020", "2" = "May 2020", "3" = "June 2020", "4" = "July 2020", 
  #                            "5" = "August 2020", "6" = "September 2020", "7" = "October 2020", "8" = "November 2020",
  #                            "9" = "December 2020", "10" = "January 2021", "11" = "February 2021", "12" = "March 2021")) + 
  theme_bw() +
  theme(legend.position = "bottom",
        legend.text = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 90))

ggsave(Fig3, filename = "Fig3.png", dpi = 300)

energymeasuremonth2 <- energymeasuremonth %>% 
  select(Date, Category, YesNo) %>%
  filter(!is.na(YesNo)) %>%
  mutate(YesNo = as.numeric(as.character(YesNo)))

energymeasuremonth2 <- energymeasuremonth2 %>%
  group_by(Date, Category) %>%                        
      summarise_at(vars(YesNo),              
               list(name = sum)) 
  
energymeasuremonth2 <- as.data.frame(energymeasuremonth2)

levels(energymeasuremonth2$Category)

energymeasuremonth2$Category <- factor(energymeasuremonth2$Category,
                                       c("Deferred payment arrangements", "Disconnection bans", 
                                         "Discounts or subsidies for energy supply", "Free energy supply", 
                                         "Personalized payment arrangements", "Reconnection of supply",
                                         "Support for off grid energy supplies", "Tariff adjustments or freezes",
                                         "Other measures", "Any Measure", "Total Measures"))


Fig4 <- ggplot(data = energymeasuremonth2[energymeasuremonth2$Category %in% c("Deferred payment arrangements", "Disconnection bans", 
                                                                           "Discounts or subsidies for energy supply", "Free energy supply", 
                                                                           "Personalized payment arrangements", "Reconnection of supply",
                                                                           "Support for off grid energy supplies", "Tariff adjustments or freezes",
                                                                           "Other measures"), ],
            aes(x = Date, y = name)) +
  geom_bar(stat = "identity", fill = "#56B4E9") + 
  facet_wrap(~ Category, ncol = 3, scales = "free_y") +
  scale_y_continuous(breaks = integer_breaks()) +
  ylab("Total number of countries") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.text = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 90))

ggsave(Fig4, filename = "Fig4.png", dpi = 300)

shpsfort <- fortify(shpsf)

Any_Measure <- ggplot(data = shpdataAny_Measure[!is.na(shpdataAny_Measure$Date), ]) +
  geom_sf(aes(fill = YesNo), color = "black", size = 0.65) +
  scale_fill_manual(values = c("0" = "#D55E00", "1" = "#56B4E9", "Missing data" = "#999999"),
                    labels = c("No", "Yes", "Missing data"), 
                    na.value = "#999999") +
  labs(fill = "At least one measure applied:") + 
  facet_wrap(~ Date, ncol = 3) +
  theme_map() +
  theme(legend.position = "bottom",
        legend.text = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(face = "bold"))

ggsave(plot = Any_Measure, file = "Any_Measure.png", dpi = 300)

shpdataAny_Measure$YesNo <- as.numeric(as.character(shpdataAny_Measure$YesNo))

#### Model
residential <- read.csv("Mobility/changes-residential-duration-covid.csv", sep = ";")

residential$month <- substr(residential$Day, 4, 5)
residential$year <- substr(residential$Day, 7, 10)

residential$month <- as.factor(residential$month)
levels(residential$month) <- c("January", "February", "March", "April", 
                               "May", "June", "July", "August",
                               "September", "October", "November", "December")

residential$Date <- paste(residential$month, residential$year, sep = " ")

meanexp <- function(x){
  x <- x/100+1
  return(prod(x^(1/length(x)))-1)
}

residentialmob <- aggregate(data = residential, residential ~ Entity+Code+Date, FUN = meanexp)
colnames(residentialmob) <- c("Country", "iso3", "Date", "residential")

income <- read.csv("World Bank groups/CLASS.csv", sep = ";")[, 2:4]

colnames(income) <- c("iso3", "region", "incomegroup")

data2model <- join(energymeasure, residentialmob, by = c("Country", "iso3", "Date"))

data2model <- join(data2model, income, by = c("iso3"))

colnames(data2model) <- c("Country", "Date", "iso2", "iso3", "Deferred", "Disconnection", 
                          "Discounts", "Free", "Other", 
                          "Personalized", "Reconnection", "Support", 
                          "Tariff", "Any", "Total", "residential", "region", "incomegroup")

save(data2model, file = "data2model.RData")


#### 
energy_fe_mod <- plm(residential ~ Deferred + Disconnection + Discounts + Free + Personalized + Reconnection + Support + Tariff + Other + incomegroup,
                     model = "within",
                     index = c("Date", "Country"),
                     data = data2model, 
                     effect = "individual")

summary(energy_fe_mod)

# Before drawing any conclusions let's make sure whether there are any country effects in our model using plmtest().

plmtest(energy_fe_mod, effect = "individual")

# The p-value ( p-value < 2.2e-16) suggests the presence of state effects. 
# In addition to state fixed effects, a number of factors could affect the time at residence that are not specific to an individual state. 
# We can model these time fixed effects using the effect = "time" argument in plm().

energy_fe_mod2 <- plm(residential ~ Deferred + Disconnection + Discounts + Free + Personalized + Reconnection + Support + Tariff + Other + incomegroup,
                     model = "within",
                     index = c("Date", "Country"),
                     data = data2model, 
                     effect = "time")

summary(energy_fe_mod2)

plmtest(energy_fe_mod2, effect = "time")

# The p-value (p-value: < 2.22e-16) tells us that we can reject the null hypothesis so we know that 
# there are time fixed effects present in our model.

energy_fe_mod3 <- plm(residential ~ Deferred + Disconnection + Discounts + Free + Personalized + Reconnection + Support + Tariff + Other + incomegroup,
                      model = "within",
                      index = c("Date", "Country"),
                      data = data2model, 
                      effect = "twoways")

summary(energy_fe_mod3)

# serial correlation
pbgtest(energy_fe_mod3)

# The null hypothesis for the Breusch-Godfrey test is that there is no serial correlation. 
# The p-value (p-value: 0.010493) from the test tells us that we can reject the null hypothesis and confirms the presence of serial correlation in our error term.

# We can correct for serial correlation using coeftest() similar to how we corrected for heteroskedastic errors.
# We'll use the vcovHC() function for obtaining a heteroskedasticity-consistent covariance matrix, 
# but since we're interested in correcting for autocorrelation as well, we will specify method = "arellano"
# which corrects for both heteroskedasticity and autocorrelation.
coeftest(energy_fe_mod3, 
         vcov = vcovHC(energy_fe_mod3, 
                       method = "arellano", 
                       type = "HC1"))

coeftest(energy_fe_mod3, 
         vcov. = vcovHC, type = "HC1")

# Cross Sectional Dependence
pcdtest(energy_fe_mod3)

# null hypothesis is that there is no cross sectional dependence. 
# The p-value (p-value = 1.761e-08) that there is indeed cross-sectional dependence and we need to correct it.

coeftest(energy_fe_mod3, 
         vcov = vcovSCC(energy_fe_mod3, 
                       type = "HC3", 
                       cluster = "group")) 

######
colnames(data2model)

for(col in c("Deferred", "Disconnection", "Discounts", "Free", "Other", "Personalized", "Reconnection", "Support", "Tariff")){
  data2model[, col] <- as.factor(as.character(data2model[, col]))
  levels(data2model[, col]) <- c("No", "Yes")
}

dataXsum <- data2model[, c("residential", 
                           "Deferred", "Disconnection", 
                           "Discounts", "Free", "Other", 
                           "Personalized", "Reconnection", 
                           "Support", "Tariff", 
                           "incomegroup")] %>%
  select("Monthly average change in time spent at home" = residential,
         "Deferred payment arrangements" = Deferred,
         "Disconnection bans" = Disconnection,
         "Discounts or subsidies for energy supply" = Discounts,
         "Free energy supply" = Free,
         "Tariff adjustments or freezes" = Tariff,
         "Personalized payment arrangements" = Personalized,
         "Reconnection of supply" = Reconnection,
         "Support for off-grid energy supplies" = Support,
         "Other measures" = Other,
         "Income group" = incomegroup)

emptycol = function(x) " "

datasummary_balance(~1,
                    data = dataXsum,
                    fmt = 2,
                    output = 'Table1.docx') 





