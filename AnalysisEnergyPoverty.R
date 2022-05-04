###################################################### 
################## ENLIGHT PROJECT ###################
###################################################### 

## Energy security and social justice in a post-COVID-19 world ##

### Working directory ###
# setwd(choose.dir())
setwd("Z:/Energy ENLIGHT")

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
  select(Country, Category, "Months until first measure") %>%
  filter(!is.na(`Months until first measure`)) %>%
  rename(Months = 3) %>%
  group_by(Category, Months) %>%
  summarise(Months2 = length(Months)) 

Months2First <- Months2First %>%
  left_join(Months2First %>%
              group_by(Category) %>%
              summarise(TotCountries = sum(Months2)), 
            by = "Category") %>%
  group_by(Category, Months) %>%
  summarise(FirstMeasure = Months2/TotCountries)

Months2First <- as.data.frame(Months2First)
Months2First$Months <- as.numeric(Months2First$Months)

dataMonths <- data.frame(Months = rep(0:13, times = length(unique(Months2First$Category))),
                         Category = rep(unique(Months2First$Category), each = length(0:13)))

Months2First <- join(dataMonths, Months2First, by = c("Months", "Category"))

Months2First[is.na(Months2First$FirstMeasure), "FirstMeasure"] <- 0

Months2First <- Months2First[order(Months2First$Category, Months2First$Months), ]

Months2First$CumSumCountries <- Months2First$FirstMeasure 

for(categories in unique(Months2First$Category)){
  for(months in 1:13){
    Months2First[Months2First$Category == categories & Months2First$Months == months, "CumSumCountries"] <- Months2First[Months2First$Category == categories & Months2First$Months == months-1, "CumSumCountries"] + Months2First[Months2First$Category == categories & Months2First$Months == months, "FirstMeasure"]
  }
}

Months2First$Category <- as.factor(Months2First$Category)

levels(Months2First$Category) <- gsub(levels(Months2First$Category), pattern = "-", replacement = " ")
Months2First$Category <- factor(Months2First$Category,
                                c("Deferred payment arrangements", "Disconnection bans", "Discounts or subsidies for energy supply",
                                  "Free energy supply", "Personalized payment arrangements", "Reconnection of supply", 
                                  "Support for off grid energy supplies", "Tariff adjustments or freezes", "Other measures"))

ggplot(Months2First, 
            aes(y = CumSumCountries, x = Months)) +
  geom_step(size = 2, colour = "#56B4E9") + 
  scale_color_colorblind() + 
  ylab("Percentage of countries") +
  xlab("Months to first time adoption of measures") + 
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~ Category, ncol = 3) +
  xlim(0, 13) + 
  theme_bw() +
  theme(legend.position = "bottom",
        legend.text = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(face = "bold"))


energymeasuremonth2 <- energymeasuremonth %>% 
  group_by(Category) %>% 
  select(Date, Category, YesNo) %>%
  filter(!is.na(YesNo)) %>%
  group_by(Category, Date) %>%
  summarise(Countries = sum(YesNo)) 

energymeasuremonth2 <- as.data.frame(energymeasuremonth2)

levels(energymeasuremonth2$Category)

energymeasuremonth2$Category <- factor(energymeasuremonth2$Category,
                                       c("Deferred payment arrangements", "Disconnection bans", 
                                         "Discounts or subsidies for energy supply", "Free energy supply", 
                                         "Personalized payment arrangements", "Reconnection of supply",
                                         "Support for off grid energy supplies", "Tariff adjustments or freezes",
                                         "Other measures", "Any Measure", "Total Measures"))

ggplot(data = energymeasuremonth2[energymeasuremonth2$Category %in% c("Deferred payment arrangements", "Disconnection bans", 
                                                                      "Discounts or subsidies for energy supply", "Free energy supply", 
                                                                      "Personalized payment arrangements", "Reconnection of supply",
                                                                      "Support for off grid energy supplies", "Tariff adjustments or freezes",
                                                                      "Other measures"), ],
       aes(x = Date, y = Countries)) +
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

ggsave(plot = Any_Measure, file = "Any_Measure.png", width = 25, height = 20, units = "cm")

shpdataAny_Measure$YesNo <- as.numeric(as.character(shpdataAny_Measure$YesNo))












Any_MeasureF <- ggplot(data = shpdataAny_Measure[!is.na(shpdataAny_Measure$YesNo) & shpdataAny_Measure$YesNo == "1", ],
                       aes(x = Date)) +
                       geom_bar(aes(y = (..count..)/sum(..count..))) + 
                       scale_y_continuous(labels = scales::percent) +
                       ylab("Relative frequencies") +
                       theme_minimal() +
                       theme(axis.text.x = element_text(angle = 90))

ggsave(plot = Any_MeasureF, file = "Any_MeasureF.png", width = 15, height = 10, units = "cm")

Deferred_payment_arrangements <- ggplot(data = shpdataDeferred_payment_arrangements[!is.na(shpdataDeferred_payment_arrangements$YesNo),]) +
  geom_sf(data = shpsfort, fill = "white", color = "grey") + 
  geom_sf(aes(fill = YesNo), color = "black", size = 0.5) +
  scale_fill_brewer(labels = c("Yes", "No"), direction = -1) +
  labs(fill = "Deferred payment \n arrangements") + 
  facet_wrap(~ Date) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

ggsave(plot = Deferred_payment_arrangements, file = "Deferred_payment_arrangements.png", width = 25, height = 20, units = "cm")

shpdataDeferred_payment_arrangements$YesNo <- as.numeric(as.character(shpdataDeferred_payment_arrangements$YesNo))

Deferred_payment_arrangementsF <- ggplot(data = shpdataDeferred_payment_arrangements[!is.na(shpdataDeferred_payment_arrangements$YesNo) & shpdataDeferred_payment_arrangements$YesNo == "1", ],
                                         aes(x = Date)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = scales::percent) +
  ylab("Relative frequencies") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

ggsave(plot = Deferred_payment_arrangementsF, file = "Deferred_payment_arrangementsF.png", width = 15, height = 10, units = "cm")

Disconnection_bans <- ggplot(data = shpdataDisconnection_bans[!is.na(shpdataDisconnection_bans$YesNo),]) +
  geom_sf(data = shpsfort, fill = "white", color = "grey") + 
  geom_sf(aes(fill = YesNo), color = "black", size = 0.5) +
  scale_fill_brewer(labels = c("Yes", "No"), direction = -1) +
  labs(fill = "Disconnection bans") + 
  facet_wrap(~ Date) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

ggsave(plot = Disconnection_bans, file = "Disconnection_bans.png", width = 25, height = 20, units = "cm")

shpdataDisconnection_bans$YesNo <- as.numeric(as.character(shpdataDisconnection_bans$YesNo))

Disconnection_bansF <- ggplot(data = shpdataDisconnection_bans[!is.na(shpdataDisconnection_bans$YesNo) & shpdataDisconnection_bans$YesNo == "1", ],
                              aes(x = Date)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = scales::percent) +
  ylab("Relative frequencies") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

ggsave(plot = Disconnection_bansF, file = "Disconnection_bansF.png", width = 15, height = 10, units = "cm")

Discounts_or_subsidies_for_energy_supply <- ggplot(data = shpdataDiscounts_or_subsidies_for_energy_supply[!is.na(shpdataDiscounts_or_subsidies_for_energy_supply$YesNo),]) +
  geom_sf(data = shpsfort, fill = "white", color = "grey") + 
  geom_sf(aes(fill = YesNo), color = "black", size = 0.5) +
  scale_fill_brewer(labels = c("Yes", "No"), direction = -1) +
  labs(fill = "Discounts or subsidies \n for energy supply") + 
  facet_wrap(~ Date) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

ggsave(plot = Discounts_or_subsidies_for_energy_supply, file = "Discounts_or_subsidies_for_energy_supply.png", width = 25, height = 20, units = "cm")

shpdataDiscounts_or_subsidies_for_energy_supply$YesNo <- as.numeric(as.character(shpdataDiscounts_or_subsidies_for_energy_supply$YesNo))

Discounts_or_subsidies_for_energy_supplyF <- ggplot(data = shpdataDiscounts_or_subsidies_for_energy_supply[!is.na(shpdataDiscounts_or_subsidies_for_energy_supply$YesNo) & shpdataDiscounts_or_subsidies_for_energy_supply$YesNo == "1", ],
                                                    aes(x = Date)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = scales::percent) +
  ylab("Relative frequencies") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

ggsave(plot = Discounts_or_subsidies_for_energy_supplyF, file = "Discounts_or_subsidies_for_energy_supplyF.png", width = 15, height = 10, units = "cm")

Free_energy_supply <- ggplot(data = shpdataFree_energy_supply[!is.na(shpdataFree_energy_supply$YesNo),]) +
  geom_sf(data = shpsfort, fill = "white", color = "grey") + 
  geom_sf(aes(fill = YesNo), color = "black", size = 0.5) +
  scale_fill_brewer(labels = c("Yes", "No"), direction = -1) +
  labs(fill = "Free energy supply") + 
  facet_wrap(~ Date) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

ggsave(plot = Free_energy_supply, file = "Free_energy_supply.png", width = 25, height = 20, units = "cm")

shpdataFree_energy_supply$YesNo <- as.numeric(as.character(shpdataFree_energy_supply$YesNo))

Free_energy_supplyF <- ggplot(data = shpdataFree_energy_supply[!is.na(shpdataFree_energy_supply$YesNo) & shpdataFree_energy_supply$YesNo == "1", ],
                              aes(x = Date)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = scales::percent) +
  ylab("Relative frequencies") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

ggsave(plot = Free_energy_supplyF, file = "Free_energy_supplyF.png", width = 15, height = 10, units = "cm")

Other_measures <- ggplot(data = shpdataOther_measures[!is.na(shpdataOther_measures$YesNo),]) +
  geom_sf(data = shpsfort, fill = "white", color = "grey") + 
  geom_sf(aes(fill = YesNo), color = "black", size = 0.5) +
  scale_fill_brewer(labels = c("Yes", "No"), direction = -1) +
  labs(fill = "Other measures") + 
  facet_wrap(~ Date) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

ggsave(plot = Other_measures, file = "Other_measures.png", width = 25, height = 20, units = "cm")

shpdataOther_measures$YesNo <- as.numeric(as.character(shpdataOther_measures$YesNo))

Other_measuresF <- ggplot(data = shpdataOther_measures[!is.na(shpdataOther_measures$YesNo) & shpdataOther_measures$YesNo == "1", ],
                          aes(x = Date)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = scales::percent) +
  ylab("Relative frequencies") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

ggsave(plot = Other_measuresF, file = "Other_measuresF.png", width = 15, height = 10, units = "cm")

Personalized_payment_arrangements <- ggplot(data = shpdataPersonalized_payment_arrangements[!is.na(shpdataPersonalized_payment_arrangements$YesNo),]) +
  geom_sf(data = shpsfort, fill = "white", color = "grey") + 
  geom_sf(aes(fill = YesNo), color = "black", size = 0.5) +
  scale_fill_brewer(labels = c("Yes", "No"), direction = -1) +
  labs(fill = "Personalized payment \n arrangements") + 
  facet_wrap(~ Date) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

ggsave(plot = Personalized_payment_arrangements, file = "Personalized_payment_arrangements.png", width = 25, height = 20, units = "cm")

shpdataPersonalized_payment_arrangements$YesNo <- as.numeric(as.character(shpdataPersonalized_payment_arrangements$YesNo))

Personalized_payment_arrangementsF <- ggplot(data = shpdataPersonalized_payment_arrangements[!is.na(shpdataPersonalized_payment_arrangements$YesNo) & shpdataPersonalized_payment_arrangements$YesNo == "1", ],
                                             aes(x = Date)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = scales::percent) +
  ylab("Relative frequencies") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

ggsave(plot = Personalized_payment_arrangementsF, file = "Personalized_payment_arrangementsF.png", width = 15, height = 10, units = "cm")


Reconnection_of_supply <- ggplot(data = shpdataReconnection_of_supply[!is.na(shpdataReconnection_of_supply$YesNo),]) +
  geom_sf(data = shpsfort, fill = "white", color = "grey") + 
  geom_sf(aes(fill = YesNo), color = "black", size = 0.5) +
  scale_fill_brewer(labels = c("Yes", "No"), direction = -1) +
  labs(fill = "Reconnection of supply") + 
  facet_wrap(~ Date) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

ggsave(plot = Reconnection_of_supply, file = "Reconnection_of_supply.png", width = 25, height = 20, units = "cm")

shpdataReconnection_of_supply$YesNo <- as.numeric(as.character(shpdataReconnection_of_supply$YesNo))

Reconnection_of_supplyF <- ggplot(data = shpdataReconnection_of_supply[!is.na(shpdataReconnection_of_supply$YesNo) & shpdataReconnection_of_supply$YesNo == "1", ],
                                  aes(x = Date)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = scales::percent) +
  ylab("Relative frequencies") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

ggsave(plot = Reconnection_of_supplyF, file = "Reconnection_of_supplyF.png", width = 15, height = 10, units = "cm")

Support_for_off_grid_energy_supplies <- ggplot(data = shpdataSupport_for_off_grid_energy_supplies[!is.na(shpdataSupport_for_off_grid_energy_supplies$YesNo),]) +
  geom_sf(data = shpsfort, fill = "white", color = "grey") + 
  geom_sf(aes(fill = YesNo), color = "black", size = 0.5) +
  scale_fill_brewer(labels = c("Yes", "No"), direction = -1) +
  labs(fill = "Support for \n off grid energy n\ supplies") + 
  facet_wrap(~ Date) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

ggsave(plot = Support_for_off_grid_energy_supplies, file = "Support_for_off_grid_energy_supplies.png", width = 25, height = 20, units = "cm")

shpdataSupport_for_off_grid_energy_supplies$YesNo <- as.numeric(as.character(shpdataSupport_for_off_grid_energy_supplies$YesNo))

Support_for_off_grid_energy_suppliesF <- ggplot(data = shpdataSupport_for_off_grid_energy_supplies[!is.na(shpdataSupport_for_off_grid_energy_supplies$YesNo) & shpdataSupport_for_off_grid_energy_supplies$YesNo == "1", ],
                                                aes(x = Date)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = scales::percent) +
  ylab("Relative frequencies") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

ggsave(plot = Support_for_off_grid_energy_suppliesF, file = "Support_for_off_grid_energy_suppliesF.png", width = 15, height = 10, units = "cm")

Tariff_adjustments_or_freezes <- ggplot(data = shpdataTariff_adjustments_or_freezes[!is.na(shpdataTariff_adjustments_or_freezes$YesNo),]) +
  geom_sf(data = shpsfort, fill = "white", color = "grey") + 
  geom_sf(aes(fill = YesNo), color = "black", size = 0.5) +
  scale_fill_brewer(labels = c("Yes", "No"), direction = -1) +
  labs(fill = "Tariff adjustments \n or freezes") + 
  facet_wrap(~ Date) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

ggsave(plot = Tariff_adjustments_or_freezes, file = "Tariff_adjustments_or_freezes.png", width = 25, height = 20, units = "cm")

shpdataTariff_adjustments_or_freezes$YesNo <- as.numeric(as.character(shpdataTariff_adjustments_or_freezes$YesNo))

Tariff_adjustments_or_freezesF <- ggplot(data = shpdataTariff_adjustments_or_freezes[!is.na(shpdataTariff_adjustments_or_freezes$YesNo) & shpdataTariff_adjustments_or_freezes$YesNo == "1", ],
                                         aes(x = Date)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = scales::percent) +
  ylab("Relative frequencies") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

ggsave(plot = Tariff_adjustments_or_freezesF, file = "Tariff_adjustments_or_freezesF.png", width = 15, height = 10, units = "cm")

Total_Measures <- ggplot(data = shpdataTotal_Measures[!is.na(shpdataTotal_Measures$YesNo),]) +
  geom_sf(data = shpsfort, fill = "white", color = "grey") + 
  geom_sf(aes(fill = YesNo), color = "black", size = 0.5) +
  scale_colour_brewer("Diamond\nclarity") +
  labs(fill = "Total Measures") + 
  facet_wrap(~ Date) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

ggsave(plot = Total_Measures, file = "Total_Measures.png", width = 25, height = 20, units = "cm")

shpdataTotal_Measures$YesNo <- as.numeric(as.character(shpdataTotal_Measures$YesNo))

Total_MeasuresF <- ggplot(data = shpdataTotal_Measures[!is.na(shpdataTotal_Measures$YesNo) & shpdataTotal_Measures$YesNo == "1", ],
                          aes(x = Date)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = scales::percent) +
  ylab("Relative frequencies") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

ggsave(plot = Total_MeasuresF, file = "Total_MeasuresF.png", width = 15, height = 10, units = "cm")

### Other data ### 
## Coronavirus ##
coronavirus <- read.csv("Coronavirus/WHO-COVID-19-global-data.csv")
coronavirus$Date <- substr(coronavirus$?..Date_reported, 4, 10)

New_cases <- aggregate(data = coronavirus[, c("Country_code", "Date", "New_cases")], 
                       New_cases~Country_code+Date, 
                       FUN = sum)
New_deaths <- aggregate(data = coronavirus[, c("Country_code", "Date", "New_deaths")], 
                        New_deaths~Country_code+Date, 
                        FUN = sum)

coronavirus <- merge(coronavirus, merge(New_cases, New_deaths, by = c("Country_code", "Date")),
                     by = c("Country_code", "Date"), all.y = TRUE)

coronavirus <- coronavirus[coronavirus$Country_code != " ", ]
coronavirus <- coronavirus[, c(1:2, 4:5, 10:11)]
coronavirus <- unique(coronavirus)
coronavirus$Month <- substr(coronavirus$Date, 1, 2)
coronavirus$Year <- substr(coronavirus$Date, 4, 7)
coronavirus$Month <- as.factor(coronavirus$Month)
levels(coronavirus$Month) <- c("January", "February", "March", "April", 
                               "May", "June", "July", "August",
                               "September", "October", "November", "December")
coronavirus$Date <- paste(coronavirus$Month, coronavirus$Year, " ")

### Freedom in the World ###
freedom <- read.csv("Freedom in the World/All_data_FIW_2013-2021.csv")
# only Edition 2021 referring to year 2020
freedom <- freedom[freedom$Edition == 2021, ]
freedom$?..Country.Territory[!freedom$?..Country.Territory %in% coronavirus$Country] <- c("Abkhazia", "Bolivia (Plurinational State of)", "Brunei Darussalam", 
                                                                                          "Democratic Republic of the Congo", "Congo", 
                                                                                          "C?te d'Ivoire", "Crimea", "Czechia", 
                                                                                          "Eastern Donbas", "Gaza Strip", "Hong Kong", "Indian Kashmir",               
                                                                                          "Iran (Islamic Republic of)", "Kosovo[1]", "Lao People's Democratic Republic", 
                                                                                          "Micronesia (Federated States of)", "Republic of Moldova", 
                                                                                          "Nagorno-Karabakh", "Namibia", "Democratic People's Republic of Korea", "Northern Cyprus",              
                                                                                          "Pakistani Kashmir", "Russian Federation", "Somaliland",
                                                                                          "Republic of Korea", "South Ossetia", 
                                                                                          "Saint Kitts and Nevis", "Saint Lucia", 
                                                                                          "Saint Vincent and the Grenadines", "Syrian Arab Republic", 
                                                                                          "Taiwan", "United Republic of Tanzania", "Gambia", "Tibet", 
                                                                                          "Transnistria", "The United Kingdom",                
                                                                                          "United States of America", "Venezuela (Bolivarian Republic of)",
                                                                                          "Vietnam", "West Bank", "Western Sahara")

### Mobility ###
mobility <- read.csv("Mobility/Global_Mobility_Report.csv")

for(cols in 10:15){
  mobility[, paste0(colnames(mobility)[cols], "rate")] <- (mobility[, cols]/100)+1
}

mobility$Month <- substr(mobility$date, 6, 7)
mobility$Month <- as.factor(mobility$Month)
levels(mobility$Month) <- c("January", "February", "March", "April", 
                            "May", "June", "July", "August",
                            "September", "October", "November", "December")
mobility$Year <- substr(mobility$date, 1, 4)
mobility$Date <- paste(mobility$Month, mobility$Year, sep = " ")

meanexp <- function(x){
  return(prod(x^(1/length(x)))-1)
}

for(activity in 16:21){
  formula2calc <- as.formula(paste0(colnames(mobility)[activity], "~", paste("country_region", "Date", sep = " + ")))
  data2func <- na.omit(mobility[, c("country_region", "Date", colnames(mobility)[activity])])
  assign(colnames(mobility)[activity], 
         aggregate(data = data2func,
                   formula2calc, 
                   FUN = meanexp))
}

save.image(file = "Image1802")


