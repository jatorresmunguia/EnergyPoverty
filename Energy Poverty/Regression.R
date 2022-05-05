########################################################################################
############# Energy security and social justice in a post-Covid-19 world ############# 
########################################################################################

### Packages ###
if(!require("stringr")) install.packages("stringr")
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("stringr")) install.packages("stringr")
if(!require("rvest")) install.packages("rvest")
if(!require("renv")) install.packages("renv")
if(!require("clock")) install.packages("clock")
if(!require("lubridate")) install.packages("lubridate")
if(!require("plm")) install.packages("plm")
if(!require("plyr")) install.packages("plyr")
if(!require("lmtest")) install.packages("lmtest")

## To load in the current session and also write the infrastructure necessary to 
## ensure the project is auto-loaded for newly-launched R sessions 
activate()

## Default Language: English
Sys.setenv(LANG = "en")
Sys.setlocale("LC_TIME", "English")

############################ Loading the data #############################
base::load("data2model.RData")

## Explaining 
energy_fe_mod <- plm(residential ~ Deferred + Disconnection + Discounts + Free + Other + Personalized + Reconnection + Support + Tariff + incomegroup,
                     model = "within",
                     index = c("Date", "Country"),
                     data = data2model)

coeftest(energy_fe_mod, vcov. = vcovHC, type = "HC1")



