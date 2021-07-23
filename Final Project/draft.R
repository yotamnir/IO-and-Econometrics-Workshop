if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, splitstackshape)

# Note – excel edit: text to columns with "|" as a separator, and file renamed. Excel downloaded on 21.07.2021.
prices <- read.csv("Final Project/Prices.csv") %>%
  as_tibble() %>%
  filter(shnat_yitzur > 2016)   # no cars produced before 2017 in quantities data

# Excel downloaded on
quantities <- read.csv("Final Project/Monthly 2018-2021.csv") %>%
  as_tibble()


# Note: prices are unique on the variable list:
# degem_cd degem_nm shnat_yitzur sug_degem tozeret_cd

combined <- full_join(
  quantities, prices,
  by = c(
    "Degem.Cd" = "degem_cd",
    "Degem.Nm" = "degem_nm",
    "Shnat.Yitzur" = "shnat_yitzur",
    "Sug.Degem" = "sug_degem",
    "Tozeret.Cd" = "tozeret_cd"
  )
) %>%
  filter(!is.na(ן..Make.ENG))   # removing price observations without a match








##### Older work #####


# Excel downloaded on 21.07.2021
aggregates <- read.csv(
  "Final Project/Aggregated sales.csv",
  fileEncoding = "UTF-16LE",
  sep = "\t",
  na.strings = ""
) %>%
  as_tibble() %>%
  select(-ends_with(".1")) %>%      # removing duplicate columns
  filter(Make.ENG != "Grand Total") # removing summary row

# Changing variables to numeric where possible
aggregates$Nefah.Manoa <- as.numeric(aggregates$Nefah.Manoa)
aggregates$Mispar.Dlatot <- as.numeric(aggregates$Mispar.Dlatot)
aggregates$Mishkal.Kolel <- as.numeric(aggregates$Mishkal.Kolel)
aggregates$X2020 <- as.numeric(gsub(",", "", aggregates$X2020))
aggregates$X2021 <- as.numeric(gsub(",", "", aggregates$X2021))

# Replacing "Jeep" with "Chrysler" as they are under the same ownership with the same brands (and we cannot distinguish between them in the price data anyway)
aggregates[which(aggregates$Make.ENG == "JEEP"), 1] <- "CHRYSLER"

# Joining by brand
combined <- full_join(
  aggregates,
  prices,
  by = c("Kinuy.Mishari" = "kinuy_mishari")
)

