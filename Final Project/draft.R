if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, splitstackshape)

# Note – excel edit: text to columns with "|" as a separator. Excel downloaded on 21.07.2021
prices <- read.csv("Final Project/Prices.csv") %>% as_tibble()

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

