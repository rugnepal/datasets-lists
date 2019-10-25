library(tabulizer)
library(tidyverse)
library(janitor)

# PDF Scrape Tables
f <- "US_overstay/Entry and Exit Overstay Report, Fiscal Year 2016.pdf"

g <- "US_overstay/19_0417_fy18-entry-and-exit-overstay-report.pdf"

n <- extract_tables(f, pages = 21, guess = TRUE, method = "decide")
o <- extract_tables(g, pages = 20, guess = TRUE, method = "decide", output = "data.frame")

names1 <- c("Country of Citizenship Expected",
         "Departures",
         "Out of Country Overstays",
         "Suspected InCountry Overstays",
         "Total Overstays",
         "Total Overstay Rate",
         "Suspected InCountry Overstay Rate")

df <- o %>% 
  pluck(1) %>% 
  unnest()
  

df <- df[-c(1, 2, 3),]

gg <- separate(df, col= Table.2, into = names1, sep = "\\s",
               remove = TRUE)

gg <- gg %>% clean_names()

gg[7, ] <- c("South Korea", 1579221, 1027, 3524, 4551, 0.29, 0.22)
gg[19, ] <- c("Czech Republic", 125142, 174, 612, 786, 0.63, 0.49)
gg[27, ] <- c("New Zealand", 345636, 252, 843, 1095, 0.32, 0.24)
gg[30, ] <- c("San Marino", 731, NA, 3, 3, 0.41, 0.41)

gg <- gg %>% 
  mutate_all(funs(gsub("-", NA, .)))

dd <- gg %>%
  mutate_all(funs(gsub("[%,]", "", .)))

dd$country_of_citizenship_expected <- str_replace(dd$country_of_citizenship_expected,
                                                  "[[:digit:]]+", "")

write_csv(dd, "US_overstay/us_overstay_2018_tourist_visa")

