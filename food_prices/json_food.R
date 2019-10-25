library(jsonlite)
library(tidyverse)

# # read remote json
# json_file <- "http://realtime-api.opendatanepal.com/kalimati-price/api/archive?id=138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,256,257,258,259,260,261,261,263,264,265,266,186&date_range=2019-01-15,2019-10-04&format=json"
# json_data <- fromJSON(json_file, flatten=F, simplifyDataFrame = TRUE)

# read local json
json_data <- fromJSON("./food_prices/food_price.json", flatten = T)


u <- json_data %>% 
  as.data.frame() %>% 
  map_if(is.data.frame, list) %>% 
  as_tibble() %>% 
  unnest(result.retailPrice) %>% 
  rename_at(c("unit", "min", "max", "avg", "date"), 
            list(~paste0(., ".x"))) %>% 
  unnest(result.wholesalePrice) 

u <- u %>% distinct()

# unnest(result.wholesalePrice)

# select results
result <- json_data$result
retail <- result$retailPrice
wholesale <- result$wholesalePrice

# select names communityName and no
nam <- result %>% select(1, 2) %>% mutate(no = row.names(result))

# combine rows by numbers
retail_df <- retail %>% bind_rows(.id = "no") %>% distinct()
wholesale_df <- wholesale %>% bind_rows(.id = "no") %>% distinct()

# full join
i <- full_join(retail_df, nam, by="no") %>% distinct()
j <- full_join(wholesale_df, nam, by="no") %>% distinct()


food_price <- bind_rows(i, j, .id = "price_type") %>% 
  mutate(price_type = case_when(
  str_detect(price_type, "1") ~ "Retail",
  TRUE ~ "Wholesale"
)) %>%  select(-no)

food_price <- food_price %>% select(date, everything())
food_price$date <- as.Date(food_price$date)


write_csv(food_price, "./food_prices/food_prices.csv")



# 
# 
# k <- full_join(i, j, by=c("no", "commodityName", "date", "id")) %>% distinct()


















# your_df <- retail  %>%
  # purrr::flatten() %>%
  # # turn nested lists into dataframes
  # map_if(is_list, as_tibble) %>%
  # # bind_cols needs tibbles to be in lists
  # map_if(is_tibble, list) %>%
  # creates nested dataframe
  # bind_rows(.id = "id")


# your_df <- retail  %>%
#   unlist() %>%
#   tibble::enframe(name = NULL)


# do.call(rbind.data.frame, retail )

# your_df <- retail %>% 
#   unlist(recursive = FALSE) %>% 
#   enframe() %>% 
#   unnest()


# json = '{"category1": {"town1": 8,"town2": 2},"category2": {"town1": 4,"town2": 3}}'
# library(plyr)
# ldply(fromJSON(json), stack)

# bind_rows(retail, .id = "id")




