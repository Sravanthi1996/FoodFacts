library(dplyr)
library(ggplot2)
library(readr)
foodFacts <- read_csv("FoodFacts.csv")
 categories <- foodFacts %>%
       filter(!is.na(main_category_en)) %>%
       group_by(main_category_en) %>%
       summarise(count = length(main_category_en)) %>%
       arrange(desc(count))
 foodFacts$main_category_top <- NA
 locs <- foodFacts$main_category_en %in% categories$main_category_en[1:7]
 foodFacts$main_category_top[locs] <- foodFacts$main_category_en[locs]
 ggplot(foodFacts, aes(x=nutrition_score_fr_100g, y=fat_100g, color=main_category_top)) +
       geom_jitter() +
       theme_light(base_size=16)