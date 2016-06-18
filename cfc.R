library(ggplot2) 
 system("ls ../input")
df = read.csv("FoodFacts.csv", header = TRUE, stringsAsFactors = FALSE)
 library(dplyr)
 df %>%
       group_by(countries_en) %>%
       summarise(median.carbon_footprint_100g = median(carbon_footprint_100g, na.rm = TRUE),
                                median.carbohydrates_100g = median(carbohydrates_100g, na.rm = TRUE)) %>%
       as.data.frame() -> country.summary
 ggplot(na.omit(country.summary), aes(x = reorder(countries_en, -median.carbon_footprint_100g), y = log10(median.carbon_footprint_100g))) + 
       geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1))-> p
 print(p)