library(ggplot2)
library(dplyr)
library(reshape2)
library(psych)
COLOR_FILL<- "#CD6771"
COLOR_TWO <- "#0D5047"
COLOR_BORDER <- "#F6A4AC"
BRANDS_COL <- 3
PNNS_1_COL <- 7
PNNS_2_COL <- 8
FAT_COL <- 14
SATURATED_FAT_COL <- 15
CARBOHYDRATES_COL <- 16
PROTEINS_COL <- 18
NUTRITION_SCORE_FR_COL <- 20
NUTRITION_SCORE_UK_COL <- 21
histogram_helper <- function(variable, xlab, bwidth) {
  histogram <- ggplot(food_data_germany, aes(variable)) +
    geom_histogram(binwidth = bwidth, fill = COLOR_FILL, 
                   +                    colour = COLOR_BORDER) +
    labs(x = xlab)
  return(histogram)
}
scatterplot_ingredients <- function(xvariable, yvariable, xl, yl) {
  scat <- ggplot(NULL, aes(x = xvariable, y = yvariable)) +
    geom_point(color = COLOR_FILL) +
    xlab(xl) +
    ylab(yl)
  #labs(xlab = xl, ylab = yl)
  
  return(scat)
}
density_plot <- function(xvariable, fill, xl) {
  ggplot(food_common, aes(x = xvariable, fill = fill)) +
    geom_density(alpha = 0.4) +
    xlab(xl) +
    ylab("density")
}
food_data <- read.csv("FoodFacts.csv", header = TRUE, 
                      +                       sep = ",", stringsAsFactors = TRUE)
food_data_germany <- filter(food_data, countries_en == "Germany")
(na_variables <- food_data_germany %>%
  melt(measure.vars = 1:159) %>%
  group_by(variable) %>% 
  summarize(perc_na = round(sum(is.na(value))/length(value), digits = 3)) %>%
  arrange(desc(perc_na)) %>%
  filter(perc_na < .4) %>%
  as.data.frame)
food_germany <- select(food_data_germany, product_name, generic_name,
                       brands, brands_tags, categories_en, origins, 
                       pnns_groups_1, pnns_groups_2, manufacturing_places, 
                       additives_en, nutrition_grade_fr, main_category_en, 
                       energy_100g, fat_100g, saturated_fat_100g, 
                       carbohydrates_100g, sugars_100g, proteins_100g, 
                       salt_100g, nutrition_score_fr_100g, 
                       nutrition_score_uk_100g)
glimpse(food_germany)
food_germany$pnns_groups_1 <- gsub("^([a-z])(\\w+)", "\\U\\1\\L\\2", 
                                   +                                    food_germany$pnns_groups_1, perl = TRUE)
food_germany$pnns_groups_2 <- gsub("^([a-z])(\\w+)", "\\U\\1\\L\\2", 
                                   +                                    food_germany$pnns_groups_2, perl = TRUE)
food_germany$pnns_groups_1 <- gsub("unknown", "", food_germany$pnns_groups_1)
food_germany$pnns_groups_1 <- as.factor(gsub("-", " ", 
                                             +                                              food_germany$pnns_groups_1))
food_germany$pnns_groups_1 <- gsub(pattern = '([[:upper:]])', perl = TRUE, 
                                   +                                    replacement = '\\L\\1', 
                                   +                                    food_germany$pnns_groups_1)
food_germany$pnns_groups_2 <- gsub(pattern = '([[:upper:]])', perl = TRUE, 
                                   +                                    replacement = '\\L\\1', 
                                   +                                    food_germany$pnns_groups_2)
histogram_helper(food_germany$fat_100g, "fat_100g", bwidth = 2)
filter(food_germany, saturated_fat_100g > 49, 
       histogram_helper(food_germany$proteins_100g, "proteins_100g", bwidth = 2)
       +        saturated_fat_100g < 60)$product_name
filter(food_germany, proteins_100g > 75)$product_name
nutrition_score_dataframe <- food_germany %>%
  +     melt(measure.vars = NUTRITION_SCORE_FR_COL:NUTRITION_SCORE_UK_COL)
ggplot(nutrition_score_dataframe, aes(x = value)) +
  +     geom_histogram(binwidth = 2, fill = COLOR_FILL, colour = COLOR_BORDER) +
  +     labs(x = "Nutrional Value") +
  +     facet_wrap(~ variable)
ggplot(nutrition_score_dataframe, aes(x = value, fill = variable)) +
  +     geom_density(alpha = .3) +
  +     labs(x = "Nutritional Value")
 common_brands_vector <- sort(table(food_germany$brands), 
                               +                              decreasing = TRUE)[2:30]
 common_brands_names <- names(common_brands_vector)
 common_brands_df <- data.frame(brands = common_brands_names, 
                                 +                                frequency = common_brands_vector)
 pairs(~ proteins_100g +
                     fat_100g+carbohydrates_100g +
                     saturated_fat_100g +
                   nutrition_score_fr_100g +
                     sugars_100g, 
               col = COLOR_FILL, pch = 1, cex = 0.2, data = food_germany,
               main="Scatterplot Matrix")
 food_types <- filter(food_germany, pnns_groups_1 %in% 
                         +                          pnns_groups_1[! pnns_groups_1 %in% c("unknown", "")])
 food_common <- filter(food_types, brands %in% common_brands_names[c(1:5)])
 density_plot(food_common$sugars_100g, food_common$pnns_groups_1, 
               +              "sugars_100g")
