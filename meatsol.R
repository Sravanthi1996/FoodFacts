 library(ggplot2)
 FoodFacts = read.csv("FoodFacts.csv", header = TRUE) 
 meat = grep("meat|Meat", FoodFacts$categories_en)
 vegan = grep("vegan|Vegan", FoodFacts$labels)
 Countries = as.data.frame(table(FoodFacts$countries_en))
 Countries = Countries[-1,]
 Top = head(Countries[order(-Countries$Freq),],10)
 Top = droplevels(Top)
 i = 1
 while (i < length(Top$Var1)+1) {
       
         Country = grep(Top[i,1], Countries$Var1)
         Country_totals = sum(Countries$Freq[Country])
         Top[i,2] = Country_totals
         i=i+1
     }
 barplot(Top$Freq, names.arg = Top$Var1, col = "grey", main = "Number of Products by Country"
                   , ylab = "Counts", las=1)
 Countries_meat = as.data.frame(table(FoodFacts$countries_en[meat]))
 Countries_meat = Countries_meat[-1,]
 Top_meat = head(Countries_meat[order(-Countries_meat$Freq),],10)
 Top_meat = Top_meat[-grep(",", Top_meat$Var1),] 
 Top_meat = droplevels(Top_meat)
 i = 1
  while (i < length(Top_meat$Var1)+1) {
       
         Country = grep(Top[i,1], Countries_meat$Var1)
         Country_totals = sum(Countries_meat$Freq[Country])
         Top_meat[i,2] = Country_totals
         i=i+1
     }
 Countries_vegan = as.data.frame(table(FoodFacts$countries_en[vegan]))
 Countries_vegan = Countries_vegan[-1,]
 Top_vegan = head(Countries_vegan[order(-Countries_vegan$Freq),],10)
 Top_vegan = Top_vegan[-grep(",", Top_vegan$Var1),] 
 Top_vegan = droplevels(Top_vegan)
 
   i = 1
   while (i < length(Top_vegan$Var1)+1) {
         
           Country = grep(Top_vegan[i,1], Countries_vegan$Var1)
           Country_totals = sum(Countries_vegan$Freq[Country])
           Top_vegan[i,2] = Country_totals
           i=i+1
       }
   rm(FoodFacts)
   colnames(Top) = c("Country", "Count")
   colnames(Top_meat) = c("Country", "Count")
   colnames(Top_vegan) = c("Country", "Count")
   Results_meat = merge(Top, Top_meat, by = "Country")
   Results_vegan = merge(Top, Top_vegan, by = "Country")
   colnames(Results_meat) = c("Country", "Total no. of products", "No. of meat products")
   colnames(Results_vegan) = c("Country", "Total no. of products", "No. of vegan products")
   Results_meat$MeatPerc = Results_meat$`No. of meat products` / Results_meat$`Total no. of products` * 100
   Results_vegan$VeganPerc = Results_vegan$`No. of vegan products` / Results_vegan$`Total no. of products` * 100
   m = ggplot(Results_meat, aes(x=reorder(Country,-MeatPerc), y=MeatPerc))
   m + geom_bar(stat = "identity", fill="pink", colour="black") +
         ggtitle("Counties with Highest % of Meat Products \n (in terms of no. of products submitted)") +
         ylab("Percentage %") +
         theme_classic() +
         theme(legend.position="none") +
         theme(axis.text.x = element_text(size=15, angle = 90)) +
         scale_x_discrete(name="")
   library(dplyr)+     scale_x_discrete(name="")