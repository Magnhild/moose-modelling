first_data <- read_excel("12_finaldata.xlsx")

moose <- (first_data$Moose_pellet_groups)
house <- (first_data$Distance_to_houses)
area <- as.factor(first_data$Area)
road <- (first_data$Distance_to_roads)
biomass <- (first_data$Total_biomass)
reddeer <- (first_data$Red_deer_pellets)
roedeer <- (first_data$Roe_deer_pellets)
hunting <- (first_data$Hunting_intensity)
psy <- (first_data$Psy_biomass)
veg <- as.factor(first_data$Vegetation_type)
class <- (first_data$Cutting_class)
bilberry <- (first_data$Bilberry_cover)
wolf <- (first_data$Wolf_presence)
dominant <- as.factor(first_data$Dominant_tree_species)
acc <- (first_data$Accumulated_browsing)
accpsy <- (first_data$`Psy_accumulated_browsing`)
species <- (first_data$Number_of_tree_species)
trees <- (first_data$Number_of_trees)
altitude <- (first_data$Meters_above_sea_level)
site <- as.factor(first_data$Site)
quadrat <- as.factor(first_data$Quadrat_name)

library(lme4)

#first we try a small model to see if it works
linear.model1 <- glm(moose ~ house + area + biomass, data=first_data, family = poisson)
anova(linear.model1)

summary(linear.model1)

#Ok good, model is running, even without warnings
#now we add all the remaining variables

linear.model2 <- glm(moose ~ house + site/quadrat + area + biomass + reddeer + roedeer + hunting + psy + veg + class + bilberry + wolf + dominant + acc + accpsy + species + trees + altitude, data=first_data, family = poisson)
anova(linear.model2)

summary(linear.model2)

#still runs without errors or warnings! 
#Now for a stepwise reduction

linear.model3 <- step(linear.model2)
anova(linear.model3)

linear.model4 <- glm(moose ~ site/quadrat + veg + class + dominant + acc + trees + altitude, data=first_data, family = poisson)

anova(linear.model3,linear.model4, test="Chisq")
anova(linear.model4)
summary(linear.model4)

#adding this 21.02.2017: chi square test the model to get p vaules 
anova(linear.model3, test="Chisq")

#R^2 for this moedl is 0.34 
#Now we remove site and quadrat and try again

linear.model4 <- glm(moose ~ house + veg + class + dominant + acc + trees + altitude, data=first_data, family = poisson)
anova(linear.model4)

summary(linear.model4)

#R^2 for this model is 0.11. So site and quadrat is a big deal, I guess 
#lets keep quadrat, get rid of site

linear.model5 <- glm(moose ~ house + veg + class + dominant + acc + trees + altitude + quadrat, data=first_data, family = poisson)
anova(linear.model5)

summary(linear.model5)

#even without site, this model has an R^2 of 0.34
#now I try to keep site, but get rid of quadrat

linear.model6 <- glm(moose ~ house + veg + class + dominant + acc + trees + altitude + site, data=first_data, family = poisson)
anova(linear.model6)

summary(linear.model6)

#Now the R^2 is 0.12
#So, I think we can conclude that quadrats is very important, but Site is not?
#does that mean we dont have nest it in site, just in quadrat?
#time to try a glmer again, I believe 