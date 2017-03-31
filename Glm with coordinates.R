# trying a glm with coordinates instead of site and/or quadrat

first_data <- read_excel("12_finaldata.xlsx")

moose <- (first_data$Moose_pellet_groups)
site <- as.factor(first_data$Site)
area <- as.factor(first_data$Area)
quadrat <- as.factor(first_data$Quadrat_name)
x <- (first_data$x)
y <- (first_data$y)

reddeer <- (first_data$Red_deer_pellets)
roedeer <- (first_data$Roe_deer_pellets)

biomass <- (first_data$Total_biomass)
psy <- (first_data$Psy_biomass)
FSAV <- (first_data$FSAV)

hunting <- (first_data$Hunting_intensity)
veg <- as.factor(first_data$Vegetation_type)
class <- (first_data$Cutting_class)
bilberry <- (first_data$Bilberry_cover)
wolf <- (first_data$Wolf_presence)

dominant <- as.factor(first_data$Dominant_tree_species)
acc <- (first_data$Accumulated_browsing)
accpsy <- (first_data$`Psy_accumulated_browsing`)
species <- (first_data$Number_of_tree_species)
trees <- (first_data$Number_of_trees)
road <- (first_data$Distance_to_roads)
house <- (first_data$Distance_to_houses)
altitude <- (first_data$Meters_above_sea_level)

library(lme4)
library(MuMIn)


linear.model1 <- glm(moose ~ site/quadrat + area + x + y + reddeer + roedeer + biomass + psy + hunting + veg + class + bilberry + wolf + dominant + acc + accpsy + species + trees + road + house + altitude, data=first_data, family = poisson)
anova(linear.model1)

summary(linear.model1)
# R^2 is 0.16

linear.model2 <- step(linear.model1)
anova(linear.model2, test="Chisq")
summary(linear.model2)

#R^2 for mm2 is 0.16
#and tests says not to remove any more variables 
#so we have a functioning glm, R^2 = 0.15
#I am going to maually add quadrats and see if the r^2 improves

linear.model3 <- glm(moose ~ x + first_data$Quadrat_name +  house + area + veg + class + dominant + acc + trees, data=first_data, family = poisson)
anova(linear.model3)
summary(linear.model3)

#R^2 is now at 0.33
#so if we can USE this model at least the R^2 is decent 
#for some godforsaken reason this model is using x coordinates 
#maybe because the sites are more equally distributed on the x axis? categorically?


#I am trying to add site

linear.model4 <- glm(moose ~ x + first_data$Site/first_data$Quadrat_name +  house + area + veg + class + dominant + acc + trees, data=first_data, family = poisson)
anova(linear.model4)
summary(linear.model4)

#R^2 is still 0.33

