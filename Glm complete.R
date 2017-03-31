# trying a glm with EVERYTHING and a right stepwise reduction 

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

linear.model1 <- glm(moose ~ site/quadrat + area + x + y + reddeer + roedeer + biomass + psy + FSAV + hunting + veg + class + bilberry + wolf + dominant + acc + accpsy + species + trees + road + house + altitude, data=first_data, family = poisson)
anova(linear.model1)

summary(linear.model1)
# R^2 is 0.35, amazingly

linear.model2 <- step(linear.model1)
anova(linear.model2, test="Chisq")
anova(linear.model1,linear.model2, test="Chisq")
summary(linear.model2)

#R^2 for mm2 is 0.34
#chi square says is is super different from lm1
#Now we remove one variable and see what happens

#remove altitude

linear.model3 <- glm(moose ~ site + veg + class + dominant + acc + trees + road + site/quadrat, data=first_data, family = poisson)
anova(linear.model3, linear.model2, test="Chisq")
anova(linear.model3, test="Chisq")
summary(linear.model3)

#not significantly different from glm2, but only by a little bit
#R^2 is 0.34
#so if we can USE this model at least the R^2 is decent 

#remove road

linear.model4 <- glm(moose ~ site + veg + class + dominant + acc + trees + site/quadrat, data=first_data, family = poisson)
anova(linear.model3, linear.model4, test="Chisq")
anova(linear.model4, test="Chisq")
summary(linear.model4)

#R^2 is still 0.34
#remove dominant tree species

linear.model5 <- glm(moose ~ site + veg + class + acc + trees + site/quadrat, data=first_data, family = poisson)
anova(linear.model5, linear.model4, test="Chisq")

#it is significantly different, so we keep glm4
#i've tried removing site as a separate explanatory variable and the result is identical



