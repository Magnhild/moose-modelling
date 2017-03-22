#trying to make a glm with only plassen to see how variables and r2 is different from models based on gravberget and ljørdalen

Plassen12 <- read_excel("Plassen12.xlsx")

moose <- Plassen12$Moose_pellet_groups
area <- Plassen12$Area
biomass <- Plassen12$Total_biomass
FSAV <- Plassen12$FSAV
bilberry <- Plassen12$Bilberry_cover
class <- Plassen12$Cutting_class
dominant <- as.factor(Plassen12$Dominant_tree_species)
reddeer <- Plassen12$Red_deer_pellets
roedeer <- Plassen12$Roe_deer_pellets
psy <- Plassen12$Psy_biomass
hunting <- Plassen12$Hunting_intensity
veg <- as.factor(Plassen12$Vegetation_type)
acc <- Plassen12$Accumulated_browsing
psyacc <- Plassen12$Psy_accumulated_browsing
trees <- Plassen12$Number_of_trees
species <- Plassen12$Number_of_tree_species
house <- Plassen12$Distance_to_houses
road <- Plassen12$Distance_to_roads
altitude <- Plassen12$Meters_above_sea_level
quadrat <- as.factor(Plassen12$Quadrat_name)

library(lme4)

#first I try a basic version, to see if I am on track

linear.model1 <- glm(moose ~ biomass + FSAV + bilberry + class, data=Plassen12, family = poisson)
anova(linear.model1)

summary(linear.model1)

#it works! Now add all the other variables

linear.model2 <- glm(moose ~ biomass + area + FSAV + bilberry + class + dominant + roedeer + reddeer + psy + hunting + veg + acc + psyacc + trees + species + house + road + altitude + quadrat, data=Plassen12, family = poisson)
anova(linear.model2)

summary(linear.model2)

#still works! One warning message, but that is like an all time low so A+ really
#Now remove the variables which should not be there anyway: wolf, hunting, roedeer and reddeer

linear.model3 <- glm(moose ~ biomass + area + FSAV + bilberry + class + dominant + psy + veg + acc + psyacc + trees + species + house + road + altitude + quadrat, data=Plassen12, family = poisson)
anova(linear.model3)

summary(linear.model3)

#now we do the stepwise reduction
#go team

linear.model4 <- step(linear.model3)
anova(linear.model4)

summary(linear.model4)

#R2 = 0.36
#perf. Now drop quadrat and compare

linear.model5 <- glm(moose ~ FSAV + class + veg + trees + house, data=Plassen12, family = poisson)
anova(linear.model5)

summary(linear.model5)

#no warning messages this time actually. R2 = 0.15
#observation: the factors remain the same with and without quadrats, only the R2 changes 