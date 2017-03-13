#trying to run a glmer on the data from 2012 in Gravberget ONLY

Gravberget12 <- read_excel("Gravberget12.xlsx")

moose <- Gravberget12$Moose_pellet_groups

biomass <- Gravberget12$Total_biomass

FSAV <- Gravberget12$FSAV

bilberry <- Gravberget12$Bilberry_cover

library(lme4)

#Starting with a basic model with only a few variables, to see when the trouble starts

IND <- factor(1:nrow(Gravberget12))
mixed.model1 <- glmer(moose ~ biomass + FSAV + bilberry + (1|IND) + (1|Quadrat_name), data=Gravberget12, family = poisson)
anova(mixed.model1)

summary(mixed.model1)

library(MuMIn)
r.squaredGLMM(mixed.model1)
#warning on r.squared: too close to zero, estimates may be unreliable 

#Mm1 runs withour errors or warnings, but produces the same problem of nearly identical R squared values. Therefore I am adding variables one at a time to see how it changes 
#firstly I am adding cutting class

class <- Gravberget12$Cutting_class

IND <- factor(1:nrow(Gravberget12))
mixed.model2 <- glmer(moose ~ biomass + FSAV + bilberry + class + (1|IND) + (1|Quadrat_name), data=Gravberget12, family = poisson)
anova(mixed.model2)

summary(mixed.model2)

r.squaredGLMM(mixed.model2)
# nothing changes yet, keep going 
#adding the first FACTOR: dominant species

dominant <- as.factor(Gravberget12$Dominant_tree_species)

IND <- factor(1:nrow(Gravberget12))
mixed.model3 <- glmer(moose ~ biomass + FSAV + bilberry + dominant + (1|IND) + (1|Quadrat_name), data=Gravberget12, family = poisson)
anova(mixed.model3)

summary(mixed.model3)

r.squaredGLMM(mixed.model3)

#NOW we are getting warnings, and the r^2 is 0.90. 
#just for fun, trying to add all remaining variables and see what changes

reddeer <- Gravberget12$Red_deer_pellets
roedeer <- Gravberget12$Roe_deer_pellets
psy <- Gravberget12$Psy_biomass
hunting <- Gravberget12$Hunting_intensity
veg <- as.factor(Gravberget12$Vegetation_type)
acc <- Gravberget12$Accumulated_browsing
psyacc <- Gravberget12$Psy_accumulated_browsing
trees <- Gravberget12$Number_of_trees
species <- Gravberget12$Number_of_tree_species
house <- Gravberget12$Distance_to_houses
road <- Gravberget12$Distance_to_roads
altitude <- Gravberget12$Meters_above_sea_level

IND <- factor(1:nrow(Gravberget12))
mixed.model4 <- glmer(moose ~ biomass + FSAV + bilberry + dominant + reddeer + roedeer + psy + hunting + veg + acc + psyacc + trees + species + house + road + altitude + (1|IND) + (1|Quadrat_name), data=Gravberget12, family = poisson)
anova(mixed.model4)

summary(mixed.model4)

r.squaredGLMM(mixed.model4)

# Now the model fails to converge, even with only one site (Gravberget) 

