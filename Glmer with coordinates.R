# Trying a glmer with coordinates instead of site and/or quadrat. Trying to fix the convergence issues. 

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
IND <- factor(1:nrow(first_data))

#redderr is rank deficient so we drop it. Model then runs without error. 

mixed.model1 <- glmer(moose ~ area + x + y + roedeer + biomass + psy + FSAV + hunting + veg + class + bilberry + wolf + dominant + acc + accpsy + species + trees + road + house + altitude + (1|IND) + (1|site/quadrat), data=first_data, family = poisson)
anova(mixed.model1)

summary(mixed.model1)

r.squaredGLMM(mixed.model1)

#now model is running, but with errors. 

x <- scale(x)
y <- scale(y)
roedeer <- scale(roedeer)
biomass <- scale(biomass)
#skipping psy because rank deficiency
FSAV <- scale(FSAV)
hunting <- scale(hunting)
class <- scale(class)
bilberry <- scale(bilberry)
wolf <- scale(wolf)
acc <- scale(acc)
accpsy <- scale(accpsy)
species <- scale(species)
trees <- scale(trees)
road <- scale(road)
house <- scale(house)
altitude <- scale(altitude)
moose <- scale(moose)

mixed.model2 <- glmer(moose ~ area + x + y + roedeer + biomass + psy + FSAV + hunting + veg + class + bilberry + wolf + dominant + acc + accpsy + species + trees + road + house + altitude + (1|IND) + (1|site/quadrat), data=first_data, family = poisson)

#same error is produced. scaling moose: negative values, not allowed. Remove scaling, add variables one by one

mixed.model3 <- glmer(moose ~ area + roedeer + biomass + psy + FSAV + hunting + veg + class + bilberry + wolf + dominant + acc + accpsy + species + trees + road + house + altitude + (1|IND) + (1|site/quadrat), data=first_data, family = poisson)

#model runs without error after removing x and y. Now we try to replace site or quadrat with x and y

mixed.model4 <- glmer(moose ~ area + x + roedeer + biomass + psy + FSAV + hunting + veg + class + bilberry + wolf + dominant + acc + accpsy + species + trees + road + house + altitude + (1|IND) + (1|quadrat), data=first_data, family = poisson)

#new error, now we try scaling

x <- scale(x)
y <- scale(y)
roedeer <- scale(roedeer)
biomass <- scale(biomass)
psy <- scale(psy)
FSAV <- scale(FSAV)
hunting <- scale(hunting)
class <- scale(class)
bilberry <- scale(bilberry)
wolf <- scale(wolf)
acc <- scale(acc)
accpsy <- scale(accpsy)
species <- scale(species)
trees <- scale(trees)
road <- scale(road)
house <- scale(house)
altitude <- scale(altitude)

mixed.model4 <- glmer(moose ~ area + x + roedeer + biomass + psy + FSAV + veg + class + bilberry + dominant + acc + accpsy + species + trees + road + house + altitude + (1|IND) + (1|quadrat), data=first_data, family = poisson)
                   
#nothing. Now check for highly correlated variables   
anova(mixed.model3)

summary(mixed.model3)

r.squaredGLMM(mixed.model3)
