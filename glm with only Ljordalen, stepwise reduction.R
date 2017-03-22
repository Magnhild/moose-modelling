# trying to run a glm on only Ljørdalen and removing the quadrats variable, to see how the R^2 changes. 
#Purpose: see if its the same as for the Gravberget model 

Ljordalen12 <- read_excel("Ljordalen12.xlsx")


moose <- Ljordalen12$Moose_pellet_groups
area <- Ljordalen12$Area
biomass <- Ljordalen12$Total_biomass
FSAV <- Ljordalen12$FSAV
bilberry <- Ljordalen12$Bilberry_cover
class <- Ljordalen12$Cutting_class
dominant <- as.factor(Ljordalen12$Dominant_tree_species)
reddeer <- Ljordalen12$Red_deer_pellets
roedeer <- Ljordalen12$Roe_deer_pellets
psy <- Ljordalen12$Psy_biomass
hunting <- Ljordalen12$Hunting_intensity
veg <- as.factor(Ljordalen12$Vegetation_type)
acc <- Ljordalen12$Accumulated_browsing
psyacc <- Ljordalen12$Psy_accumulated_browsing
trees <- Ljordalen12$Number_of_trees
species <- Ljordalen12$Number_of_tree_species
house <- Ljordalen12$Distance_to_houses
road <- Ljordalen12$Distance_to_roads
altitude <- Ljordalen12$Meters_above_sea_level
quadrat <- as.factor(Ljordalen12$Quadrat_name)
site <- as.factor(Ljordalen12$Site)

library(lme4)

#first I try a basic version, to see if I am on track

linear.model1 <- glm(moose ~ biomass + FSAV + bilberry + class, data=Ljordalen12, family = poisson)
anova(linear.model1)

summary(linear.model1)

#ok, so this basic verison of the model is working, R^2 = 0.07. Now we add all the other variables and start narrowing it down

linear.model2 <- glm(moose ~ biomass + area + FSAV + bilberry + class + dominant + roedeer + reddeer + psy + hunting + veg + acc + psyacc + trees + species + house + road + altitude + quadrat, data=Ljordalen12, family = poisson)
anova(linear.model2)

summary(linear.model2)

#Model is running! :D Ok, first observations: hunting and wolf (which I forgot to add) is the same the whole way through, so those go out.
#red deer also has no variation, and goes out as well.
#I forgot to add quadrat as factor, so I remove the three variables and try again

linear.model3 <- glm(moose ~ biomass + area + FSAV + bilberry + class + dominant + roedeer + psy + veg + acc + psyacc + trees + species + house + road + altitude + quadrat, data=Ljordalen12, family = poisson)
anova(linear.model3)

summary (linear.model3)

#Next we do the stepwise reduction that we know and love

linear.model4 <- step(linear.model3)
anova(linear.model4)

summary (linear.model4)

#50 or more warnings. Nothing we haven't seen before. 
#this model actually has no significant individual quadrats 


#ok, that does it. Not all remaining variables are significant, but ok.  
#now we calculate r^2 and see how it changes when we remove quadrat 
#R^2 in linear.model4 is 0.37, whereas Gravberget was 0.38. 
#There definitely are different variables than in Graberget though
#now we remove quadrats

linear.model5 <- glm(moose ~ bilberry + dominant + acc + trees + altitude, data=Ljordalen12, family = poisson)
anova(linear.model5)

summary (linear.model5)

#R^2 of linear.model5 is 0.15, whereas Gravberget was 0.16 
#therefore the spatial element is really important here as well
#and I think that means we really need to use a mixed model 
# first I am going to add both site and quadrat again, see what happens 


linear.model6 <- glm(moose ~ bilberry + site/quadrat + class + trees + acc + dominant + veg, data=Ljordalen12, family = poisson)
anova(linear.model6)

summary(linear.model6)

#The R^2 of this model is 0.16 again. Same problems
#If nothing else at least i did the stepwise regression by hand correctly... 

#PS: I realise now that obviously site doesnt matter on a model using only ljørdalen