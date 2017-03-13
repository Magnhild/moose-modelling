# trying to run a glm on only Gravberget and removing the quadrats variable, to see how the R^2 changes 

Gravberget12 <- read_excel("Gravberget12.xlsx")


moose <- Gravberget12$Moose_pellet_groups
biomass <- Gravberget12$Total_biomass
FSAV <- Gravberget12$FSAV
bilberry <- Gravberget12$Bilberry_cover
class <- Gravberget12$Cutting_class
dominant <- as.factor(Gravberget12$Dominant_tree_species)
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
quadrat <- as.factor(Gravberget12$Quadrat_name)

library(lme4)

#first I try a basic version, to see if I am on track

linear.model1 <- glm(moose ~ biomass + FSAV + bilberry + class, data=Gravberget12, family = poisson)
anova(linear.model1)

summary(linear.model1)

#ok, so this basic verison of the model is working, R^2 = 0.02. Now we add all the other variables and start narrowing it down

linear.model2 <- glm(moose ~ biomass + FSAV + bilberry + class + dominant + roedeer + reddeer + psy + hunting + veg + acc + psyacc + trees + species + house + road + altitude + quadrat, data=Gravberget12, family = poisson)
anova(linear.model2)

summary(linear.model2)

#Model is running! :D Ok, first observations: hunting and wolf (which I forgot to add) is the same the whole way through, so those go out.
#red deer also has no variation, and goes out as well.
#I forgot to add quadrat as factor, so I remove the three variables and try again

linear.model3 <- glm(moose ~ biomass + FSAV + bilberry + class + dominant + roedeer + psy + veg + acc + psyacc + trees + species + house + road + altitude + quadrat, data=Gravberget12, family = poisson)
anova(linear.model3)

summary (linear.model3)

#I think the next step is take out the one with the highest p value, so I am removing roedeer from analysis
#problem: most is as expected except biomass which has a very high p value. I don't know. 

linear.model4 <- glm(moose ~ biomass + FSAV + bilberry + class + dominant + psy + veg + acc + psyacc + trees + species + house + road + altitude + quadrat, data=Gravberget12, family = poisson)
anova(linear.model4)

summary (linear.model4)

#that works. I forgot to compare models, so lets do that now

anova(linear.model1,linear.model2)
anova(linear.model2, linear.model3)
anova(linear.model3, linear.model4)
# I mean its not giving me a p value here. How do I know if they are significantly different?
#I dont really know what to do about that
#I guess we just keep going and try to do the actaul stepwise reduction function later
#I guessI keep going unitl all remaining variables are flagged as significant?

#I mean the model is WORKING but biomass has the highest p value and i really dont want to take it out...
#individual categories of veg and dominant trees have a higher p value, but i dont think I can easily take that out SO
#remove biomass from analysis

linear.model5 <- glm(moose ~ FSAV + bilberry + class + dominant + psy + veg + acc + psyacc + trees + species + house + road + altitude + quadrat, data=Gravberget12, family = poisson)
anova(linear.model5)

summary (linear.model5)

#remove house from analysis

linear.model6 <- glm(moose ~ FSAV + bilberry + class + dominant + psy + veg + acc + psyacc + trees + species + road + altitude + quadrat, data=Gravberget12, family = poisson)
anova(linear.model6)

summary (linear.model6)

#remove FSAV from analysis 
linear.model7 <- glm(moose ~ bilberry + class + dominant + psy + veg + acc + psyacc + trees + species + road + altitude + quadrat, data=Gravberget12, family = poisson)
anova(linear.model7)

summary (linear.model7)

#remove species from analysis 

linear.model8 <- glm(moose ~ bilberry + class + dominant + psy + veg + acc + psyacc + trees + road + altitude + quadrat, data=Gravberget12, family = poisson)
anova(linear.model8)

summary (linear.model8)

#remove altitude from analysis

linear.model9 <- glm(moose ~ bilberry + class + dominant + psy + veg + acc + psyacc + trees + road + quadrat, data=Gravberget12, family = poisson)
anova(linear.model9)

summary (linear.model9)

#remove psy

linear.model10 <- glm(moose ~ bilberry + class + dominant + veg + acc + psyacc + trees + road + quadrat, data=Gravberget12, family = poisson)
anova(linear.model10)

summary (linear.model10)

#remove psyacc

linear.model11 <- glm(moose ~ bilberry + class + dominant + veg + acc + trees + road + quadrat, data=Gravberget12, family = poisson)
anova(linear.model11)

summary (linear.model11)

#ok, that does it. all remaining variables are now either significant or part of a factor variable 
#now we calculate r^2 and see how it changes when we remove quadrat 
#R^2 in linear.model11 is 0.38
#now we remove quadrats

linear.model12 <- glm(moose ~ bilberry + class + dominant + veg + acc + trees + road, data=Gravberget12, family = poisson)
anova(linear.model12)

summary (linear.model12)

#R^2 of linear.model12 is 0.16 
#therefore the spatial element is really important 
#and I think that means we really need to use a mixed model 
#but first I want to try this entire thing again, but with all three sites and THEN remove the spatial grouping.

linear.model13 <- step(linear.model2)
summary(linear.model13)

#this model has an R^2 of 0.38
#it's actaully the same as linear.model12, but without the road variable 
#now the same model, without quadrats...

linear.model14 <- glm(moose ~ bilberry + class + trees + acc + dominant + veg, data=Gravberget12, family = poisson)
anova(linear.model14)

summary(linear.model14)

#The R^2 of this model is 0.16 again. Same problems
#If nothing else at least i did the stepwise regression by hand correctly... 

#PS: I realised I forgot the area variable in this model
