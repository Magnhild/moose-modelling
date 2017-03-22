#trying a glmer with nesting only in quadrats and not in sites 

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

mixed.model1 <- glmer(moose ~ bilberry + biomass + (1|Site/Quadrat_name), data=first_data, family = poisson)

#model runs, but it is recommended to rescale some variables 
#so let's try that

bilberry <- scale (bilberry)
biomass <- scale (biomass)

#then we run the model again

mixed.model1 <- glmer(moose ~ bilberry + biomass + (1|Site/Quadrat_name), data=first_data, family = poisson)

#no warnings! swell
#lets try to get a R^2 vaule just for fun

IND <- factor(1:nrow(first_data))
mixed.model1 <- glmer(moose ~ bilberry + biomass + (1|IND) + (1|Site/Quadrat_name), data=first_data, family = poisson)
anova (mixed.model1)

library(MuMIn)
r.squaredGLMM(mixed.model1)

#very low R2 and also identical, but we knew that already
#I mean who's to say no one will publish a model which explains 0.3% of moose habitat selection

#Now we remove site and see if the model still works 

IND <- factor(1:nrow(first_data))
mixed.model2 <- glmer(moose ~ bilberry + biomass + (1|IND) + (1|Quadrat_name), data=first_data, family = poisson)
anova (mixed.model2)

r.squaredGLMM(mixed.model2)

#So the model still runs and R squared has changed MINIMALLY 
#Now we add the rest of the variables 
#bear in mind, I am keeping site variables such as hunting intensity and wolf presence 
IND <- factor(1:nrow(first_data))
mixed.model3 <- glmer(moose ~ house + area + road + biomass + reddeer + roedeer + hunting + psy + veg + class + bilberry + wolf + dominant + acc + accpsy + species + trees + altitude + (1|IND) + (1|Quadrat_name), data=first_data, family = poisson)
anova (mixed.model3)

r.squaredGLMM(mixed.model3)

#the R2 is actaully slightly more different than it was before i think
#the model works but we are still getting ye olde convergence and scale issues
#do I rescale now, or after we've narrowed the list down? 
#who knows
#not me
#I'm going to narrow it down first 

#remove roedeer
#also drop red deer, because it is apparently 0 all the way down 

IND <- factor(1:nrow(first_data))
mixed.model4 <- glmer(moose ~ house + area + road + biomass + hunting + psy + veg + class + bilberry + wolf + dominant + acc + accpsy + species + trees + altitude + (1|IND) + (1|Quadrat_name), data=first_data, family = poisson)
anova (mixed.model4)

anova(mixed.model3,mixed.model4)

r.squaredGLMM(mixed.model4)

#drop altitude

IND <- factor(1:nrow(first_data))
mixed.model5 <- glmer(moose ~ house + area + road + biomass + hunting + psy + veg + class + bilberry + wolf + dominant + acc + accpsy + species + trees + (1|IND) + (1|Quadrat_name), data=first_data, family = poisson)
anova (mixed.model5)

anova(mixed.model4,mixed.model5)

r.squaredGLMM(mixed.model5)

#in a very unexpected series of events there seems to be a significant difference between mm4 and mm5 
#r2 of mm5 is at 0.60 so you know if i could BELIEVE that this was it that would be grand 
#but no
#I think we keep going and we will see what happens

#drop house

IND <- factor(1:nrow(first_data))
mixed.model6 <- glmer(moose ~ area + road + biomass + hunting + psy + veg + class + bilberry + wolf + dominant + acc + accpsy + species + trees + (1|IND) + (1|Quadrat_name), data=first_data, family = poisson)
anova (mixed.model6)

anova(mixed.model5,mixed.model6)

r.squaredGLMM(mixed.model6)

#drop dominant species

IND <- factor(1:nrow(first_data))
mixed.model7 <- glmer(moose ~ area + road + biomass + hunting + psy + veg + class + bilberry + wolf + acc + accpsy + species + trees + (1|IND) + (1|Quadrat_name), data=first_data, family = poisson)
anova (mixed.model7)

anova(mixed.model6,mixed.model7)

r.squaredGLMM(mixed.model7)

#drop bilberry (...?)

IND <- factor(1:nrow(first_data))
mixed.model8 <- glmer(moose ~ area + road + biomass + hunting + psy + veg + class + wolf + acc + accpsy + species + trees + (1|IND) + (1|Quadrat_name), data=first_data, family = poisson)
anova (mixed.model8)

anova(mixed.model7,mixed.model8)

r.squaredGLMM(mixed.model8)

#very close to being significantly different
#drop road

IND <- factor(1:nrow(first_data))
mixed.model9 <- glmer(moose ~ area + biomass + hunting + psy + veg + class + wolf + acc + accpsy + species + trees + (1|IND) + (1|Quadrat_name), data=first_data, family = poisson)
anova (mixed.model9)

anova(mixed.model8,mixed.model9)

r.squaredGLMM(mixed.model9)

#remove accpsy

IND <- factor(1:nrow(first_data))
mixed.model10 <- glmer(moose ~ area + biomass + hunting + psy + veg + class + wolf + acc + species + trees + (1|IND) + (1|Quadrat_name), data=first_data, family = poisson)
anova (mixed.model10)

anova(mixed.model9,mixed.model10)

r.squaredGLMM(mixed.model10)

#ITS DIFFERENT
#back to mm9 we go
#so thats still 11 significant explanatory variables 
#here's mm9 again for posterity reasons 

IND <- factor(1:nrow(first_data))
mixed.model9 <- glmer(moose ~ area + biomass + hunting + psy + veg + class + wolf + acc + accpsy + species + trees + (1|IND) + (1|Quadrat_name), data=first_data, family = poisson)
anova (mixed.model9)

anova(mixed.model8,mixed.model9)

r.squaredGLMM(mixed.model9)

#running the model gives us 3 warning messages 
#lets see what we can do
#theres the failure to converge and rescalability
#so let's start with rescaling the obvious ones
#from earlier we have:

bilberry <- scale (bilberry)
biomass <- scale (biomass)

#bilberry is out of the model
#lets rescale some more

hunting <- scale(hunting)
psy <- scale(psy)
trees <- scale(trees)

#lets try running it again
#though I am getting the feeling I definitely should have rescaled things before narrowing it down

IND <- factor(1:nrow(first_data))
mixed.model9 <- glmer(moose ~ area + biomass + hunting + psy + veg + class + wolf + acc + accpsy + species + trees + (1|IND) + (1|Quadrat_name), data=first_data, family = poisson)
anova (mixed.model9)

r.squaredGLMM(mixed.model9)

#one warning message less! 
#but still convergence and scale
#so lets scale some more

species <- scale(species)

#and run

IND <- factor(1:nrow(first_data))
mixed.model9 <- glmer(moose ~ area + biomass + hunting + psy + veg + class + wolf + acc + accpsy + species + trees + (1|IND) + (1|Quadrat_name), data=first_data, family = poisson)
anova (mixed.model9)

r.squaredGLMM(mixed.model9)

#Two new warning messages!!!! 
#neither of which is failed to converge!!!
#praise the stats gods 
#R2 is 0.13, but still identical though
#so the PROBLEM is still there, but at least there are new warnings 

#finally I am going to drop the quadrat name and see what happens to the R2

IND <- factor(1:nrow(first_data))
mixed.model11 <- glmer(moose ~ area + biomass + hunting + psy + veg + class + wolf + acc + accpsy + species + trees + (1|IND), data=first_data, family = poisson)
anova (mixed.model11)

r.squaredGLMM(mixed.model11)

#So the mdoel still works when I drop quadrat as a nesting variable
#and the R2 goes from 0.13 to 0.71, but is still identical in r2m and r2c
#I have no idea what this means 

# now, just cuz, i am trying to add both quadrats and sites again to see how the r2 changes

IND <- factor(1:nrow(first_data))
mixed.model12 <- glmer(moose ~ area + biomass + hunting + psy + veg + class + wolf + acc + accpsy + species + trees + (1|IND) + (1|Site/Quadrat_name), data=first_data, family = poisson)
anova (mixed.model12)

r.squaredGLMM(mixed.model12)

#r2 is .24
# now let's try it with just site

IND <- factor(1:nrow(first_data))
mixed.model13 <- glmer(moose ~ area + biomass + hunting + psy + veg + class + wolf + acc + accpsy + species + trees + (1|IND) + (1|Site), data=first_data, family = poisson)
anova (mixed.model13)

r.squaredGLMM(mixed.model13)

#r2 is 0.13 again. 
#cool 
