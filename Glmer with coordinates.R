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
library(arm)
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
#x, y, and altitude, and psy and biomass are highly correlated (rs>0.8)
#so I try to remove some of them from the model

mixed.model5 <- glmer(moose ~ y + area + roedeer + biomass + FSAV + veg + class + bilberry + dominant + acc + accpsy + species + trees + road + house + (1|IND) + (1|quadrat), data=first_data, family = poisson)

#no combination of that worked 
#now i try to standardise the model
#to do that I need a functioning model, so I remove x + y and try again
#I also remove class
mixed.model6 <- glmer(moose ~ area + x + y + roedeer + biomass + psy + FSAV + hunting + veg + bilberry + wolf + dominant + acc + accpsy + species + trees + road + house + altitude + (1|IND) + (1|quadrat), data=first_data, family = poisson)

stdz.model <- standardize(mixed.model6, standardize.y = FALSE)
#not converging, so i try to automate model selection
model.set <- dredge(stdz.model) 

#this didnt work. I am going to try and rescale the predictor variables again, but using a different method 
#never mind, that didnt work. Now I am increasing the amount of iterations
mixed.model7 <- glmer(moose ~ area + roedeer + biomass + psy + FSAV + hunting + veg + 
                        bilberry + dominant + acc + accpsy + species + trees + road + house + 
                        altitude + (1|IND) + (1|quadrat), data=first_data, family = poisson, 
                      glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 200000)))

#bear in mind these are currently scaled 
#so that actaully worked however Hessian is singular and it is therefore probably a false positive.
#Now I am going to try it with a different optimiser (not bobyqa)


mixed.model8 <- glmer(moose ~ area + roedeer + biomass + psy + FSAV + hunting + veg + 
                        bilberry + dominant + acc + accpsy + species + trees + road + house + 
                        altitude + (1|IND) + (1|quadrat), data=first_data, family = poisson, 
                      glmerControl(optCtrl = list(maxfun = 2e5)))

#so that worked in approximately a quarter of the time, but the convergence warning is back
#so I guess we can't keep it

#I have now descaled the continuous parameters, and I am giving mm7 one more go. 

mixed.model9 <- glmer(moose ~ area + roedeer + biomass + psy + FSAV + hunting + veg + 
                        bilberry + dominant + acc + accpsy + species + trees + road + house + 
                        altitude + (1|IND) + (1|quadrat), data=first_data, family = poisson, 
                      glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 200000)))

#that failed to converge. so we are working with SCALED parameters and 200 000 iterations. 

#Now I try to double check the gradient calculations
derivs1 <- mixed.model7@optinfo$derivs
sc_grad1 <- with(derivs1,solve(Hessian,gradient))
max(abs(sc_grad1))

max(pmin(abs(sc_grad1),abs(derivs1$gradient)))
#this is getting close to the tolerance we usually set (0.001). 
#bear in mind that I have absolutely no idea what that means

#now I am going to give standardisation another go
#but this time without the x and y variables
mixed.model10 <- glmer(moose ~ area + roedeer + biomass + psy + FSAV + hunting + veg + bilberry + wolf + dominant + acc + accpsy + species + trees + road + house + altitude + (1|IND) + (1|quadrat), data=first_data, family = poisson)

stdz.model <- standardize(mixed.model7, standardize.y = FALSE)
#same problems as mm7
#now i try to transform moose, see if that gives us more variation to work with
logmoose <- (moose)^3

mixed.model11 <- glmer(logmoose ~ area + roedeer + biomass + psy + FSAV + hunting + veg + bilberry + wolf + 
                         dominant + acc + accpsy + species + trees + road + house + altitude + 
                         (1|IND) + (1|quadrat), data=first_data, family = poisson)

#warnings of convergense and singluar hessian
#now we try it on the 200000 iterations

mixed.model12 <- glmer(logmoose ~ area + roedeer + biomass + psy + FSAV + hunting + veg + 
                        bilberry + dominant + acc + accpsy + species + trees + road + house + 
                        altitude + (1|IND) + (1|quadrat), data=first_data, family = poisson, 
                      glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 200000)))

#that didnt really work
#now I try 200 000 iterations without the obvious variables
mixed.model12 <- glmer(moose ~ biomass + FSAV + veg + 
                         bilberry + dominant + acc + accpsy + species + trees + road + 
                         altitude + (1|IND) + (1|quadrat), data=first_data, family = poisson, 
                       glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 200000)))

#now theres a negative hessian instead of a singular one
#I try the same modela s above but with x and y coordinates
mixed.model13 <- glmer(moose ~ biomass + x + y + FSAV + veg + 
                         bilberry + dominant + acc + accpsy + species + trees + road + 
                         altitude + (1|IND) + (1|quadrat), data=first_data, family = poisson, 
                       glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 200000)))

#that still doesnt work
#now i try it with 200 000 iterations but without bobyqa 

mixed.model13 <- glmer(moose ~ biomass + FSAV + veg + 
                         bilberry + dominant + acc + accpsy + species + trees + road + 
                         altitude + (1|IND) + (1|quadrat), data=first_data, family = poisson, 
                       glmerControl(optCtrl = list(maxfun = 200000)))

anova(mixed.model12)

summary(mixed.model1)

r.squaredGLMM(mixed.model12)
