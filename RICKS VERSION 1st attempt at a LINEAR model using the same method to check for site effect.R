first_data <- read_excel("12_finaldata.xlsx")

moose <- (first_data$Moose_pellet_groups)

house <- (first_data$Distance_to_houses)

area <- (first_data$Area)

road <- (first_data$Distance_to_roads)

biomass <- (first_data$Total_biomass)

reddeer <- (first_data$Red_deer_pellets)

roedeer <- (first_data$Roe_deer_pellets)

hunting <- (first_data$Hunting_intensity)

psy <- (first_data$Psy_biomass)

veg <- (first_data$Vegetation_type)

class <- (first_data$Cutting_class)

bilberry <- (first_data$Bilberry_cover)

wolf <- (first_data$Wolf_presence)

dominant <- (first_data$Dominant_tree_species)

acc <- (first_data$Accumulated_browsing)

accpsy <- (first_data$`Psy_accumulated_browsing`)

species <- (first_data$Number_of_tree_species)

trees <- (first_data$Number_of_trees)

library(lme4)

linear.model1 <- glm(moose ~   biomass + Site/Quadrat_name + area  + house + road + bilberry + class + psy + FSAV + veg+ wolf + dominant + trees + acc + hunting + accpsy + species, data=first_data, family = poisson) # link function, but no p value
anova(linear.model1)

#drop species
linear.model2 <- glm(moose ~   biomass + Site/Quadrat_name + area  + house + road + bilberry + class + psy + FSAV + veg+ wolf + dominant + trees + acc + hunting + accpsy, data=first_data, family = poisson) # link function, but no p value
anova(linear.model2)
summary(linear.model2)

anova(linear.model1,linear.model2)

summary(linear.model2)

linear.model3<-step(linear.model2)

anova(linear.model3, test="Chisq")

library(MuMIn)
r.squaredGLMM(linear.model2)

