first_data <- read_excel("~/Master by Bioresearch, MRes/DATA ANALYSIS/R Forsøk/02.02.2017/12_finaldata.xlsx")

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

mixed.model1 <- glmer(moose ~   biomass + area + house + road + bilberry + class + psy + FSAV + veg+ wolf + dominant + trees + acc + hunting + accpsy + species + (1|Site/Quadrat_name), data=first_data, family = poisson) # link function, but no p value
anova(mixed.model1)

#drop bilberry
mixed.model2 <- glmer(moose ~   biomass + area + house + road + class + psy + FSAV + veg+ wolf + dominant + trees + acc + hunting + accpsy + species + (1|Site/Quadrat_name), data=first_data, family = poisson) # link function, but no p value
anova(mixed.model2)

anova(mixed.model1,mixed.model2)

#drop species
mixed.model3 <- glmer(moose ~   biomass + area + house + road + class + psy + FSAV + veg+ wolf + dominant + trees + acc + hunting + accpsy + (1|Site/Quadrat_name), data=first_data, family = poisson) # link function, but no p value
anova(mixed.model3)

anova(mixed.model2,mixed.model3)

#drop road
mixed.model4 <- glmer(moose ~   biomass + area + house + class + psy + FSAV + veg+ wolf + dominant + trees + acc + hunting + accpsy + (1|Site/Quadrat_name), data=first_data, family = poisson) # link function, but no p value
anova(mixed.model4)

anova(mixed.model3,mixed.model4)

#drop hunting
mixed.model5 <- glmer(moose ~   biomass + area + house + class + psy + FSAV + veg+ wolf + dominant + trees + acc + accpsy + (1|Site/Quadrat_name), data=first_data, family = poisson) # link function, but no p value
anova(mixed.model5)

anova(mixed.model4,mixed.model5)

#drop accpsy
mixed.model6 <- glmer(moose ~   biomass + area + house + class + psy + FSAV + veg+ wolf + dominant + trees + acc + (1|Site/Quadrat_name), data=first_data, family = poisson) # link function, but no p value
anova(mixed.model6)

anova(mixed.model5,mixed.model6)

#drop house
mixed.model7 <- glmer(moose ~   biomass + area + class + psy + FSAV + veg+ wolf + dominant + trees + acc + (1|Site/Quadrat_name), data=first_data, family = poisson) # link function, but no p value
anova(mixed.model7)

anova(mixed.model6,mixed.model7)

#drop FSAV
IND <- factor(1:nrow(first_data))
mixed.model7 <- glmer(moose ~   biomass + area + class + psy + veg + wolf + dominant + trees + acc + (1|Site/Quadrat_name) + (1|IND), data=first_data, family = poisson) # link function, but no p value
anova(mixed.model7)

anova(mixed.model6,mixed.model7)

#drop dominant
mixed.model8 <- glmer(moose ~   biomass + area + class + psy + veg+ wolf + trees + acc + (1|Site/Quadrat_name), data=first_data, family = poisson) # link function, but no p value
anova(mixed.model8)

anova(mixed.model7,mixed.model8)

#keep model7

summary(mixed.model7)


library(MuMIn)
r.squaredGLMM(mixed.model7)
