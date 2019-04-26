# SETUP ------------------------------------------------------------------------

rm(list=ls())
packages <- c("ggplot2", "gridExtra", "grid", "dplyr", "purrr", "MASS", "car", "pROC")
lapply(packages, require, character.only = TRUE)

work <- getwd
gitpath <- paste0(work, "/GitHub/Zombies")


# I. ZOMBIES ! -----------------------------------------------------------------

# Dataset with n = 1 participant with missing zombie status
zombies <- read.delim(file.path(gitpath, "/Zombies.txt")) %>%
  filter(zombie == "...") %>%
  dplyr::select(-water.person)

# Dataset with n = 60 participants with known zombie status
zombies <- read.delim(file.path(gitpath, "/Zombies.txt")) %>%
  filter(!zombie == "...") %>%
  dplyr::select(-water.person)

zombies$zombie <- droplevels(zombies$zombie)

# Create water per person feature
zombies[, c("age", "water", "household")] <- sapply(zombies[, c("age", "water", "household")] , as.numeric)
zombies$water.person <- zombies$water/zombies$household

# Fill in missing values for features "clothing" and "documents"
levels(zombies$clothing) <- c(levels(zombies$clothing), "No clothing")
zombies$clothing[is.na(zombies$clothing)] <- "No clothing"

levels(zombies$documents) <- c(levels(zombies$documents), "No documents")
zombies$documents[is.na(zombies$documents)] <- "No documents"

# Classify features 
vars <- names(dplyr::select(zombies, -c("zombie", "zombieid")))
vars.numeric <- names(select_if(zombies, is.numeric))
vars.discrete <- vars[!vars %in% vars.numeric]

    
# II. Explore differences between zombies and humans ---------------------------

# 1. Test differences

# - Continuous features
exploratory.ttests <- map(zombies[, c(vars.numeric)], ~ t.test(. ~ zombies[, "zombie"]))
names(exploratory.ttests) <- vars.numeric

for(i in 1:length(vars.numeric)){
  if(exploratory.ttests[[vars.numeric[i]]]$p.value < 0.05){
    print(paste("Significant differences in", vars.numeric[[i]], "by zombie status"))
  } else{
    print(paste("No significant differences in", vars.numeric[[i]], "by zombie status"))        
  }
}

# - Discrete features
exploratory.chisq <- map(zombies[, c(vars.discrete)], ~chisq.test(., zombies[, "zombie"]))
names(exploratory.chisq) <- vars.discrete

for(i in 1:length(vars.discrete)){
  if(exploratory.chisq[[vars.discrete[i]]]$p.value < 0.05){
    print(paste("Significant differences in", vars.discrete[[i]], "by zombie status"))
  } else{
    print(paste("No significant differences in", vars.discrete[[i]], "by zombie status"))        
  }
}

# 2. Plot differences
fun_hist <- function(x, y){

  if(class(zombies[, x])=="numeric"){
    title <- ifelse(exploratory.ttests[[x]]$p.value<.05, paste0("Explore ", x, "*"), paste0("Explore ", x)) 
    binwidth <- 1
  } else {
    title <- ifelse(exploratory.chisq[[x]]$p.value<.05, paste0("Explore ", x, "*"), paste0("Explore ", x))
    binwidth <- .5
  }

  ggplot(data = zombies[!(zombies$zombie == "..."), ], aes(x = as.numeric(!!sym(x)), fill = factor(!!sym(y)))) + 
    geom_histogram(binwidth = binwidth) +
    # geom_density(alpha = 0.3) +
    facet_wrap(~zombie) +
    ggtitle(title) + 
    theme(legend.position = "none") +
    labs(x = paste0(x), y = "Density")
}

exploratory.histograms <- map2(vars, "zombie", ~ fun_hist(.x, .y))
names(exploratory.histograms) <- vars

legend <- list(textGrob("** Pink = Human,\n Blue = Zombie", x = 1.6, just = "right", gp = gpar(fontsize = 11)))
allplots <- do.call("grid.arrange", c(exploratory.histograms, legend, ncol = 3))


# III. Build & Assess models----------------------------------------------------

# 1. Build logit models
mod.saturated <- glm(zombie ~ age + rurality + food + medication + firstaid + sanitation + water.person, data=zombies, family = binomial(logit))
summary(mod.saturated)

vif(mod.saturated) # Extremely high signs of multicollinearity
avPlots(mod.saturated) # Medication and rurality show no relationship when controlled for other vars

mod.reduced <- glm(zombie ~ age + rurality + food + medication, data=zombies, family = binomial(logit))
summary(mod.reduced)

vif(mod.reduced) # No signs of multicollinearity
avPlots(mod.reduced) # Medication and rurality show no relationship when controlled for other vars

mod.sparse <- glm(zombie ~ age + food, data=zombies, family = binomial(logit))
summary(mod.sparse)

vif(mod.sparse) # No signs of multicollinearity
avPlots(mod.sparse) # Medication and rurality show no relationship when controlled for other vars

# 2. Compare models
anova(mod.saturated, mod.reduced, mod.sparse)

AIC(mod.saturated,     
    mod.reduced,         
    mod.sparse)  

BIC(mod.saturated,
    mod.reduced,
    mod.sparse) #Both the minimum AIC and BIC values appear alongside the
                #reduced model that we tested above.

# 3. Assess model
zombies$yhatZombie <- predict(mod.reduced, type = "response")

zombies %>% group_by(zombie) %>% summarise(meanyhat = mean(yhatZombie)) # Human = 0.0451, Zombie = 0.955
zombies$predZombie <- ifelse(zombies$yhatZombie > 0.9, 1, 0)
zombies$trueZombie <- ifelse(zombies$zombie=="Zombie", 1, 0)

ROC <- roc(predictor = zombies$predZombie, response = zombies$trueZombie) #AUC = 0.9333
plot(ROC) # Excellent

# 4. Check assumptions
zombies$yhatZombie <- predict(mod.reduced)

ageLinearity <- ggplot(data = zombies, aes(x = age, y = yhatZombie))+
  geom_point(color = "gray") +
  geom_smooth(method = "loess", se = FALSE, color = "orange") + 
  geom_smooth(method = "lm", se = FALSE, color = "gray") + 
  theme_bw() 

# 5. Make out-of-sample prediction
newdata <- data.frame(age = c(71, 20), 
                      water.person = c(5, 5),
                      food = c("Food", "Food"),
                      rurality = c("Suburban", "Rural"),
                      medication = c("Medication", "Medication"),
                      sanitation = c("Sanitation", "Sanitation"))

predict(mod.reduced, newdata, type="response") # Young and old made all the difference
