# SETUP ------------------------------------------------------------------------

rm(list=ls())
packages <- c("ggplot2", "gridExtra", "grid", "dplyr", "purrr", "MASS", "car", "pROC", "naivebayes")
lapply(packages, require, character.only = TRUE)

work <- getwd()
gitpath <- paste0(work, "/GitHub/Zombies")


# I. ZOMBIES ! -----------------------------------------------------------------

# Dataset with n = 200 participants with known zombie status
zombies <- read.delim(file.path(gitpath, "/Zombies.txt")) %>% 
  dplyr::select(-rowname) %>% 
  dplyr::rename(zombieid = rowid.zombieid)

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
vars.numeric <- names(select_if(zombies[,vars], is.numeric))
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
} # Age, water, water per person

# - Discrete features
exploratory.chisq <- map(zombies[, c(vars.discrete)], ~chisq.test(., zombies[, "zombie"]))
names(exploratory.chisq) <- vars.discrete

for(i in 1:length(vars.discrete)){
  if(exploratory.chisq[[vars.discrete[i]]]$p.value < 0.05){
    print(paste("Significant differences in", vars.discrete[[i]], "by zombie status"))
  } else{
    print(paste("No significant differences in", vars.discrete[[i]], "by zombie status"))        
  }
} # Rurality, food, medication, sanitation

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
mod.saturated <- glm(zombie ~ age + water.person + rurality + food + medication + sanitation, data=zombies, family = binomial(logit))
summary(mod.saturated)

vif(mod.saturated) # No signs of multicollinearity
avPlots(mod.saturated) # Medication and sanitation show slight signs of neutrality

mod.reduced <- glm(zombie ~ age + rurality + food, data=zombies, family = binomial(logit))
summary(mod.reduced)

vif(mod.reduced) # No signs of multicollinearity
avPlots(mod.reduced) # Medication and rurality show no relationship when controlled for other vars

# 2. Compare models
anova(mod.saturated, mod.reduced)

AIC(mod.saturated,     
    mod.reduced) 

BIC(mod.saturated,
    mod.reduced) # Both the minimum AIC and BIC values appear alongside the
                 # full model that we tested above.

# 3. How sensitive / specific are our predictions ?
zombies$yhatZombie <- predict(mod.saturated, type = "response") # Predict

ggplot(data=zombies, aes(x = yhatZombie, fill=zombie)) + 
  geom_histogram() # Plot likelihood against zombie status. 
                   # See pretty good differentiation

zombies %>% 
  group_by(zombie) %>% 
  summarise(meanyhat = mean(yhatZombie)) # Mean likelihood by zombie status: Human = 0.0451, Zombie = 0.955
                                         # Too specific to rely on as likelihood threshold

threshold <- 0.25 # Likelihood threshold for predicting zombie status
zombies$predZombie <- ifelse(zombies$yhatZombie > threshold, 1, 0) 
zombies$trueZombie <- ifelse(zombies$zombie=="Zombie", 1, 0) 

ROC <- roc(predictor = zombies$predZombie, response = zombies$trueZombie) # How specific / sensitive ?
ROC  # AUC = 0.7515
plot(ROC) # Good

mean(zombies$predZombie == zombies$trueZombie) # 79.5% accuracy rate

# 4. Make out-of-sample prediction
newdata <- data.frame(age = c(71, 20), 
                      water.person = cd(5, 5),
                      food = c("Food", "Food"),
                      rurality = c("Suburban", "Rural"),
                      medication = c("Medication", "Medication"),
                      sanitation = c("Sanitation", "Sanitation")) # Create rando dataset

predict(mod.reduced, newdata, type="response") 


# Naive Bayes ------------------------------------------------------------------

mod.nb <- naive_bayes(zombie ~ age + rurality + food + medication, data = zombies)

newdata <- data.frame(age = c(71, 21),
                      food = factor(c("No food", "No food")),
                      rurality = factor(c("Rural", "Suburban")),
                      medication = factor(c("Medication", "No medication")))

levels(newdata$food) <- c("Food", "No food")
levels(newdata$rurality) <- c("Rural", "Suburban", "Urban")
levels(newdata$medication) <- c("Medication", "No medication")

predict(mod.nb, newdata)
