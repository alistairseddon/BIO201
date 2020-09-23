library("tidyverse")
library("broom")
library("GGally")


load("outputs/treatment_data.RData")
treatment_data

##### TOTAL NUMBER OF EGGS- BASIC (i.e. removing mung beans and extra females)
basicData <- treatment_data %>% 
  filter(n_females == 1) %>% 
  filter(bean_type == "BEB")


# Plot the total number of eggs 
total_eggs <- basicData %>% 
  ggplot(aes(x = number_beans, y = mean_total_eggs)) +
  geom_point() +
  facet_grid(.~temperature)

total_eggs

# So we are interested in fitting a model to explain this.
# One hypothesis would be a linear model

model_lm <- lm(mean_total_eggs ~ number_beans * temperature, data = basicData)
total_eggs + geom_smooth(method = "lm", se = FALSE)
summary(model_lm)

# But!!! This is not really a very good model because we are modelling counts, 
# and counts can't have non-negative values

# Instead we fit a glm with a Poisson error distribution
model_glm <- glm(mean_total_eggs ~ number_beans*temperature, 
                 family = poisson(link = "log"),
                 data = basicData)

# Get predictions of the model for different numbers of beans
toPredict <- data.frame(number_beans = rep(5:135, 2), 
                         temperature = c(rep("24", length(5:135)), rep("28", length(5:135))))
model_glm_pred <- predict(model_glm, newdata = toPredict, type = "response")
# Store the results in a dataframe for plotting
predicted_data <- data.frame( number_beans = toPredict$number_beans,
                              temperature = toPredict$temperature,
                              mean_total_eggs = model_glm_pred)

# Add these model predictions to the plot
total_eggs_plot_glm <- total_eggs + 
  geom_line(data = predicted_data,col = "blue", size = 0.75)
total_eggs_plot_glm
summary(model_glm)

# Can use the ggcoef function in GGally to visualise these results
ggcoef(model_glm)





#####################
######### Modelling the Egg-bean Ratio
#####################

# We could try to fit a similar model but instead testing for the egg:bean ratio
#  Plot the mean number of eggs per bean
mean_egg_bean_ratio_plot<- basicData %>% 
  ggplot(aes(x = number_beans, y = mean_egg_bean_ratio )) +
  geom_point() +
  facet_grid(.~temperature)

mean_egg_bean_ratio_plot

# A model to describe the shape of the relationship
model_glm_egg_bean_ratio <- glm(mean_egg_bean_ratio ~   number_beans*temperature,  
                                    family = poisson(link = "log"), data= basicData)
model_glm_egg_bean_ratio_pred <- predict(model_glm_egg_bean_ratio, 
                                         newdata = toPredict, 
                                         type = "response")
# Store the results in a dataframe for plotting
predicted_data <- data.frame( number_beans = toPredict$number_beans,
                              temperature = toPredict$temperature,
                              mean_egg_bean_ratio = model_glm_egg_bean_ratio_pred)


mean_egg_bean_ratio_plot_glm <- mean_egg_bean_ratio_plot +
  geom_line(data = predicted_data ,col = "blue", size = 0.75)
mean_egg_bean_ratio_plot_glm

summary(model_glm_egg_bean_ratio)



##########
### Group 1 were not interested in temperature, but instead of the effects of the number of females
##########

grp1_data <- treatment_data %>% 
  filter(temperature == 28) %>% 
  filter(bean_type == "BEB")

# Can make similar plots, but facet_grid by n_females instead
mean_eggs_per_bean_plot_females<- grp1_data %>% 
  ggplot(aes(x = number_beans, y = mean_egg_bean_ratio)) +
  geom_point() +
  facet_grid(.~n_females)

total_eggs_per_bean_plot_females<- grp1_data %>% 
  ggplot(aes(x = number_beans, y = mean_total_eggs)) +
  geom_point() +
  facet_grid(.~n_females)


## You could try to fit similar models to the models above





########
# Group 5 were only interested in the very high and very low treatments, and were not interested in temperature
########
mungBean <- treatment_data %>% 
  filter(number_beans != 15) %>% 
  filter(number_beans != 45) %>% 
  filter(temperature == 28) %>% 
  filter(n_females == 1)

# Now we can plot these data-
mung_bean_eggs_on_bean_plot<- mungBean %>% 
  ggplot(aes(x = number_beans, y = mean_egg_bean_ratio, group = bean_type)) +
  geom_point(aes(color = bean_type)) %>% 
  facet_grid(.~ bean_type)

# Check with Group 5 if all the data are present..
