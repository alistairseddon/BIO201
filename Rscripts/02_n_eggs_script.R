library("tidyverse")
library("broom")
library("GGally")
theme_set(theme_bw())

#NB: throughout script- Changed name of variable to mean_eggs_per_bean

load("outputs/treatment_data.RData")

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
ggcoef(model_glm,exclude_intercept = TRUE)





#####################
######### Modelling the Mean eggs per bean
#####################

# We could try to fit a similar model but instead testing for the egg:bean ratio
#  Plot the mean number of eggs per bean
mean_eggs_per_bean_plot<- basicData %>% 
  ggplot(aes(x = number_beans, y = mean_eggs_per_bean )) +
  geom_point() +
  facet_grid(.~temperature)

mean_eggs_per_bean_plot

# A model to describe the shape of the relationship
model_glm_mean_eggs_per_bean <- glm(mean_eggs_per_bean ~   number_beans*temperature,  
                                    family = poisson(link = "log"), data= basicData)
model_glm_mean_eggs_per_bean_pred <- predict(model_glm_mean_eggs_per_bean, 
                                         newdata = toPredict, 
                                         type = "response")
# Store the results in a dataframe for plotting
predicted_data <- data.frame( number_beans = toPredict$number_beans,
                              temperature = toPredict$temperature,
                              mean_eggs_per_bean = model_glm_mean_eggs_per_bean_pred)


mean_eggs_per_bean_plot_glm <- mean_eggs_per_bean_plot +
  geom_line(data = predicted_data ,col = "blue", size = 0.75)
mean_eggs_per_bean_plot_glm

summary(model_glm_mean_eggs_per_bean)



##########
### Group 1 were not interested in temperature, but instead of the effects of the number of females
##########

#Filtering unnnecessary parameters
grp1_data <- treatment_data %>% 
  filter(temperature == 28) %>% 
  filter(bean_type == "BEB") %>% 
  mutate(n_females_factor="1 Female")%>%
  mutate(n_females_factor=replace(n_females_factor, n_females==2, "2 Females")) %>%
  as.data.frame()

#Plot
mean_eggs_per_bean_plot_females<- grp1_data %>% 
  ggplot(aes(x = number_beans, y = mean_eggs_per_bean)) +
  geom_point() +
  labs(y= "Mean eggs per bean", x = "Number of beans") + 
  ggtitle ("The affects of competition on egg laying") +
  theme(plot.title = element_text(size=13, face="bold", hjust = 0.5)) +
  facet_grid(.~n_females_factor)

#Predictions
model_glm_mean_eggs_per_bean <- glm(mean_eggs_per_bean ~ number_beans*n_females_factor, 
                                family = poisson(link = "log"), data= grp1_data)

toPredict <- data.frame(number_beans = rep(5:135, 2), n_females_factor = c(rep("1 Female", length(5:135)), rep("2 Females", length(5:135))))

model_glm_mean_eggs_per_bean_pred <- predict(model_glm_mean_eggs_per_bean, 
                                         newdata = toPredict, 
                                         type = "response")
#Stored results
predicted_data <- data.frame(number_beans = toPredict$number_beans,
                             n_females_factor = toPredict$n_females,
                             mean_eggs_per_bean = model_glm_mean_eggs_per_bean_pred)

# added predictions
mean_eggs_plot_glm <- mean_eggs_per_bean_plot_females + 
  geom_line(data = predicted_data, col = "blue", size = 0.75)
mean_eggs_plot_glm
summary(model_glm_mean_eggs_per_bean)

# visualise
ggcoef(model_glm_mean_eggs_per_bean,exclude_intercept = TRUE)



########
# Group 5 were only interested in the very high and very low treatments, and were not interested in temperature
########
mungBean <- treatment_data %>% 
  filter(number_beans != 15) %>% 
  filter(number_beans != 45) %>% 
  filter(temperature == 28) %>% 
  filter(n_females == 1) %>% 
  mutate(number_beans_factor = as_factor(number_beans))

# Now we can plot these data-
mung_bean_total_eggs_plot<- mungBean %>% 
  ggplot(aes(x = bean_type, y = mean_total_eggs)) +
  geom_point(aes(color = bean_type)) +
  facet_grid(.~ number_beans_factor) +
  theme(legend.position="none")

# Do the modelling
beanType_m1 <- glm(mean_total_eggs ~ bean_type * number_beans, 
                   data = mungBean, family = poisson(link = "log"))
summary(beanType_m1)

beanType_m2 <- glm(mean_total_eggs ~ bean_type, 
                   data = mungBean, family = poisson(link = "log"))
summary(beanType_m2)

