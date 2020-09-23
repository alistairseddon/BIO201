library(tidyverse)
theme_set(theme_bw())
load("outputs/emergence_data.RData")

# Now we can plot/ model the emergence rate

basic_emergence_data <- emergence_data %>% 
  filter(n_females == 1) %>% 
  filter(bean_type == "BEB") %>% 
  ungroup %>% 
  mutate(number_beans = as.numeric(number_beans)) 

emergence_rate_plot <- basic_emergence_data %>% 
  ggplot(aes(x = number_beans, y = mean_emergence_rate)) +
  geom_point() +
  facet_grid(.~temperature)
emergence_rate_plot

# Fit a model
model_emer_rate_glm <- glm(cbind(mean_total_emergence, mean_total_eggs-mean_total_emergence) ~ number_beans, 
                 family = binomial(link = "logit"),
                 data = basic_emergence_data)


# Get prediction on the model
toPredict <- data.frame(number_beans = 5:135)
model_emer_rate_pred <- predict(model_emer_rate_glm, 
                                newdata = toPredict, 
                                type= "response")
predicted_data <- data.frame(number_beans = toPredict$number_beans,
                             mean_emergence_rate = model_emer_rate_pred)

# Add these model predictions to the plot
emergence_rate_plot_glm <- emergence_rate_plot + 
  geom_line(data = predicted_data,col = "blue", size = 0.75)
emergence_rate_plot_glm
summary(model_emer_rate_glm)


# But this isn't a very nice model- I think there might be a better one:

# See here: https://www.statforbiology.com/nonlinearregression/usefulequations

# We can try to fit an asymtopic model to this instead
asymModel <- nls(mean_emergence_rate ~ SSasymp(number_beans, Asym, R0, lrc), data= basic_emergence_data)
summary(asymModel)

model_emer_rate_pred_asym <- predict(asymModel, newdata = toPredict, se.fit = TRUE) # se.fit is ignored by function so can't estimate confidence intervals

predicted_data <- data.frame(number_beans = toPredict$number_beans,
                             mean_emergence_rate = model_emer_rate_pred_asym[1:131])

# Add these model predictions to the plot
emergence_rate_plot_asym <- emergence_rate_plot + 
  geom_line(data = predicted_data,col = "blue", size = 0.75)
emergence_rate_plot_asym

######
# What about we use the mean number of eggs per bean as a proxy for competition strength?

emergence_competition_plot <- basic_emergence_data %>% 
  ggplot(aes(x = mean_egg_bean_ratio, y = mean_emergence_rate)) +
  geom_point() +
  facet_grid(.~temperature)
emergence_competition_plot

# We can try to fit an asymtopic model to this instead
comp_model <- glm(cbind(mean_total_emergence, mean_total_eggs-mean_total_emergence) ~ mean_egg_bean_ratio, 
    family = binomial(link = "logit"),
    data = basic_emergence_data)

# Get prediction on the model
toPredict <- data.frame(mean_egg_bean_ratio = 1:12)
model_comp_pred <- predict(comp_model, 
                                newdata = toPredict, 
                                type= "response")
predicted_data <- data.frame(mean_egg_bean_ratio = toPredict$mean_egg_bean_ratio,
                             mean_emergence_rate = model_comp_pred)



# Add these model predictions to the plot
emergence_competition_plot_glm <- emergence_competition_plot + 
  geom_line(data = predicted_data,col = "blue", size = 0.75)
summary(comp_model)


# ##### Can also investigate whether the number of females can influence the emergence rate (Group 1)
# gp1_data <- emergence_data %>% 
#   filter(bean_type == "BEB") %>% 
#   ungroup %>% 
#   mutate(number_beans = as.numeric(number_beans)) 
# 
# basic_emergence_data <- basic_emergence_data[1:7,]
# 
# gp_1_emergence_rate_plot <- gp1_data %>% 
#   ggplot(aes(x = number_beans, y = mean_emergence_rate)) +
#   geom_point() +
#   facet_grid(.~n_females)
# gp_1_emergence_rate_plot
