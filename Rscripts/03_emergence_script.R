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
model_emer_rate_glm <- glm(cbind(mean_total_emergence, mean_total_eggs-mean_total_emergence) ~ number_beans*temperature, 
                 family = binomial(link = "logit"),
                 data = basic_emergence_data)


# Get prediction on the model
toPredict <- data.frame(number_beans = rep(5:135, 2), 
                        temperature = c(rep("24", length(5:135)), rep("28", length(5:135))))
model_emer_rate_pred <- predict(model_emer_rate_glm, 
                                newdata = toPredict, 
                                type= "response")
predicted_data <- data.frame(number_beans = toPredict$number_beans,
                             temperature = toPredict$temperature,
                             mean_emergence_rate = model_emer_rate_pred)




# Add these model predictions to the plot
emergence_rate_plot_glm <- emergence_rate_plot + 
  geom_line(data = predicted_data,col = "blue", size = 0.75)
emergence_rate_plot_glm
summary(model_emer_rate_glm)


# But this isn't a very nice model- I think there might be a better one:
# See here: https://www.statforbiology.com/nonlinearregression/usefulequations

# We can try to fit an asymtopic model to this instead, but need to do this for two separate models
basic_emergence_data_24 <- basic_emergence_data %>% 
  filter(temperature == 24)
toPredict <- data.frame(number_beans = 5:135)

asymModel_24 <- nls(mean_emergence_rate ~ SSasymp(number_beans, Asym, R0, lrc), data= basic_emergence_data_24)
model_emer_rate_pred_asym_24 <- predict(asymModel_24, newdata = toPredict, se.fit = TRUE) # se.fit is ignored by function so can't estimate confidence intervals
predicted_data_24 <- data.frame(number_beans = toPredict$number_beans,
                             mean_emergence_rate = model_emer_rate_pred_asym_24[1:131],
                             temperature = "24")

# Repeat for 28 
basic_emergence_data_28 <- basic_emergence_data %>% 
  filter(temperature == 28)
toPredict <- data.frame(number_beans = 5:135)
asymModel_28 <- nls(mean_emergence_rate ~ SSasymp(number_beans, Asym, R0, lrc), data= basic_emergence_data_28)
model_emer_rate_pred_asym_28 <- predict(asymModel_28, newdata = toPredict, se.fit = TRUE) # se.fit is ignored by function so can't estimate confidence intervals
predicted_data_28 <- data.frame(number_beans = toPredict$number_beans,
                                mean_emergence_rate = model_emer_rate_pred_asym_28[1:131],
                                temperature = "28")

predicted_data_emergence <- bind_rows(predicted_data_28,predicted_data_24 )

# Add these model predictions to the plot
emergence_rate_plot_asym <- emergence_rate_plot + 
  geom_line(data = predicted_data_emergence,col = "blue", size = 0.75)
emergence_rate_plot_asym

summary(asymModel_28)
summary(asymModel_24)


######
# What about we use the mean number of eggs per bean as a proxy for competition strength?

emergence_competition_plot <- basic_emergence_data %>% 
  ggplot(aes(x = mean_eggs_per_bean, y = mean_emergence_rate)) +
  geom_point() +
  facet_grid(.~temperature)
emergence_competition_plot

# We fit a glm to this
comp_model <- glm(cbind(mean_total_emergence, mean_total_eggs-mean_total_emergence) ~ mean_eggs_per_bean*temperature, 
    family = binomial(link = "logit"),
    data = basic_emergence_data)

# Get prediction on the model
toPredict <- data.frame(mean_eggs_per_bean = rep(1:12, 2), 
                        temperature = c(rep("24", length(1:12)), rep("28", length(1:12))))
model_comp_pred <- predict(comp_model, 
                                newdata = toPredict, 
                                type= "response")
predicted_data <- data.frame(mean_eggs_per_bean = toPredict$mean_eggs_per_bean,
                             temperature = toPredict$temperature,
                             mean_emergence_rate = model_comp_pred)


# Add these model predictions to the plot
emergence_competition_plot_glm <- emergence_competition_plot + 
  geom_line(data = predicted_data,col = "blue", size = 0.75)
emergence_competition_plot_glm
summary(comp_model)


########
### Group 3: Modelling total number of adults emerged
basic_emergence_data_24 <- basic_emergence_data %>% 
  filter(temperature == 24)
toPredict <- data.frame(number_beans = 5:135)

asymModel_24 <- nls(mean_total_emergence ~ SSasymp(number_beans, Asym, R0, lrc), data= basic_emergence_data_24)
model_emer_rate_pred_asym_24 <- predict(asymModel_24, newdata = toPredict, se.fit = TRUE) # se.fit is ignored by function so can't estimate confidence intervals
predicted_data_24 <- data.frame(number_beans = toPredict$number_beans,
                                mean_total_emergence = model_emer_rate_pred_asym_24[1:131],
                                temperature = "24")

# Repeat for 28 
basic_emergence_data_28 <- basic_emergence_data %>% 
  filter(temperature == 28)
toPredict <- data.frame(number_beans = 5:135)
asymModel_28 <- nls(mean_total_emergence ~ SSasymp(number_beans, Asym, R0, lrc), data= basic_emergence_data_28)
model_emer_rate_pred_asym_28 <- predict(asymModel_28, newdata = toPredict, se.fit = TRUE) # se.fit is ignored by function so can't estimate confidence intervals
predicted_data_28 <- data.frame(number_beans = toPredict$number_beans,
                                mean_total_emergence = model_emer_rate_pred_asym_28[1:131],
                                temperature = "28")

predicted_data_total_emergence <- bind_rows(predicted_data_28,predicted_data_24 )

# Add these model predictions to the plot
total_emergence_plot_asym <- basic_emergence_data %>% 
  ggplot(aes(x = number_beans, y = mean_total_emergence)) +
  geom_point() +
  facet_grid(.~temperature) +
  geom_line(data = predicted_data_total_emergence,col = "blue", size = 0.75)
total_emergence_plot_asym

summary(asymModel_28)
summary(asymModel_24)


# Could probably just take the data from the 45 and 135 treatments, pool and run a ttest
tTest_data <- basic_emergence_data %>% 
#  filter(number_beans >= 45)
  filter(number_beans == 135)

tTest_glm <- glm(mean_total_emergence ~ temperature, data =tTest_data, 
             family = poisson(link = "log"))
summary(tTest_glm)



#####
#Group 5: emergence data analysis
GP_5_emergence <- emergence_data %>% 
  filter(number_beans != 15) %>% 
  filter(number_beans != 45) %>% 
  filter(temperature == 28) %>% 
  filter(n_females == 1) %>% 
  mutate(number_beans_factor = as.factor(number_beans))


GP_5_emergence %>% 
  ggplot(aes(x = bean_type, y = mean_emergence_rate)) +
  geom_point(aes(color = bean_type)) + 
  facet_grid(.~ number_beans_factor) +
  theme(legend.position="None")


modelGP5 <- glm(cbind(mean_total_emergence, mean_total_eggs-mean_total_emergence) ~ bean_type*number_beans_factor, 
                data = GP_5_emergence,
                family = binomial(link = "logit"))
summary(modelGP5)


GP_5_emergence %>% 
  ggplot(aes(x = bean_type, y = mean_total_emergence)) +
  geom_point(aes(color = bean_type)) + 
  facet_grid(.~ number_beans_factor) +
  theme(legend.position="None")

modelGP5_total_emergence <- glm(mean_total_emergence ~ bean_type*number_beans_factor, 
                data = GP_5_emergence,
                family = poisson(link = "log"))
summary(modelGP5_total_emergence)


eggSums %>%
  group_by(temperature, temp_treatment, number_beans) %>% 
  summarise(mean_eggs = mean(total_eggs)) %>% 
  ungroup() %>% 
  group_by(temperature, number_beans) %>% 
  summarise(mean_eggs = mean(mean_eggs))



### Group 1

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
model_glm_mean_eggs_per_bean <- glm(mean_eggs_per_bean ~   number_beans*n_females_factor,  
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



