library(tidyverse)
theme_set(theme_bw())
emergence_data <- readxl::read_excel("Data/survivalCount.xlsx")
View(emergence_data)
# Align emergence data with egg data. NB Not calculated sex ratio yet
emergenceSums <- emergence_data %>% 
  group_by(sample_code) %>% 
  summarise(total_emergence = sum(count)) %>% 
  left_join(eggSums, by = "sample_code") %>%    # join up with the egg count data (egg_sums)
  mutate(prop_emergence = total_emergence/total_eggs)

# Calculate the means at the correct replica unit
emergence_data <-emergenceSums %>% 
  group_by(temp_treatment, number_beans, n_females, bean_type) %>%
  summarise(mean_total_eggs = ceiling(mean(total_eggs)),
            mean_total_eggs_sd = sd(total_eggs),
            mean_eggs_on_bean = ceiling(mean(mn_eggs_on_bean)),
            mean_eggs_on_bean_sd = sd(mn_eggs_on_bean),
            max_eggs_on_bean = mean(max_eggs_on_bean),
            n = n(),
            mean_emergence_rate = mean(prop_emergence, na.rm = TRUE),
            mean_total_emergence = ceiling(mean(total_emergence, na.rm = TRUE))) %>% 
  # Split up the temp_treatment variable- this is useful for ploting later
  separate(temp_treatment, 
           into = c("temperature", "temp_cabinent"), 
           sep = "_", extra = "drop", remove = "FALSE")




# Now we can plot/ model the emergence rate

basic_emergence_data <- emergence_data %>% 
  filter(n_females == 1) %>% 
  filter(bean_type == "BEB") %>% 
  ungroup %>% 
  mutate(number_beans = as.numeric(number_beans)) 

basic_emergence_data <- basic_emergence_data[1:7,]

emergence_rate_plot <- basic_emergence_data %>% 
  ggplot(aes(x = number_beans, y = mean_emergence_rate)) +
  geom_point() +
  facet_grid(.~temperature)
emergence_rate_plot

# Fit a model
model_emer_rate_glm <- glm(cbind(mean_total_emergence, mean_total_eggs-mean_total_emergence) ~ number_beans, 
                 family = binomial(link = "logit"),
                 data = basic_emergence_data)
summary(model_emer_rate_glm)

# Get prediction on the model
toPredict <- data.frame(number_beans = 5:135)

# Check what the inverse link function for the model is (we need this later for calculating the confidence intervals)
ilink <- family(model_emer_rate_glm)$linkinv
# Get the predictions of the model
model_emer_rate_pred <- predict(model_emer_rate_glm, newdata = toPredict, se.fit = TRUE)
# Get critical value from t-distrbution to estimate confidence intervals
critVal <- qt(0.025, df = df.residual(model_emer_rate_glm), lower.tail = FALSE)
# Create the confidence intervals and back transform to response scale
predicted_data <- data.frame(number_beans = toPredict$number_beans,
                             mean_emergence_rate = ilink(model_emer_rate_pred$fit),
                             upper = ilink(model_emer_rate_pred$fit + (critVal *model_emer_rate_pred$se.fit)),
                             lower = ilink(model_emer_rate_pred$fit - (critVal *model_emer_rate_pred$se.fit)))

# Add these model predictions to the plot
emergence_rate_plot_glm <- emergence_rate_plot + 
  geom_line(data = predicted_data,col = "blue", size = 0.75) +
  geom_ribbon(data = predicted_data,
              aes(ymin = lower, ymax = upper),
              alpha = 0.1)
emergence_rate_plot_glm

# But this isn't a very nice model and doesn't describe it very well

# We can try to fit an asymtopic model to this instead
asymModel <- nls(mean_emergence_rate ~ SSasymp(number_beans, Asym, R0, lrc), data= basic_emergence_data)
summary(asymModel)

emergence_total_plot <- basic_emergence_data %>% 
  ggplot(aes(x = number_beans, y = mean_total_emergence)) +
  geom_point() +
  facet_grid(.~temperature)
emergence_total_plot

toPredict <- data.frame(number_beans = 5:135)

# Get the predictions of the model
model_emer_rate_pred_asym <- predict(asymModel, newdata = toPredict, se.fit = TRUE) # se.fit is ignored by function so can't estimate confidence intervals

predicted_data <- data.frame(number_beans = toPredict$number_beans,
                             mean_emergence_rate = model_emer_rate_pred_asym[1:131])

# Add these model predictions to the plot
emergence_rate_plot_asym <- emergence_rate_plot + 
  geom_line(data = predicted_data,col = "blue", size = 0.75)
emergence_rate_plot_asym


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
