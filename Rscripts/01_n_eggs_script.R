### BIO201 Bean Beetle experiment
library(tidyverse)
theme_set(theme_bw())
egg_data <- readxl::read_excel("Data/eggCount.xlsx")

# Find the total number of eggs per treatment, and the mean number of eggs per bean
# Note, because these counts and we can't have 0.25 eggs, we round up the mean n_eggs on bean using the ceiling() function
eggSums <- egg_data %>% 
  mutate(n_eggs = n_eggs_on_bean * count ) %>% 
  group_by(sample_code) %>% 
  summarise(total_eggs = sum(n_eggs),
            max_eggs_on_bean = max(n_eggs_on_bean))

#### Calculate the median eggs per bean
calc_mean_eggs_per_bean <- function(x){
    y <- x$n_eggs_on_bean
    z  <- x$count
    return(ceiling(mean(rep(y, z))))
}  

mean_eggs_bean <- egg_data %>%
  group_by(sample_code) %>% 
  nest() %>% 
  mutate(mn_eggs_on_bean = map(data, calc_mean_eggs_per_bean)) %>% 
  select(-data) %>% 
  unnest(cols = c(mn_eggs_on_bean))
  
## Combine the two dataframes
eggSums <- eggSums %>% left_join(mean_eggs_bean)


# Then split up the sample code into the constituent bits of information
eggSums <- eggSums %>% 
  separate(sample_code, 
         into = c("group_number", "temperature", "temp_cabinent", "number_beans", "extra_treatment"), 
         sep = "_", fill = "right", remove = "FALSE") %>% 
  unite(col = "temp_treatment", temperature:temp_cabinent, remove = FALSE) %>% 
  mutate(n_females = 1, bean_type = "BEB") %>%
  mutate(n_females = replace(n_females, extra_treatment == "(2F)", 2)) %>% 
  mutate(bean_type = replace(bean_type, grep("m", extra_treatment, ignore.case= TRUE), "Mung")) 

# Then this allows you to calculate the plotting data at the correct treatment level according to the treatment
treatment_data <-eggSums %>% 
  group_by(temp_treatment, number_beans, n_females, bean_type) %>%
  summarise(mean_total_eggs = ceiling(mean(total_eggs)),
            mean_total_eggs_sd = sd(total_eggs),
            mean_eggs_on_bean = ceiling(mean(mn_eggs_on_bean)),
            mean_eggs_on_bean_sd = sd(mn_eggs_on_bean),
            max_eggs_on_bean = mean(max_eggs_on_bean),
            n = n()) %>% 
  # Split up the temp_treatment variable- this is useful for ploting later
  separate(temp_treatment, 
           into = c("temperature", "temp_cabinent"), 
           sep = "_", extra = "drop", remove = "FALSE")

# change the number_beans value from a "character" to a numeric column
treatment_data <- treatment_data %>%
  ungroup() %>% 
  mutate(number_beans = as.numeric(number_beans))

# save(treatment_data, file = "outputs/treatment_data.RData")

##### TOTAL NUMBER OF EGGS- BASIC (i.e. removing mung beans and extra females)

basicData <- treatment_data %>% 
  filter(n_females == 1) %>% 
  filter(bean_type == "BEB")


# Plot the total number of eggs 
total_eggs <- basicData %>% 
  ggplot(aes(x = number_beans, y = mean_total_eggs)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_total_eggs - (2* (mean_total_eggs_sd/sqrt(n))), ymax =  mean_total_eggs + (2*(mean_total_eggs_sd/sqrt(n)))), width = 2, alpha = 0.5)+
  facet_grid(.~temperature)

total_eggs

# So we are interested in fitting a model to explain this.
# One hypothesis would be a linear model

model_lm <- lm(mean_total_eggs ~ number_beans * temperature, data = basicData)
summary(model_lm)

total_eggs + geom_smooth(method = "lm")

# But!!! This is not really a very good model because we are modelling counts, 
# and counts can't have non-negative values

# Instead we fit a glm with a Poisson error distribution- also force it through the intercept
model_glm <- glm(mean_total_eggs ~ number_beans*temperature, 
                 family = poisson(link = "log"),
                 data = basicData)
summary(model_glm)

# Create new variable to predict values of the model on
toPredict <- data.frame(number_beans = rep(5:135, 2), 
                        temperature = c(rep("24", length(5:135)), rep("28", length(5:135))))

# Check what the inverse link function for the model is (we need this later for calculating the confidence intervals)
ilink <- family(model_glm)$linkinv
# Get the predictions of the model
model_glm_pred <- predict(model_glm, newdata = toPredict, se.fit = TRUE)
# Get critical value from t-distrbution to estimate confidence intervals
critVal <- qt(0.025, df = df.residual(model_glm), lower.tail = FALSE)
# Create the confidence intervals and back transform to response scale
predicted_data <- data.frame(number_beans = toPredict$number_beans,
                             temperature = toPredict$temperature,
                             mean_total_eggs = ilink(model_glm_pred$fit),
                             upper = ilink(model_glm_pred$fit + (critVal *model_glm_pred$se.fit)),
                             lower = ilink(model_glm_pred$fit - (critVal *model_glm_pred$se.fit)))

# Add these model predictions to the plot
total_eggs_plot_glm_1 <- total_eggs + 
  geom_line(data = predicted_data,col = "blue", size = 0.75) +
  geom_ribbon(data = predicted_data,
              aes(ymin = lower, ymax = upper),
              alpha = 0.1)

total_eggs_plot_glm_1

# We could try a more complicated model - e.g. a quadratic model
model_glm_2 <- glm(mean_total_eggs ~ number_beans + temperature + I(number_beans^2)  + number_beans: temperature + temperature: I(number_beans^2), 
                 family = poisson(link = "log"),
                 data = basicData)
summary(model_glm_2)

# Create new variable to predict values of the model on
toPredict <- data.frame(number_beans = rep(5:135, 2), 
                        temperature = c(rep("24", length(5:135)), rep("28", length(5:135))))

# Check what the inverse link function for the model is (we need this later for calculating the confidence intervals)
ilink <- family(model_glm_2)$linkinv
# Get the predictions of the model
model_glm_pred_2 <- predict(model_glm_2, newdata = toPredict, se.fit = TRUE)
# Get critical value from t-distrbution to estimate confidence intervals
critVal <- qt(0.025, df = df.residual(model_glm_2), lower.tail = FALSE)
# Create the confidence intervals and back transform to response scale
predicted_data_2 <- data.frame(number_beans = toPredict$number_beans,
                             temperature = toPredict$temperature,
                             mean_total_eggs = ilink(model_glm_pred_2$fit),
                             upper = ilink(model_glm_pred_2$fit + (critVal *model_glm_pred_2$se.fit)),
                             lower = ilink(model_glm_pred_2$fit - (critVal *model_glm_pred_2$se.fit)))

# Add these model predictions to the plot
total_eggs_plot_glm_2 <- total_eggs + 
  geom_line(data = predicted_data_2,col = "blue", size = 0.75) +
  geom_ribbon(data = predicted_data_2,
              aes(ymin = lower, ymax = upper),
              alpha = 0.1)
total_eggs_plot_glm_2


######### We could try to fit a similar model but instead testing for the mean number of eggs per bean


#  Plot the mean number of eggs per bean
mean_eggs_per_bean_plot<- basicData %>% 
  ggplot(aes(x = number_beans, y = mean_eggs_on_bean)) +
  geom_point() +
  facet_grid(.~temperature)

# A model to describe the shape of the relationship
model_glm_mean_eggs_per_bean <- glm(mean_eggs_on_bean ~   number_beans*temperature,  
                                    family = poisson(link = "log"), data= basicData)
summary(model_glm_mean_eggs_per_bean)


# Create new variable to predict values of the model on
toPredict <- data.frame(number_beans = rep(5:135, 2), 
                        temperature = c(rep("24", length(5:135)), rep("28", length(5:135))))

# Check what the inverse link function for the model is (we need this later for calculating the confidence intervals)
ilink <- family(model_glm_mean_eggs_per_bean)$linkinv
# Get the predictions of the model
model_glm_pred_3 <- predict(model_glm_mean_eggs_per_bean, newdata = toPredict, se.fit = TRUE)
# Get critical value from t-distrbution to estimate confidence intervals
critVal <- qt(0.025, df = df.residual(model_glm_mean_eggs_per_bean), lower.tail = FALSE)
# Create the confidence intervals and back transform to response scale
predicted_data_3 <- data.frame(number_beans = toPredict$number_beans,
                               temperature = toPredict$temperature,
                               mean_eggs_on_bean = ilink(model_glm_pred_3$fit),
                               upper = ilink(model_glm_pred_3$fit + (critVal *model_glm_pred_3$se.fit)),
                               lower = ilink(model_glm_pred_3$fit - (critVal *model_glm_pred_3$se.fit)))

# Add these model predictions to the plot
mean_eggs_per_bean_plot_glm <- mean_eggs_per_bean_plot + 
  geom_line(data = predicted_data_3,col = "blue", size = 0.75) +
  geom_ribbon(data = predicted_data_3,
              aes(ymin = lower, ymax = upper),
              alpha = 0.1)

mean_eggs_per_bean_plot_glm

##########
### Group 1 were not interested in temperature, but instead of the effects of the number of females
##########

grp1_data <- treatment_data %>% 
  filter(temperature == 28) %>% 
  filter(bean_type == "BEB")

# Can make similar plots, but facet_grid by n_females instead
mean_eggs_per_bean_plot_females<- grp1_data %>% 
  ggplot(aes(x = number_beans, y = mean_eggs_on_bean)) +
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
  ggplot(aes(x = number_beans, y = mean_eggs_on_bean, group = bean_type)) +
  geom_point(aes(color = bean_type)) %>% 
  facet_grid(.~ bean_type)

# Check with Group 5 if all the data are present..
