### BIO201 Bean Beetle experiment

library(tidyverse)
egg_data <- readxl::read_excel("Data/eggCount.xlsx")
  
# remove G5 mung_bean treatment
egg_data <- egg_data %>% filter(bean_type == "BEB")

# Find the total number of eggs per treatment, and the mean number of eggs per bean
eggSums <- egg_data %>% 
  mutate(n_eggs = n_eggs_on_bean * count ) %>% 
  group_by(sample_code) %>% 
  summarise(total_eggs = sum(n_eggs),
            mean_eggs_on_bean = mean(n_eggs_on_bean),
            max_eggs_on_bean = max(n_eggs_on_bean))

# Then split up the sample code into the constituent bits of information
eggSums <- eggSums %>% 
  separate(sample_code, 
         into = c("group_number", "temperature", "temp_cabinent", "number_beans"), 
         sep = "_", extra = "drop", remove = "FALSE") %>% 
  unite(col = "temp_treatment", temperature:temp_cabinent, remove = FALSE)


# Then this allows you to calculate the plotting data at the correct treatment level according to the treatment
treatment_data <-eggSums %>% 
  group_by(temp_treatment, number_beans) %>%
  summarise(mean_total_eggs = mean(total_eggs),
            mean_eggs_on_bean = mean(mean_eggs_on_bean),
            max_eggs_on_bean = mean(max_eggs_on_bean)) %>% 
  # Split up the temp_treatment variable- this is useful for ploting later
  separate(temp_treatment, 
           into = c("temperature", "temp_cabinent"), 
           sep = "_", extra = "drop", remove = "FALSE")
  
#  Plot the mean numner of eggs per bean
mean_eggs_per_bean<- treatment_data %>% 
  ggplot(aes(x = number_beans, y = mean_eggs_on_bean)) +
  geom_point() +
  facet_grid(.~temperature)

# Plot the maximum number of eggs per bean
max_eggs_per_bean <- treatment_data %>% 
  ggplot(aes(x = number_beans, y = max_eggs_on_bean)) +
  geom_point() +
  facet_grid(.~temperature)

# Plot the toal number of eggs 
total_eggs <- treatment_data %>% 
  ggplot(aes(x = number_beans, y = mean_total_eggs)) +
  geom_point() +
  facet_grid(.~temperature)

# You can play around with these data/ plots yourself now. What could we do to improve these plots?
  
