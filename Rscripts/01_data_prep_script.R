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
            max_eggs_on_bean = max(n_eggs_on_bean),
            egg_bean_ratio = sum(n_eggs)/ sum(count))

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
            mean_egg_bean_ratio = ceiling(mean(egg_bean_ratio)),
            max_egg_bean_ratio = ceiling(mean(egg_bean_ratio)),
            n = n()) %>% 
  # Split up the temp_treatment variable- this is useful for ploting later
  separate(temp_treatment, 
           into = c("temperature", "temp_cabinent"), 
           sep = "_", extra = "drop", remove = "FALSE")

# change the number_beans value from a "character" to a numeric column
treatment_data <- treatment_data %>%
  ungroup() %>% 
  mutate(number_beans = as.numeric(number_beans))

save(treatment_data, file = "outputs/treatment_data.RData")


######## We can also process the emergence data

emergence_data <- readxl::read_excel("Data/survivalCount.xlsx") %>% 
  filter(sample_code != "G6_28_B_135")

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
            mean_egg_bean_ratio = ceiling(mean(egg_bean_ratio)),
            max_egg_bean_ratio = ceiling(mean(egg_bean_ratio)),
            mean_emergence_rate = mean(prop_emergence, na.rm = TRUE),
            mean_total_emergence = ceiling(mean(total_emergence, na.rm = TRUE))) %>% 
  # Split up the temp_treatment variable- this is useful for ploting later
  separate(temp_treatment, 
           into = c("temperature", "temp_cabinent"), 
           sep = "_", extra = "drop", remove = "FALSE")

# change the number_beans value from a "character" to a numeric column
treatment_data <- treatment_data %>%
  ungroup() %>% 
  mutate(number_beans = as.numeric(number_beans))

save(emergence_data, file = "outputs/emergence_data.RData")

