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
            egg_bean_ratio = ceiling(sum(n_eggs)/ sum(count)))

#### Calculate the mean eggs per bean
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
plot(eggSums$egg_bean_ratio, eggSums$mn_eggs_on_bean)

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
            # mean_total_eggs_sd = sd(total_eggs),
            mean_eggs_on_bean = ceiling(mean(mn_eggs_on_bean)),
           # mean_eggs_on_bean_sd = sd(mn_eggs_on_bean),
            max_eggs_on_bean = ceiling(mean(max_eggs_on_bean)),
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










