# Employment self-sufficency looks at what proportion of local jobs are filled by local residents
# data is from the 2016 ABS Census
# ur from other states included as there is travel over the borders

#load_data
pacman::p_load(tidyverse, janitor)

#raw data
vic_lgas_raw <- read_csv("data_in/ur_vic_lgas_2016.csv", skip = 9) %>% #removes cols at the start
  clean_names() %>% 
  filter(!is.na(counting)) %>% # removes cols at the end
  select(-x5) # removes the last column

other_states_raw <- read_csv("data_in/ur_other_states_2016.csv", skip = 9) %>% #removes cols at the start
  clean_names() %>% 
  filter(!is.na(counting)) %>% # removes cols at the end
  select(-x5) %>% # removes the last column
  rename(lga_ur = state_ur) # renamed to match

#merged
data_merged <- bind_rows(vic_lgas_raw, other_states_raw) %>% 
  select(-counting)

total_jobs_data <- data_merged %>% 
  group_by(lga_pow) %>% 
  summarise(total_jobs = sum(count)) %>% 
  rename(lga = lga_pow)


local_jobs_data <- data_merged %>% 
  mutate(local_jobs = case_when(lga_ur == lga_pow ~ count,
                                TRUE ~ NA_real_)) %>% 
  filter(!is.na(local_jobs)) %>% 
  select(-count, -lga_pow) %>% 
  rename(lga = lga_ur)

# joined
ess_data <- left_join(total_jobs_data, local_jobs_data) %>% 
  mutate(ess = local_jobs/total_jobs) %>% 
  filter(!is.na(ess))

# what is the highest?
ess_data %>% 
  arrange(desc(ess))

#export
write_csv(ess_data, "data_out/ess_data_2016_census.csv")
  
