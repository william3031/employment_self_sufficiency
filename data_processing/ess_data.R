# ess_calculations
# Employment self-sufficency looks at what proportion of local jobs are filled by local residents
# data is from the 2011 and 2016 ABS Censuses
# ur from other states included as there is travel over the borders


# libraries ---------------------------------------------------------------

library(tidyverse)
library(janitor)


# load data ---------------------------------------------------------------

# 2011 data
vic_lgas_raw_11 <- read_csv("data_in/ur_vic_lgas_2011.csv", skip = 9) %>% #removes cols at the start
  clean_names() %>% 
  filter(!is.na(counting)) %>% # removes cols at the end
  rename(lga_pow = local_government_areas_2011_boundaries_pow,
         lga_ur = lga) %>% #rename lga
  select(-x5) %>% 
  mutate(year = 2011)

other_states_raw_11 <- read_csv("data_in/ur_other_states_2011.csv", skip = 9) %>% #removes cols at the start
  clean_names() %>% 
  filter(!is.na(counting)) %>% # removes cols at the end
  select(-x5) %>% # removes the last column
  rename(lga_pow = local_government_areas_2011_boundaries_pow,
         lga_ur = state) %>% 
  mutate(year = 2011)

# 2016
vic_lgas_raw_16 <- read_csv("data_in/ur_vic_lgas_2016.csv", skip = 9) %>% #removes cols at the start
  clean_names() %>% 
  filter(!is.na(counting)) %>% # removes cols at the end
  select(-x5) %>% 
  mutate(year = 2016)

other_states_raw_16 <- read_csv("data_in/ur_other_states_2016.csv", skip = 9) %>% #removes cols at the start
  clean_names() %>% 
  filter(!is.na(counting)) %>% # removes cols at the end
  select(-x5) %>% # removes the last column
  rename(lga_ur = state_ur) %>% 
  mutate(year = 2016)


# combine -----------------------------------------------------------------

data_merged <- bind_rows(vic_lgas_raw_11,
                      other_states_raw_11,
                      vic_lgas_raw_16,
                      other_states_raw_16) %>% 
  select(-counting)

# all jobs
total_jobs_data <- data_merged %>% 
  group_by(lga_pow, year) %>% 
  summarise(total_jobs = sum(count)) %>% 
  rename(lga = lga_pow)

# jobs that are in the same lga
local_jobs_data <- data_merged %>% 
  mutate(local_jobs = case_when(lga_ur == lga_pow ~ count,
                                TRUE ~ NA_real_)) %>% 
  filter(!is.na(local_jobs)) %>% 
  select(-count, -lga_pow) %>% 
  rename(lga = lga_ur)

# join and calculate
ess_data <- left_join(total_jobs_data, local_jobs_data, by = c("lga", "year")) %>% 
  mutate(ess = local_jobs/total_jobs) %>% 
  filter(!is.na(ess)) %>% 
  select(lga, total_jobs, year, local_jobs, ess) %>% 
  mutate(across(c("total_jobs", "local_jobs"), ~as.integer(.)),
         year = factor(year),lga = str_remove_all(lga, "[\\(].*[\\)]"),
         lga = str_trim(lga),
         ess = round(ess, 3))

# what is the highest?
ess_data %>% 
  arrange(desc(ess))


# export ------------------------------------------------------------------
# as an rds to save the structure
write_rds(ess_data, "./data_out/ess_data.rds")
