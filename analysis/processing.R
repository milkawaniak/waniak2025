library(tidyverse)
#add
#1. attention checks trial_index == 277 (i1) and 500 (e0) response column
#2 text questions
# Path to your folder
path <- "/Users/milka/Documents/GitHub/psyc 251/waniak2025/data/raw/pilot B"

# Read all CSV files from the folder
files <- list.files(path, pattern = "\\.csv$", full.names = TRUE)

# Combine all files into one dataframe
df <- files %>%
  map_df(~ {
    file_name <- basename(.x)
    participant_id <-  tools::file_path_sans_ext(file_name) 
    
    read_csv(.x, show_col_types = FALSE) %>%  
      mutate(participant_id = participant_id) %>%
      select(participant_id, any_of(c("trial_index", "response", "phase", "valence", "past_response_label")))
  })

additional_responses <- df %>%
  filter(trial_index %in% c(277, 500, 501, 502, 503, 504, 505, 506, 507)) %>%
  select(participant_id, trial_index, response) %>%
  mutate(
    response_var = case_when(
      trial_index == 277 ~ "attention1",
      trial_index == 500 ~ "attention2",
      trial_index == 501 ~ "slider1",
      trial_index == 502 ~ "slider2",
      trial_index == 503 ~ "response1",
      trial_index == 504 ~ "response2",
      trial_index == 505 ~ "response3",
      trial_index == 506 ~ "response4",
      trial_index == 507 ~ "demographics"
    )
  ) %>%
  select(participant_id, response_var, response) %>%
  pivot_wider(
    names_from = response_var,
    values_from = response
  )

# Ensure phase is character
df <- df %>% mutate(phase = as.character(phase))

# Assign perspective based on trial_index FIRST (before grouping)
df <- df %>%
  mutate(
    perspective = case_when(
      trial_index >= 70 & trial_index <= 275 ~ "first_person",
      trial_index > 282 ~ "third_person",
      TRUE ~ NA_character_
    )
  ) %>%
  drop_na(perspective)  # remove test trials

# Create trial_id: group consecutive rows (every ~7 rows is one trial)
df <- df %>%
  group_by(participant_id) %>%
  mutate(
    is_new_trial = !is.na(valence),
    trial_id_within = cumsum(is_new_trial)  # trial number within participant
  ) %>%
  ungroup() %>%
  filter(trial_id_within > 0) %>%
  mutate(trial_id = paste(participant_id, trial_id_within, sep = "_"))  # unique ID

# Now collapse by trial_id
df_trials <- df %>%
  group_by(participant_id, trial_id, perspective) %>%
  summarize(
    trial_index = first(trial_index),
    prime = first(valence[!is.na(valence)]),
    choice_raw = case_when(
      first(perspective) == "first_person" ~ first(response[response %in% c("e","i")]),
      first(perspective) == "third_person" ~ tolower(first(past_response_label[!is.na(past_response_label)]))
    ),
    influenced = first(response[response %in% c("0","1")]),
    .groups = "drop"
  ) %>%
  # Recode choice and influenced
  mutate(
    choice = case_when(
      perspective == "first_person" & choice_raw == "e" ~ "unpleasant",
      perspective == "first_person" & choice_raw == "i" ~ "pleasant",
      perspective == "third_person" ~ choice_raw,
      TRUE ~ NA_character_
    ),
    influenced = case_when(
      influenced == "0" ~ "influenced",
      influenced == "1" ~ "not_influenced",
      TRUE ~ NA_character_
    )
  ) %>%
  select(-choice_raw)

# Renumber trials within each perspective
df_trials <- df_trials %>%
  group_by(participant_id, perspective) %>%
  mutate(trial_num = row_number()) %>%
  ungroup()

# Convert to WIDE format - one row per participant
final_df_wide <- df_trials %>%
  select(participant_id, perspective, trial_num, prime, choice, influenced) %>%
  pivot_longer(
    cols = c(prime, choice, influenced),
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(col_name = paste(perspective, variable, trial_num, sep = "_")) %>%
  select(participant_id, col_name, value) %>%
  pivot_wider(
    names_from = col_name,
    values_from = value
  )

final_df_wide <- final_df_wide %>%
  select(
    participant_id,
    # First person trials in order
    starts_with("first_person_prime_") %>% sort(),
    starts_with("first_person_choice_") %>% sort(),
    starts_with("first_person_influenced_") %>% sort(),
    # Third person trials in order
    starts_with("third_person_prime_") %>% sort(),
    starts_with("third_person_choice_") %>% sort(),
    starts_with("third_person_influenced_") %>% sort()
  )

final_df_wide <- final_df_wide %>%
  left_join(additional_responses, by = "participant_id") %>%
  select(participant_id, attention1, attention2, slider1, slider2, 
         response1, response2, response3, response4, demographics, 
         everything())  # Put all additional responses at the front


write_csv(final_df_wide, file.path('/Users/milka/Documents/GitHub/psyc 251/waniak2025/data/processed/pilot B', "data.csv"))

