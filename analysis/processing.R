library(tidyverse)

# Path to your folder
path <- "/Users/milka/Documents/GitHub/psyc 251/waniak2025/data/raw/pilot"

# Read all CSV files from the folder
files <- list.files(path, pattern = "\\.csv$", full.names = TRUE)

# Combine all files into one dataframe
df <- files %>%
  map_df(~ {
    file_name <- basename(.x)
    participant_id <- str_extract(file_name, "(?<=_)\\w+(?=\\.csv)")
    
    read_csv(.x, skip = 1, show_col_types = FALSE) %>%  # skip first row (metadata)
      mutate(participant_id = participant_id) %>%
      select(participant_id, any_of(c("trial_index", "response", "phase", "valence", "past_response_label")))
  })

# Ensure phase is character
df <- df %>% mutate(phase = as.character(phase))

# Assign perspective based on trial_index FIRST (before grouping)
df <- df %>%
  mutate(
    perspective = case_when(
      trial_index >= 68 & trial_index <= 277 ~ "first_person",
      trial_index > 277 ~ "third_person",
      TRUE ~ NA_character_
    )
  ) %>%
  drop_na(perspective)  # remove test trials

# Create trial_id: group consecutive rows (every ~7 rows is one trial)
df <- df %>%
  group_by(participant_id) %>%
  mutate(
    is_new_trial = !is.na(valence),  # TRUE when we see a prime
    trial_id = cumsum(is_new_trial)   # increment trial counter
  ) %>%
  filter(trial_id > 0) %>%  # Remove rows before first prime
  ungroup()

# Now collapse by trial_id
df_trials <- df %>%
  group_by(participant_id, trial_id, perspective) %>%
  summarize(
    trial_index = first(trial_index),
    prime = first(valence[!is.na(valence)]),
    choice = case_when(
      perspective == "first_person" ~ first(response[response %in% c("e","i")]),
      perspective == "third_person" ~ tolower(first(past_response_label[!is.na(past_response_label)]))
    ),
    influenced = first(response[response %in% c("0","1")]),
    .groups = "drop"
  ) %>%
  # Recode choice and influenced
  mutate(
    choice = case_when(
      perspective == "first_person" & choice == "e" ~ "unpleasant",
      perspective == "first_person" & choice == "i" ~ "pleasant",
      perspective == "third_person" ~ choice,
      TRUE ~ NA_character_
    ),
    influenced = case_when(
      influenced == "0" ~ "influenced",
      influenced == "1" ~ "not_influenced",
      TRUE ~ NA_character_
    )
  )

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

print(final_df_wide)

write_csv(final_df_wide, file.path('/Users/milka/Documents/GitHub/psyc 251/waniak2025/data/processed/pilot', "data.csv"))

