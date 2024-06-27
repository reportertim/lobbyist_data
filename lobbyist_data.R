# Data must be manually downloaded from https://floridalobbyist.gov/CompensationReportSearch/DownloadCompReport. Typically we use the most recent four quarterly reports.


# Load libraries
library(readr)
library(dplyr)
library(writexl)


# Set the working directory to legislative data
setwd("data/leg")

# List all the tab-delimited text files in the folder
files <- list.files(pattern = "\\.txt$")

# Read each file into a data frame, converting all columns to character type
leg_data_frames <- lapply(files, function(file) {
  df <- read_delim(file, delim = "\t")
  df <- mutate_all(df, as.character)
  return(df)
})

# Combine all data frames into one
leg_data <- bind_rows(leg_data_frames)

# Set the working directory to executive data
setwd("..")
setwd("exec")

# List all the tab-delimited text files in the folder
files <- list.files(pattern = "\\.txt$")

# Read each file into a data frame, converting all columns to character type
exec_data_frames <- lapply(files, function(file) {
  df <- read_delim(file, delim = "\t")
  df <- mutate_all(df, as.character)
  return(df)
})

# Combine all data frames into one
exec_data <- bind_rows(exec_data_frames)

# Get back to main directory
setwd("..")
setwd("..")

# Make the reported numbers into median values
leg_data <- leg_data %>%
  mutate(median_value = case_when(
    PRINCIPAL_COMPENSATION_RANGE == "$1.00-$9,999.00" ~ 5000,
    PRINCIPAL_COMPENSATION_RANGE == "$10,000.00-$19,999.00" ~ 15000,
    PRINCIPAL_COMPENSATION_RANGE == "$20,000.00-$29,999.00" ~ 25000,
    PRINCIPAL_COMPENSATION_RANGE == "$30,000.00-$39,999.00" ~ 35000,
    PRINCIPAL_COMPENSATION_RANGE == "$40,000.00-$49,999.00" ~ 45000,
    TRUE ~ 0
  ))

exec_data <- exec_data %>%
  mutate(median_value = case_when(
    PRINCIPAL_COMPENSATION_RANGE == "$1.00-$9,999.00" ~ 5000,
    PRINCIPAL_COMPENSATION_RANGE == "$10,000.00-$19,999.00" ~ 15000,
    PRINCIPAL_COMPENSATION_RANGE == "$20,000.00-$29,999.00" ~ 25000,
    PRINCIPAL_COMPENSATION_RANGE == "$30,000.00-$39,999.00" ~ 35000,
    PRINCIPAL_COMPENSATION_RANGE == "$40,000.00-$49,999.00" ~ 45000,
    TRUE ~ 0
  ))


# Get total comp
leg_comp <- leg_data %>%
  group_by(FIRM_NAME) %>%
  summarise(
    leg_compensation=sum(median_value)
  ) %>%
  arrange(desc(leg_compensation))

exec_comp <- exec_data %>%
  group_by(FIRM_NAME) %>%
  summarise(
    exec_compensation=sum(median_value)
  ) %>%
  arrange(desc(exec_compensation))

total_comp <- leg_comp |>
  inner_join(exec_comp, by = join_by(FIRM_NAME == FIRM_NAME)) %>%
  mutate(total_comp = rowSums(across(where(is.numeric)))) %>%
  arrange(desc(total_comp))


# Get top clients
leg_top_clients <- leg_data %>%
  group_by(FIRM_NAME, PRINCIPAL_NAME) %>%
  summarise(leg_payment=sum(median_value)
            ) %>%
  filter(leg_payment == max(leg_payment)) %>%
  arrange(FIRM_NAME, desc(leg_payment)) %>%
  ungroup()

leg_top_clients <- rename(leg_top_clients,c(top_legislative_clients=PRINCIPAL_NAME)) %>%
  filter(leg_payment > 50000)


exec_top_clients <- exec_data %>%
  group_by(FIRM_NAME, PRINCIPAL_NAME) %>%
  summarise(exec_payment=sum(median_value)
  ) %>%
  filter(exec_payment == max(exec_payment)) %>%
  arrange(FIRM_NAME, desc(exec_payment)) %>%
  ungroup()

exec_top_clients <- rename(exec_top_clients,c(top_executive_clients=PRINCIPAL_NAME)) %>%
  filter(exec_payment > 50000)


# Top buyers of lobbying services

leg_buyers <- leg_data %>%
  group_by(PRINCIPAL_NAME) %>%
  summarise(leg_lobbying_payment = sum(median_value)) %>%
  arrange(desc(leg_lobbying_payment))

exec_buyers <- exec_data %>%
  group_by(PRINCIPAL_NAME) %>%
  summarise(exec_lobbying_payment = sum(median_value)) %>%
  arrange(desc(exec_lobbying_payment))

total_buyers <- leg_buyers |>
  inner_join(exec_buyers, by = join_by(PRINCIPAL_NAME == PRINCIPAL_NAME)) %>%
  mutate(total_payment = rowSums(across(where(is.numeric)))) %>%
  arrange(desc(total_payment))

# Make list of lobbyists with demographic info

leg_demo <- leg_data %>%
  filter(RECORD_TYPE == "FIRM") %>%
  select(FIRM_NAME, CERTIFICATION_NAME, ADDRESS_LINE_1, ADDRESS_LINE_2, CITY, STATE, POSTAL_CODE, PHONE_NUMBER) %>%
  distinct()

exec_demo <- exec_data %>%
  filter(RECORD_TYPE == "FIRM") %>%
  select(FIRM_NAME, CERTIFICATION_NAME, ADDRESS_LINE_1, ADDRESS_LINE_2, CITY, STATE, POSTAL_CODE, PHONE_NUMBER) %>%
  distinct()

total_demo <- inner_join(leg_demo, exec_demo) %>%
  distinct()

# Combine for list

for_list <- total_demo |>
  inner_join(total_comp, by = join_by(FIRM_NAME == FIRM_NAME)) %>%
  arrange(desc(total_comp))

for_list <- for_list %>%
  distinct(FIRM_NAME, .keep_all = TRUE) 

# export

write_xlsx(for_list,"top_lobbyists.xlsx")
write_xlsx(total_buyers, "top_buyers.xlsx")
write_xlsx(leg_top_clients,"top_leg_clients.xlsx")
write_xlsx(exec_top_clients,"top_exec_clients.xlsx")