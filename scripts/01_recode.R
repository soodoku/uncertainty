# Load libs
library(tidyverse)
library(readr)

# Load dat
uncert <- read_csv("data/Uncertainty+Effect+replication_January+28,+2023_20.38.zip")

# Filter attn_check
uncert$attn_checkc <- uncert$attn_check == "Extremely interested,Very interested"

# Filter those who consent and not preview
fin_dat <- uncert[uncert$consent == 'Yes' & uncert$DistributionChannel != "preview" & uncert$attn_checkc, ]

## Recoding
## -----------

# Numeracy
fin_dat$num1c <- fin_dat$num1 == "$29.50"
fin_dat$num2c <- fin_dat$num2 == "500"
fin_dat$num3c <- fin_dat$num3 == "100"
fin_dat$num4c <- fin_dat$num4 == "$150"
fin_dat$num5c <- fin_dat$num5 == "$9,000"
fin_dat$num6c <- fin_dat$num6 == "100"

fin_dat$numeracy <- with(fin_dat, rowMeans(cbind(num1c, num2c, num3c, num4c, num5c, num6c)))

# Uncertainty qs. --- proportion choosing cash

fin_dat <- fin_dat %>% 
  mutate(sure_choice = case_when(
    cond == "clarifying" ~ uncertain_clarified == "$25 in cash",
    cond == "original_sure" ~ original_sure == "$25 in cash",
    cond == "original_uncertain" ~ original_uncertain == "$25 in cash",
    cond == "direct" ~ direct_comp == "A $50 Amazon gift card"
  ))

# Coerce followup to numeric + take out the $1000 response 

fin_dat$followup_n <- as.numeric(fin_dat$followup)
fin_dat$followup_n[fin_dat$followup_n == 1000] <- NA

### Analysis
fin_dat %>%
  group_by(cond) %>%
  summarise(mean(sure_choice))

fin_dat %>%
  group_by(cond) %>%
  summarise(mean_est = mean(followup_n, na.rm = T), med_est = median(followup_n, na.rm = T))

fin_dat %>%
  group_by(cond, sure_choice) %>%
  summarise(mean_est = mean(followup_n, na.rm = T), med_est = median(followup_n, na.rm = T))

#### Let's look at numeracy
fin_dat %>%
  group_by(cond, numeracy > .7) %>%
  summarise(mean(sure_choice))

fin_dat %>%
  filter(numeracy > .7) %>%
  group_by(cond, sure_choice) %>%
  summarise(mean_est = mean(followup_n, na.rm = T), med_est = median(followup_n, na.rm = T))

fin_dat %>%
  group_by(cond, numeracy > .7) %>%
  summarise(mean_est = mean(followup_n, na.rm = T), med_est = median(followup_n, na.rm = T))

### By follow-up estimate
fin_dat %>%
  group_by(cond, followup >= 50) %>%
  summarise(mean(sure_choice))

