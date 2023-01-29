# Load libs
library(tidyverse)
library(readr)

# Load dat
uncert <- read_csv("data/Uncertainty+Effect+replication_January+28,+2023_20.38.zip")

# Filter attn_check
uncert$attn_checkc <- uncert$attn_check == "Extremely interested,Very interested"

# Filter those who consent and not preview
fin_dat <- uncert[uncert$consent == 'Yes' & uncert$DistributionChannel != "preview" & uncert$attn_checkc, ]

# Numeracy
fin_dat$num1c <- fin_dat$num1 == "$29.50"
fin_dat$num2c <- fin_dat$num2 == "500"
fin_dat$num3c <- fin_dat$num3 == "100"
fin_dat$num4c <- fin_dat$num4 == "$150"
fin_dat$num5c <- fin_dat$num5 == "$9,000"
fin_dat$num6c <- fin_dat$num6 == "100"

fin_dat$numeracy <- with(fin_dat, rowMeans(cbind(num1c, num2c, num3c, num4c, num5c, num6c)))

# Uncertainty qs. --- proportion choosing cash
mean(fin_dat$original_sure == "$25 in cash", na.rm = T)
mean(fin_dat$original_uncertain == "$25 in cash", na.rm = T)
mean(fin_dat$uncertain_clarified == "$25 in cash", na.rm = T)
mean(as.numeric(fin_dat$followup))

# Direct comp
mean(fin_dat$direct_comp == "A $50 Amazon gift card", na.rm = T)

# PID 
fin_dat$dem <- fin_dat$political_party %in% c("1", "2", "3", "4")
fin_dat$rep <- fin_dat$political_party %in% c("7", "8", "9", "10")

# Unemployment interpretation
fin_dat %>% 
  group_by(econ_cond) %>% 
  summarise(a = mean(unemp_reps == "Got Better", na.rm = T),
            b = mean(unemploy_dems == "Got Better", na.rm = T), 
            c = mean(inflation_reps == "Got Better", na.rm = T),
            d = mean(inflation_dems == "Got Better", na.rm = T))

# Mistakes
fin_dat$edit_reps_n <- as.numeric(fin_dat$edit_reps)
fin_dat$edit_dems_n <- as.numeric(fin_dat$edit_dems)

fin_dat %>% 
  group_by(edit_cond, dem) %>%
  summarise(mean_rep = mean(edit_reps_n, na.rm = T), median_rep = median(edit_reps_n, na.rm = T), 
            mean_dem = mean(edit_dems_n, na.rm = T), median_dem = median(edit_dems_n, na.rm = T))

# Representativeness
fin_dat %>%
  group_by(rh_cond, rh_partyorder) %>%
  summarise(mean_black_dem = mean(as.numeric(black_dem), na.rm = T), mean_dem_black = mean(as.numeric(dem_black), na.rm = T), 
            mean_evang_rep = mean(as.numeric(evang_rep), na.rm = T), mean_rep_evang = mean(as.numeric(rep_evang), na.rm = T))

