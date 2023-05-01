# Load libs
library(tidyverse)
library(readr)
library(tidyr)
library(ggplot2)
library(knitr)

# Load dat
uncert <- read_csv("data/prolific/Bot research_April 30, 2023_21.46.csv")

# Prolific
prolific_1 <- read_csv("data/prolific/prolific_export_644c0bbca40d3cd7cb3c7c7f.csv")
prolific_2 <- read_csv("data/prolific/prolific_export_644d3a9ebcd1c388bd1a262e.csv")
prolific_3 <- read_csv("data/prolific/prolific_export_6440131f3fd384272e37636e.csv")

prolific   <- rbind(prolific_1, prolific_2, prolific_3)

# Filter attn_check
uncert$attn_checkc <- uncert$attn == "Extremely interested,Very interested"

# Left join
all_dat <- uncert %>% 
  left_join(prolific, by = c("prolific_id" = "Participant id"))

# Filter those who consent and not preview
fin_dat <- all_dat %>% 
  filter(consent == 'Yes' & DistributionChannel != "preview" & !is.na(prolific_id) & attn_checkc)


## Recoding
## -----------

# Numeracy
fin_dat$num1c <- fin_dat$num1 == "45 miles"
fin_dat$num2c <- fin_dat$num2 == "500"
fin_dat$num3c <- fin_dat$num3 == "44"
fin_dat$num4c <- fin_dat$num4 == "$150"
fin_dat$num5c <- fin_dat$num5 == "$9000"
fin_dat$num6c <- fin_dat$num6 == "10"

fin_dat$numeracy <- with(fin_dat, rowMeans(cbind(num1c, num2c, num3c, num4c, num5c, num6c)))

num_mean_se = fin_dat %>%
  select(num1c:num6c) %>%
  gather() %>%
  group_by(key) %>%
  summarize(avg = mean(value), se = sqrt(avg*(1 - avg)/n()), low_ci = avg - 2*se, hi_ci = avg + 2*se)

num_mean_se$lab <- c("Train", "Dice", "Father-Son", "Sofa Sale", "Car", "Lottery")

kable(num_mean_se[, c("lab", "avg")])

ggplot(num_mean_se, 
       aes(x=lab, y=avg)) + 
  geom_point(aes(x=lab, y=avg), color = "#aaccff") + 
  geom_errorbar(aes(ymin=low_ci, ymax=hi_ci), width = 0, colour="#0099ff", alpha = .5) + 
  theme_bw() +
  xlab("") + 
  ylab("Proportion Correct") + 
  theme(panel.grid.major = element_line(color="#e1e1e1",  linetype = "dotted"),
        panel.grid.minor = element_blank(),
        legend.position  ="bottom",
        legend.key      = element_blank(),
        legend.key.width = unit(1, "cm"),
        axis.title   = element_text(size = 10, color = "#555555"),
        axis.text    = element_text(size = 10, color = "#555555"),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(vjust = -1, margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(vjust = 1),
        axis.ticks   = element_line(color = "#e3e3e3", linewidth = .2),
        plot.margin = unit(c(0, 1, .5, 0), "cm")) + 
  coord_flip()
ggsave("figs/prolific_numeracy.png")

# Uncertainty qs. --- proportion choosing cash

fin_dat <- fin_dat %>% 
  mutate(sure_choice = case_when(
    uncertainty_exp == "ariely_choice" ~ ariely_choice == "$25 in cash",
    uncertainty_exp == "gigerenzer_choice" ~ gigerenzer_choice == "$25 in cash",
    uncertainty_exp == "certain_choice" ~ certain_choice == "$25 in cash",
    uncertainty_exp == "uncertain_choice" ~ uncertain_choice == "$25 in cash"
  ))

### Analysis
fin_dat %>%
  group_by(uncertainty_exp) %>%
  summarise(mean(sure_choice))

#### Let's look at numeracy
fin_dat %>%
  group_by(uncertainty_exp, numeracy > .7) %>%
  summarise(mean(sure_choice))

### Bonus
fin_dat <- fin_dat %>% 
  mutate(sure_choice_bonus = case_when(
    bonus == "certain" ~ bonus_certain == "$0.25 in cash",
    bonus == "pre" ~ bonus_with_pre_ev == "$0.25 in cash",
    bonus == "uncertain" ~ bonus_uncertain == "$0.25 in cash",
  ))

### Analysis

fin_dat %>%
  group_by(bonus, numeracy > .7) %>%
  summarise(mean(sure_choice_bonus, na.rm = T))

