# Load libs
library(tidyverse)
library(readr)
library(tidyr)
library(ggplot2)
library(knitr)

# Load dat
uncert <- read_csv("data/prolific/Bot research_April 30, 2023_11.19.csv")
 
# Filter attn_check
uncert$attn_checkc <- uncert$attn == "Extremely interested,Very interested"

# Filter those who consent and not preview
fin_dat <- uncert[uncert$consent == 'Yes' & uncert$DistributionChannel != "preview" & !is.na(uncert$prolific_id) & uncert$attn_checkc, ]

## Recoding
## -----------

# Numeracy
fin_dat$num1c <- fin_dat$num1 == "$29.50"
fin_dat$num2c <- fin_dat$num2 == "500"
fin_dat$num3c <- fin_dat$num3 == "100"
fin_dat$num4c <- fin_dat$num4 == "$150"
fin_dat$num5c <- fin_dat$num5 == "$9,000"
fin_dat$num6c <- fin_dat$num6 == "10"

fin_dat$numeracy <- with(fin_dat, rowMeans(cbind(num1c, num2c, num3c, num4c, num5c, num6c)))

num_mean_se = fin_dat %>%
  select(num1c:num6c) %>%
  gather() %>%
  group_by(key) %>%
  summarize(avg = mean(value), se = sqrt(avg*(1 - avg)/n()), low_ci = avg - 2*se, hi_ci = avg + 2*se)

num_mean_se$lab <- c("Overdraft", "Dice", "Disease", "Sofa Sale", "Car", "Lottery")

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
        axis.ticks   = element_line(color = "#e3e3e3", size = .2),
        plot.margin = unit(c(0, 1, .5, 0), "cm")) + 
  coord_flip()
ggsave("figs/numeracy.png")



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

