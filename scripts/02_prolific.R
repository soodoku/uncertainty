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
  filter(consent == 'Yes' & DistributionChannel != "preview" & !is.na(prolific_id) & attn_checkc & bonus != '.50' & bonus != '1')

# Bonus via prolific
bon <- fin_dat %>% filter(!is.na(bonus_amt)) %>% select(prolific_id, bonus_amt)
bon$bonus_amt <- ifelse(grepl("\\.", bon$bonus_amt), paste0("0", bon$bonus_amt), bon$bonus_amt)
write_csv(bon, file = "data/prolific/bonus_uncertain_cash.csv") # sum(as.numeric(bon$bonus_amt)) ($197)
fin_dat$yes_to_cash_3_TEXT[!is.na(fin_dat$yes_to_cash_3_TEXT)] #email only gift cards
a <- fin_dat$prolific_id[!is.na(fin_dat$bonus_certain) & fin_dat$bonus_certain == "$0.25 in cash"]
b <- fin_dat$prolific_id[!is.na(fin_dat$bonus_uncertain) & fin_dat$bonus_uncertain == "$0.25 in cash"]
c <- fin_dat$prolific_id[!is.na(fin_dat$bonus_with_pre_ev) & fin_dat$bonus_with_pre_ev == "$0.25 in cash"]
d <- unique(c(a, b, c))
e <- d[!(d %in% c("63f77d4f671167ec16eee15c", "62c66885ba0578c9b5a0e4ae", "5e3244f5bac19521aee1a0de", "610da052e73bf3db58b1402f",
         "60c2dde65a63ca7ad434f658", "5c17a9fbfeaf2c0001c4b19a", "5feca808ebe59297998b5133", "63ed86ca95e89a58884b9677", 
         "5ea7b7db3021ac19f4c60770", "5e6033dbfb4665381e27f2fc", "57d810f1813eb30001825b65", "61086791465482e3ec238c87", 
         "63d698429453269de520bbb8", "615defb34adf2f2e2e4bc539"))]
f <- data.frame(prolific_id = e, bonus = "0.25")
write_csv(f, file = "data/prolific/bonus_certain.csv")

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
        plot.margin = unit(c(.2, .5, 1, .2), "cm")) + 
coord_flip()
ggsave("figs/prolific_numeracy.png")
ggsave("figs/prolific_numeracy.pdf")

# Uncertainty qs. --- proportion choosing cash

### Analysis

fin_dat <- fin_dat %>% filter(!is.na(uncertainty_exp))
fin_dat <- fin_dat %>% 
  mutate(sure_choice = case_when(
    uncertainty_exp == "ariely_choice" ~ ariely_choice == "$25 in cash",
    uncertainty_exp == "gigerenzer_choice" ~ gigerenzer_choice == "$25 in cash",
    uncertainty_exp == "certain_choice" ~ certain_choice == "$25 in cash",
    uncertainty_exp == "uncertain_choice" ~ uncertain_choice == "$25 in cash"
  ))

fin_dat <- fin_dat %>% 
  mutate(uncertainty_exp = recode(uncertainty_exp,
                       "ariely_choice" = "Decoy",
                       "gigerenzer_choice" = "Natural Frequency",
                       "certain_choice" = "Certain choice",
                       "uncertain_choice" = "Uncertain choice"))

### Analysis
prolific_exp1 <- fin_dat %>%
  filter(!is.na(uncertainty_exp)) %>%
  group_by(uncertainty_exp) %>%
  summarise(mean = mean(sure_choice, na.rm = T), 
            se = sd(sure_choice)/sqrt(n()),
            lower = mean - qt(0.975, df = length(sure_choice)-1) * se,
            upper = mean + qt(0.975, df = length(sure_choice)-1) * se)

latex_prolific_exp1 <- xtable(prolific_exp1, caption = "Means and SEs from Experiment 1", label = "table:prolific_means")
print(latex_prolific_exp1, include.rownames = FALSE, caption.placement = "top", file = "tabs/prolific_exp1.tex")

# Create the ggplot
ggplot(prolific_exp1, aes(x = mean, y = uncertainty_exp, xmin = lower, xmax = upper)) +
  geom_point(size = 2, color = "#aaccff") +
  geom_errorbar(width = 0, colour="#0099ff", alpha = .5) +
  labs(x = "Proportion Choosing $25 in Cash", y = "") +
  #geom_text(aes(label = cond), hjust = 0.5, vjust = -0.75, color = "black", size = 3) + 
  geom_text(aes(label = paste(gsub("^0+", "",round(mean, 2)), "\n(",gsub("^0+", "",round(se, 2)),")")),
            hjust = -0.1, vjust = 1.5, color = "black", size = 3) +
  theme_bw() + 
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
        plot.margin = unit(c(.2, .5, 1, .2), "cm"))
ggsave("figs/prolific_exp1.png")
ggsave("figs/prolific_exp1.pdf")

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

fin_dat <- fin_dat %>% 
  mutate(bonus = recode(bonus,
                                "pre" = "Ask",
                                "certain" = "Certain choice",
                                "uncertain" = "Uncertain choice"))

prolific_exp2 <- fin_dat %>%
  group_by(bonus) %>%
  summarise(mean = mean(sure_choice_bonus, na.rm = T), 
            se = sd(sure_choice_bonus, na.rm = T)/sqrt(n()),
            lower = mean - qt(0.975, df = length(sure_choice_bonus)-1) * se,
            upper = mean + qt(0.975, df = length(sure_choice_bonus)-1) * se)

latex_prolific_exp2 <- xtable(prolific_exp2, caption = "Means and SEs from Experiment 1", label = "table:prolific_means")
print(latex_prolific_exp2, include.rownames = FALSE, caption.placement = "top", file = "tabs/prolific_exp1.tex")

# Create the ggplot
ggplot(prolific_exp2, aes(x = mean, y = bonus, xmin = lower, xmax = upper)) +
  geom_point(size = 2, color = "#aaccff") +
  geom_errorbar(width = 0, colour="#0099ff", alpha = .5) +
  labs(x = "Proportion Choosing $25 in Cash", y = "") +
  #geom_text(aes(label = cond), hjust = 0.5, vjust = -0.75, color = "black", size = 3) + 
  geom_text(aes(label = paste(gsub("^0+", "",round(mean, 2)), "\n(",gsub("^0+", "",round(se, 2)),")")),
            hjust = -0.1, vjust = 1.5, color = "black", size = 3) +
  theme_bw() + 
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
        plot.margin = unit(c(.2, .5, 1, .2), "cm"))
ggsave("figs/prolific_exp2.png")
ggsave("figs/prolific_exp2.pdf")

## Regressions
summary(lm(sure_choice ~ uncertainty_exp + bonus + numeracy, data = fin_dat))
summary(lm(sure_choice ~ uncertainty_exp*numeracy, data = fin_dat))
summary(lm(sure_choice_bonus ~ bonus + numeracy, data = fin_dat))

### Long form
exp_1 <- fin_dat %>%
  select(sure_choice, uncertainty_exp, ResponseId, numeracy)

exp_2 <- fin_dat %>%
  select(sure_choice_bonus, bonus, ResponseId, numeracy) %>%
  rename(sure_choice = sure_choice_bonus, uncertainty_exp = bonus)

ldat <- bind_rows(exp_1, exp_2)
# 1. certain_choice_or_not ~ condition_ids [6 dummies] + clustered_se_by_respondent
library(lme4)
summary(lmer(sure_choice ~ uncertainty_exp + numeracy + (1|ResponseId), data = ldat))

# Recode to 5 dummies, combining certain and a variable for bonus

ldat <- ldat %>%
  mutate(bonus_or_not = case_when(
    uncertainty_exp == 'certain_choice' ~ 0,
    uncertainty_exp == 'gigerenzer_choice' ~ 0,
    uncertainty_exp == 'ariely_choice' ~ 0,
    uncertainty_exp == 'uncertain_choice' ~ 0,
    uncertainty_exp == 'certain' ~ 1,
    uncertainty_exp == 'pre' ~ 1,
    uncertainty_exp == 'uncertain' ~ 1
))

ldat <- ldat %>%
  mutate(treat_five = case_when(
    uncertainty_exp == 'certain_choice' ~ 'certain',
    uncertainty_exp == 'gigerenzer_choice' ~ 'gigerenzer_choice',
    uncertainty_exp == 'ariely_choice' ~ 'ariely_choice',
    uncertainty_exp == 'uncertain_choice' ~ 'uncertain',
    uncertainty_exp == 'certain' ~ 'certain',
    uncertainty_exp == 'pre' ~ 'pre',
    uncertainty_exp == 'uncertain' ~ 'uncertain'
  )) %>%
  mutate(treat_five = fct_relevel(treat_five, "certain"))


summary(lmer(sure_choice ~ treat_five*bonus_or_not*numeracy + (1|ResponseId), data = ldat))

# Let's do transitions 
small_dat <- fin_dat %>%
  filter(uncertainty_exp == 'uncertain_choice' | bonus == 'uncertain')
table(small_dat$sure_choice, small_dat$sure_choice_bonus)

