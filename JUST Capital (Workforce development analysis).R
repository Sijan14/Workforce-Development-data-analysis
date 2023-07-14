install.packages("psych")
library(tidyverse)
library(dplyr)
library(psych)
library(stats)
library(forcats)

df <- read_csv("2024 Workforce Development - data-collection.csv")

# Cleaning data and keeping only the necessary variables / columns
df <- df %>% 
  select(company_name = name,
         ticker,
         employee_size,
         apprenticeships = datapointslug_value_1,
         second_chance = datapointslug_value_2,
         veteran_hiring = datapointslug_value_3,
         restart_program = datapointslug_value_4)

df <- df %>% 
  mutate_at(vars(apprenticeships, second_chance, veteran_hiring, restart_program), as.numeric)

df1 <- read_csv("2024 Workforce Development - rankings2023_data.csv")

df1 <- df1 %>% 
  select(!c(APPRENTICESHIP, SECOND_CHANCE, VETERAN_HIRING, company_perm_id))

# Joining the two data set together
data <- df %>% 
  inner_join(df1, by = "ticker")

data %>% group_by(industry_name) %>% 
  count() %>% arrange(desc(n))

# Transforming the NA values to 0
data$apprenticeships <- ifelse(is.na(data$apprenticeships), 0, data$apprenticeships)
data$second_chance <- ifelse(is.na(data$second_chance), 0, data$second_chance)
data$veteran_hiring <- ifelse(is.na(data$veteran_hiring), 0, data$veteran_hiring)
data$restart_program <- ifelse(is.na(data$restart_program), 0, data$restart_program)

# correlation between market_caps and datapoints
cor.test(data$apprenticeships, data$marketcap, method = 'pearson') # significant

ggplot(data, aes(x = marketcap, y = apprenticeships)) +
  geom_point() +
  labs(x = "Market Capitalization", y = "Has Program") +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() # doesn't really work

median(data$marketcap, na.rm = TRUE) # 17.4 billion usd
mcap <- data %>% 
  mutate(marketcap = ifelse(marketcap <= median(marketcap, na.rm = T), "below median", "above median")) %>% 
  filter(!is.na(marketcap))

mcap %>% group_by(marketcap) %>% count()
# chi-square test for apprenticeship
chisq.test(mcap$marketcap, mcap$apprenticeships) # significant
ggplot(mcap, aes(x = marketcap, fill = factor(apprenticeships), na.rm = T)) +
  geom_bar(position = "fill") +
  labs(x = "Market Capitalization", y = "Proportion") +
  scale_fill_discrete(name = "Apprenticeship", labels = c("No", "Yes")) +
  theme_minimal()

# chi-square test for veteran hiring
chisq.test(mcap$marketcap, mcap$veteran_hiring)
ggplot(mcap, aes(x = marketcap, fill = factor(veteran_hiring), na.rm = T)) +
  geom_bar(position = "fill") +
  labs(x = "Market Capitalization", y = "Proportion") +
  scale_fill_discrete(name = "Veteran Hiring", labels = c("No", "Yes")) +
  theme_minimal()

# chi-square test for second chance
chisq.test(mcap$marketcap, mcap$second_chance)
ggplot(mcap, aes(x = marketcap, fill = factor(second_chance), na.rm = T)) +
  geom_bar(position = "fill") +
  labs(x = "Market Capitalization", y = "Proportion") +
  scale_fill_discrete(name = "Second Chance", labels = c("No", "Yes")) +
  theme_minimal()

# chi-square test for restart program
chisq.test(mcap$marketcap, mcap$restart_program)
ggplot(mcap, aes(x = marketcap, fill = factor(restart_program), na.rm = T)) +
  geom_bar(position = "fill") +
  labs(x = "Market Capitalization", y = "Proportion") +
  scale_fill_discrete(name = "Restart program", labels = c("No", "Yes")) +
  theme_minimal()

options(scipen = 999)
# Graph for apprenticeships
logit <- glm(apprenticeships ~ factor(industry_name), data = data, family = binomial)
summary(logit)

app <- data %>% 
  group_by(industry_name) %>% 
  count(apprenticeships) %>% 
  pivot_wider(names_from = apprenticeships, values_from = n) %>% 
  mutate(percentage = `1`/sum(`0`+`1`)) %>% 
  filter(!is.na(percentage))
app

app %>% 
  ggplot(aes(x = percentage, y = reorder(industry_name, percentage))) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Apprenticeships %", y = "Industry")

# Graph for veteran hiring
vet <- data %>% 
  group_by(industry_name) %>% 
  count(veteran_hiring) %>% 
  pivot_wider(names_from = veteran_hiring, values_from = n) %>% 
  mutate(percentage = `1`/sum(`0`+`1`)) %>% 
  filter(!is.na(percentage))
vet

vet %>% 
  ggplot(aes(x = percentage, y = reorder(industry_name, percentage))) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Veteran Hiring %", y = "Industry")

# Graph for second chance
sec <- data %>% 
  group_by(industry_name) %>% 
  count(second_chance) %>% 
  pivot_wider(names_from = second_chance, values_from = n) %>% 
  mutate(percentage = `1`/sum(`0`+`1`))
sec <- sec %>% 
  arrange(is.na(percentage))

sec %>% 
  ggplot(aes(x = percentage, y = reorder(industry_name, percentage))) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Second Chance %", y = "Industry")

# Graph for restart program
re <- data %>% 
  group_by(industry_name) %>% 
  count(restart_program) %>% 
  pivot_wider(names_from = restart_program, values_from = n) %>% 
  mutate(percentage = `1`/sum(`0`+`1`))
re <- re %>% 
  arrange(is.na(percentage))

re %>% 
  ggplot(aes(x = percentage, y = reorder(industry_name, percentage))) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Restart Program %", y = "Industry")


