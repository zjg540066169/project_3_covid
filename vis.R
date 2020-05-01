library(tidyverse)
library(foreach)

data = 
  read_csv("covid19-1.csv") %>% 
  #select(-Id) %>% 
  janitor::clean_names() %>% 
  mutate(date = as.Date.character(date, "%m/%d/%y")) %>% 
  group_by(country_region, date) %>% 
  mutate(confirmed_cases = sum(confirmed_cases), fatalities = sum(fatalities)) %>% 
  ungroup() %>% 
  select(-id, -province_state, -fatalities, -lat, -long) %>% 
  distinct()
  
first_infection = 
  data %>% 
  filter(confirmed_cases != 0) %>% 
  group_by(country_region) %>%
  summarise(first = min(date)) %>% 
  ungroup()

data = 
  data %>% 
  left_join(first_infection, by = "country_region") %>% 
  
  mutate(date = date - first + 1) %>% 
  mutate(date = as.integer(date)) %>% 
  select(-first)

data$date[data$date < 0] = 0

data = distinct(data) %>% filter(date != 0)

coef <- read.csv("cumulative_region_no_0.csv")

reg_a <- foreach(reg=data$country_region) %do% coef$X.a.[reg==coef$X.region_name.]
reg_b <- foreach(reg=data$country_region) %do% coef$X.b.[reg==coef$X.region_name.]
reg_c <- foreach(reg=data$country_region) %do% coef$X.c.[reg==coef$X.region_name.]

reg_a <- unnest(tibble(reg_a))
reg_b <- unnest(tibble(reg_b))
reg_c <- unnest(tibble(reg_c))

new_data <- data %>% add_column(
  a = reg_a,
  b = reg_b,
  c = reg_c
) %>% mutate(
  estimate_case = a$reg_a/(1+exp(-b$reg_b*(date-c$reg_c)))
) %>% select(country_region, date, confirmed_cases,estimate_case)

reg_max <- new_data %>% group_by(country_region) %>% summarise(max = max(confirmed_cases))

reg_50_100 <- reg_max %>% filter(max>50 & max<=100)
reg_100_200 <- reg_max %>% filter(max>100 & max<=200)
reg_200_1000 <- reg_max %>% filter(max>200 & max<=1000)
reg_1000_10000 <- reg_max %>% filter(max>1000 & max<=10000)
reg_over_10000 <- reg_max %>% filter(max>10000)

new_data_50_100 <- new_data %>% filter(country_region %in% reg_50_100$country_region)
new_data_100_200 <- new_data %>% filter(country_region %in% reg_100_200$country_region)
new_data_200_1000 <- new_data %>% filter(country_region %in% reg_200_1000$country_region)
new_data_1000_10000 <- new_data %>% filter(country_region %in% reg_1000_10000$country_region)
new_data_over_10000 <- new_data %>% filter(country_region %in% reg_over_10000$country_region)

ggplot(new_data_50_100, aes(x=date, y=estimate_case, color=country_region)) + geom_line() + theme(legend.position="bottom")
ggplot(new_data_100_200, aes(x=date, y=estimate_case, color=country_region)) + geom_line() + theme(legend.position="bottom")
ggplot(new_data_200_1000, aes(x=date, y=estimate_case, color=country_region)) + geom_line() + theme(legend.position="bottom")
ggplot(new_data_1000_10000, aes(x=date, y=estimate_case, color=country_region)) + geom_line() + theme(legend.position="bottom")
ggplot(new_data_over_10000, aes(x=date, y=estimate_case, color=country_region)) + geom_line() + theme(legend.position="bottom")
