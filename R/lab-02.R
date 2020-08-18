#Covid 19 Data
---
  title: "Geography 176A"
author: "[Meagan Brown](https://meaganbrown.github.io)"
subtitle: 'Lab 02: COVID-19 Pandemic'
output:
  html_document:
  theme: darkly
---

library(tidyverse)
library(knitr)
library(readxl)
library(zoo)
library(kableExtra)
library(dplyr)
library(ggplot2)

url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'
covid = read_csv(url)

#Question 1

dat = covid %>%
  filter(state == "California") %>%
  group_by(county) %>%
  mutate(newCases = cases - lag(cases)) %>%
  ungroup() %>%
  filter(date == max(date))


most_new_cases = dat %>%
  slice_max(newCases, n = 5) %>%
  select(county, newCases)

most_cases = dat %>%
  slice_max(cases, n=5) %>%
  select(county,cases)


knitr::kable(most_new_cases,
             caption = "Most New Cases within California by County",
             col.names = c("County", "New Cases"),
             format.args = list(big.mark = ",")) %>%
  kableExtra::kable_styling("striped", full_width = TRUE, font_size = 14)

knitr::kable(most_cases,
             caption = "Most Cases within California by County",
             col.names = c("County", "New Cases"),
             format.args = list(big.mark = ",")) %>%
  kableExtra::kable_styling("striped", full_width = TRUE, font_size = 14)

library(readxl)
pop <- read_excel("~/Documents/github/geog-176A-labs/data/PopulationEstimates.xls",
                  skip = 2)

popu =
  pop %>%
  select(fips = "FIPStxt", state = "State", Area_Name, pop2019 = "POP_ESTIMATE_2019") %>%
  right_join(covid, by = c("fips"))

cali = popu %>%
  filter(state.x == "CA") %>%
  group_by(county) %>%
  mutate(newCases = cases - lag(cases)) %>%
  mutate(cpc = cases/pop2019) %>%
  mutate(cpcnew = newCases/pop2019) %>%
  ungroup() %>%
  filter(date == max(date))

mostNewCasesbypop = cali %>%
  arrange((cpcnew)) %>%
  slice_max(cpcnew, n = 5) %>%
  select(county, New_Cases_Per_Capita = cpc)

mostCasesbypop = cali %>%
  slice_max(cpc, n=5) %>%
  arrange(-cpc) %>%
  select(county,Cases_Per_Capita = cpc)

knitr::kable(mostNewCasesbypop,
             caption = "Most New Cases Per Capita within California by County",
             col.names = c("County", "New Cases Per Capita"),
             format.args = list(big.mark = ",")) %>%
  kableExtra::kable_styling("striped", full_width = TRUE, font_size = 14)

knitr::kable(mostCasesbypop,
             caption = "Most Cases per Capita within California by County",
             col.names = c("County", "Cases per Capita"),
             format.args = list(big.mark = ",")) %>%
  kableExtra::kable_styling("striped", full_width = TRUE, font_size = 14)

fourteen = popu %>%
  filter(date > max(date) - 14, state.x == "CA") %>%
  group_by(fips) %>%
  mutate(newCases = cases - lag(cases)) %>%
  summarize(cases14 = sum(cases, na.rm = TRUE),
            newCases14 = sum(newCases, na.rm = TRUE),
            pop2019 = max(pop2019)) %>%
  mutate(pc_nc = newCases14 / (pop2019 / 100000)) %>%
  filter(pc_nc <= 100) %>%
  select(fips, pc_nc)


# (cases / unit) >= case / unit2
# 100,000 unit1 = unit2

#Question 2
pop2 = pop %>%
  select(state= Area_Name, POP_ESTIMATE_2019)


four = covid %>%
  filter(state == "California" | state == "New York" | state == "Louisiana" | state == "Florida") %>%
  group_by(state, date) %>%
  summarize(cases = sum(cases)) %>%
  left_join(pop2, by = "state")


morefour = four %>%
  group_by(state) %>%
  mutate(newCases = cases - lag(cases)) %>%
  mutate(avg = rollmean(newCases, 7, fill = NA, align = "right"))

graph1 =
  morefour %>%
  group_by(date) %>%
  ggplot(aes(x=date, y= newCases, color = state))+
  geom_line(size = 0.5) +
  facet_wrap(~state) +
  ggthemes::theme_igray() +
  labs(title = "Number of Daily New Cases by State",
       x = "Date",
       y = "New Cases",
       caption = "Lab 02")
ggsave(graph1, file = "~/Documents/github/geog-176A-labs/img/graph1.png")


graph20 =
  morefour %>%
  group_by(date) %>%
  ggplot(aes(x=date, y= avg, color = state))+
  geom_line(size = 0.5) +
  facet_wrap(~state, scale = "free_y") +
  ggthemes::theme_igray() +
  labs(title = "Average Weekly Increase in Cases by State",
       x = "Date",
       y = "Average Weekly Increases",
       caption = "Lab 02")
ggsave(graph20, file = "~/Documents/github/geog-176A-labs/img/graph2AWI.png")

fourcapita = morefour %>%
  group_by(state) %>%
  mutate(newCpTP = newCases/POP_ESTIMATE_2019) %>%
  mutate(meancap = rollmean(newCpTP, 7, fill = NA, align = "right"))


graph11 = fourcapita %>%
  group_by(date) %>%
  ggplot(aes(x=date, y= newCpTP, yellow = "California", black = "Florida"), y = free)+
  geom_line(size = 0.5) +
  facet_wrap(~state, scale = "free_y") +
  ggthemes::theme_igray() +
  labs(title = "Number of Daily New Cases per Capita by State",
       x = "Date",
       y = "New Cases per Capita",
       caption = "Lab 02")
ggsave(graph11, file = "~/Documents/github/geog-176A-labs/img/graphDNCbycapita.png")


#Question 2
pop2 = pop %>%
  select(state= Area_Name, POP_ESTIMATE_2019)


four = covid %>%
  filter(state == "California" | state == "New York" | state == "Louisiana" | state == "Florida") %>%
  group_by(state, date) %>%
  summarize(cases = sum(cases)) %>%
  left_join(pop2, by = "state")


morefour = four %>%
  group_by(state) %>%
  mutate(newCases = cases - lag(cases)) %>%
  mutate(avg = rollmean(newCases, 7, fill = NA, align = "right"))

graph1 =
  morefour %>%
  group_by(date) %>%
  ggplot(aes(x=date, y= newCases, color = state))+
  geom_line(size = 0.5) +
  facet_wrap(~state) +
  ggthemes::theme_igray() +
  labs(title = "Number of Daily New Cases by State",
       x = "Date",
       y = "New Cases",
       caption = "Lab 02")
ggsave(graph1, file = "~/Documents/github/geog-176A-labs/img/graph1.png")


fourcapita = morefour %>%
  group_by(state) %>%
  mutate(newCpTP = newCases/POP_ESTIMATE_2019) %>%
  mutate(meancap = rollmean(newCpTP, 7, fill = NA, align = "right"))

