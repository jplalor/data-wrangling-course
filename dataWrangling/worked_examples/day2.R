library(haven)
library(dplyr)


# Q1
stataExample <- haven::read_dta(file = "https://www3.nd.edu/~sberry5/data/stataExample.dta")
names(stataExample)

stataExample %>%
  summary() 

stataExample %>%
  group_by(Gender) %>%
  summarise(n = n())

stataExample %>%
  summarise(mean = mean(leader_tenure, na.rm=TRUE))

stataExample %>%
  summarise(n = n_distinct(leaderID))


# Q2
# star wars data 

library(tidyr)
library(ggplot2)
names(starwars)

starwars %>% 
  filter(species == "Human" & grepl("(Skywalker)|(Rey)|(Vader)|(Kylo)", .$name)) %>% 
  select(name, height, mass) %>% 
  tidyr::pivot_longer(., names_to="type", values_to="value", -name) %>% 
  ggplot(., aes(x = name, y = value, color = type)) + 
  geom_point(size = 3.5) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal()

table(starwars$species)

starwars %>% 
  filter(species == "Droid") %>% 
  select(name, height, mass) %>% 
  tidyr::pivot_longer(., names_to="type", values_to="value", -name) %>% 
  ggplot(., aes(x = name, y = value, color = type)) + 
  geom_point(size = 3.5) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal()

starwars %>% 
  filter(species == "Human") %>% 
  select(name, homeworld) %>%
  group_by(homeworld) %>%
  mutate(n = n()) %>% 
  ggplot(., aes(x=homeworld, y=n)) + 
  geom_point() + 
  theme_minimal()

  tidyr::pivot_longer(., names_to="type", values_to="value", -name) %>% 
  ggplot(., aes(x = name, y = value, color = type)) + 
  geom_point(size = 3.5) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal()


  
realComments = c("I love wrangling data", "stringz r fun", 
                   "This guy is a hack", "Can't we use excel?")

# movie data
highest = read_html("https://en.wikipedia.org/wiki/List_of_highest-grossing_films") %>% 
  html_table(fill = TRUE) %>%
  magrittr::extract2(1) %>% 
  mutate(gross = stringr::str_replace_all(.$`Worldwide gross`, "\\$|,|[A-Za-z].*", ""), 
         gross = as.numeric(gross)) %>%
  mutate(Peak = stringr::str_replace_all(.$Peak, "[A-Za-z]", "")) %>%
  mutate(Peak = as.numeric(Peak))

highest


# data merging

data2003 = readr::read_csv("https://www3.nd.edu/~sberry5/data/c2003_a.csv")

data2004 = readr::read_csv("https://www3.nd.edu/~sberry5/data/c2004_a.csv")

data2013 = readr::read_csv("https://www3.nd.edu/~sberry5/data/c2013_a.csv")

complete = rbind(data2003, data2004)

## This will cause an error because of variable names!

complete = rbind(complete, data2013)

data2013 <- data2013 %>%
  select(names(complete))
  

  