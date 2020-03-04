library(haven)
library(dplyr)
library(corrplot)










# YT1
testData <- haven::read_dta(file = "https://www3.nd.edu/~sberry5/data/stataExample.dta")
table(testData$Rater)
table(testData$Gender)
testData %>%
  select("Rater", "Gender", starts_with("lvi"), "effect", starts_with("leader"), starts_with("cred")) %>%
  #  filter(Rater==0) %>%
  #  filter(Rater==3) %>%
  # filter(Gender==1) %>%
    filter(Gender==0) %>%
  select(starts_with("lvi")) %>%
  cor(., use="pairwise.complete.obs") %>%
  corrplot()

# YT2 
testData %>%
  select(-num_range("lvi",11:48)) %>%
  filter(Rater == 0) %>%
  rowwise() %>% 
  mutate(mean1 = mean(c(lvi01, lvi02,lvi03, lvi04,lvi05), na.rm=TRUE),
         mean2 = mean(c(lvi06, lvi07,lvi08, lvi09,lvi10), na.rm=TRUE)) %>% 
  select(mean1, mean2)

# YT3
stataExample <- haven::read_dta(file = "https://www3.nd.edu/~sberry5/data/stataExample.dta")
names(stataExample)

table(stataExample$Gender)
stataExample %>%
  summary() 

stataExample %>%
  group_by(Gender) %>%
  summarise(n = n())

stataExample %>%
  filter(!is.na(Gender)) %>%
  group_by(Gender) %>%
  summarise(n = n())

stataExample %>%
  summarise(mean = mean(leader_tenure, na.rm=TRUE))

stataExample %>%
  summarise(n = n_distinct(leaderID))



# YT4
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
  filter(n>1) %>%
  ggplot(., aes(x=homeworld, y=n)) + 
  geom_point() + 
  theme_minimal()


# YT5 
library(haven)
library(readr)
merge1 = "https://www3.nd.edu/~sberry5/data/merge1Company.dta"
merge2Hoberg = "https://www3.nd.edu/~sberry5/data/merge2Hoberg.txt"
sasExample = "https://www3.nd.edu/~sberry5/data/wciklink_gvkey.sas7bdat"

m1 = haven::read_dta(merge1)
m2 = readr::read_delim(merge2Hoberg, "\t")
m4 = haven::read_sas(sasExample)

names(m1)
names(m2)
names(m4)

head(m1)
head(m2)
head(m4)

m4$gvkey = as.numeric(m4$gvkey)

inner_join(m1, m4, by=c("coname"))
left_join(m1, m2, by=c("gvkey"))

anti_join(m1, m2, by=c("gvkey"))


# regex

realComments = c("I love wrangling data", "strings r fun", 
                 "I wish it were break", "Can't we use excel?")

grep(pattern = "\\b[a-z]{2}\\b", x = realComments, value=TRUE)
grepl(pattern = "\\b[a-z]{2}\\b", x = realComments)


# YT6 
# movie data
library(rvest)
highest = read_html("https://en.wikipedia.org/wiki/List_of_highest-grossing_films") %>% 
  html_table(fill = TRUE) %>%
  magrittr::extract2(1) %>% 
  mutate(gross = stringr::str_replace_all(.$`Worldwide gross`, "[A-Za-z]*[0-9]*\\$|,|[A-Za-z].*", ""), 
         gross = as.numeric(gross)) %>%
  mutate(Peak = stringr::str_replace_all(.$Peak, "[A-Za-z]+[0-9]*", "")) %>%
  mutate(Peak = as.numeric(Peak))

highest$Peak
highest$gross



  