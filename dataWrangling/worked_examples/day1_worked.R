library(haven)
library(dplyr)
library(corrplot)

testData <- haven::read_dta(file = "https://www3.nd.edu/~sberry5/data/stataExample.dta")
names(testData)

# Q1
testData %>%
  select(starts_with("lvi"), "effect", starts_with("leader"), starts_with("cred")) %>%
  summary()

testData %>%
  select(starts_with("lvi")) %>%
#  cor() %>%
  cor(., use="pairwise.complete.obs") %>% 
  corrplot()

# Q2
table(testData$Rater)
testData %>%
  select("Rater", "Gender", starts_with("lvi"), "effect", starts_with("leader"), starts_with("cred")) %>%
#  filter(Rater==0) %>%
#  filter(Rater==3) %>%
  filter(Gender==1) %>%
#  filter(Gender==0) %>%
  select(starts_with("lvi")) %>%
  cor(., use="pairwise.complete.obs") %>%
  corrplot()

# Q3
testData %>%
  select(-num_range("lvi",11:48)) %>%
  filter(Rater == 0) %>%
  rowwise() %>% 
  mutate(mean1 = mean(c(lvi01, lvi02,lvi03, lvi04,lvi05), na.rm=TRUE),
         mean2 = mean(c(lvi06, lvi07,lvi08, lvi09,lvi10), na.rm=TRUE)) %>% 
  select(mean1, mean2)


