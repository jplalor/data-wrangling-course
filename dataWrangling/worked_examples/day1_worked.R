library(haven)
library(dplyr)
library(corrplot)


# YT1
D.sdc <- read.table("https://www3.nd.edu/~sberry5/data/sdcTest.txt",
                    header=T, sep="^",
                    comment.char = "",
                    quote="", stringsAsFactors = F)





# YT2
testData <- haven::read_dta(file = "https://www3.nd.edu/~sberry5/data/stataExample.dta")
names(testData)


testData %>%
  select(starts_with("lvi"), "effect", starts_with("leader"), starts_with("cred")) %>%
  summary()

testData %>%
  select(starts_with("lvi")) %>%
  cor(., use="pairwise.complete.obs") %>% 
  corrplot()


testdata <- haven::read_dta(file = "https://www3.nd.edu/~sberry5/data/stataExample.dta")

attributes(testdata$Rater) 

attr(testdata$Rater, "label")

attr(testdata$Rater, "label") <- "New description"

attr(testdata$Rater, "label")


# YT3
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



