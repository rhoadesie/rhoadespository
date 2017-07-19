library(dplyr)

data <- read.csv("svlpl.csv")
disc <- read.csv("Discard.csv")

wk16 <- filter(data, Wk16 > .5)
wk16 <- arrange(wk16, desc(Wk16))
pkwk16.1 <- top_n(wk16,1,Wk16)

