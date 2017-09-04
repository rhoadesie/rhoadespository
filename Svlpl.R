library(dplyr)

data <- read.csv("svlpl.csv")
#disc <- read.csv("Discard.csv")
discard <- data.frame("Team" = "anyteam", "Wk" = "wk", "Indx" = 5000)

wk1 <- select(data, Team, Wk1); wk1$wk = "Wk1"; colnames(wk1) <- c("Team", "Prb", "Wk")
wk2 <- select(data, Team, Wk2); wk2$wk = "Wk2"; colnames(wk2) <- c("Team", "Prb", "Wk")
wk3 <- select(data, Team, Wk3); wk3$wk = "Wk3"; colnames(wk3) <- c("Team", "Prb", "Wk")
wk4 <- select(data, Team, Wk4); wk4$wk = "Wk4"; colnames(wk4) <- c("Team", "Prb", "Wk")
wk5 <- select(data, Team, Wk5); wk5$wk = "Wk5"; colnames(wk5) <- c("Team", "Prb", "Wk")
wk6 <- select(data, Team, Wk6); wk6$wk = "Wk6"; colnames(wk6) <- c("Team", "Prb", "Wk")
wk7 <- select(data, Team, Wk7); wk7$wk = "Wk7"; colnames(wk7) <- c("Team", "Prb", "Wk")
wk8 <- select(data, Team, Wk8); wk8$wk = "Wk8"; colnames(wk8) <- c("Team", "Prb", "Wk")
wk9 <- select(data, Team, Wk9); wk9$wk = "Wk9"; colnames(wk9) <- c("Team", "Prb", "Wk")
wk10 <- select(data, Team, Wk10); wk10$wk = "Wk10"; colnames(wk10) <- c("Team", "Prb", "Wk")
wk11 <- select(data, Team, Wk11); wk11$wk = "Wk11"; colnames(wk11) <- c("Team", "Prb", "Wk")
wk12 <- select(data, Team, Wk12); wk12$wk = "Wk12"; colnames(wk12) <- c("Team", "Prb", "Wk")
wk13 <- select(data, Team, Wk13); wk13$wk = "Wk13"; colnames(wk13) <- c("Team", "Prb", "Wk")
wk14 <- select(data, Team, Wk14); wk14$wk = "Wk14"; colnames(wk14) <- c("Team", "Prb", "Wk")
wk15 <- select(data, Team, Wk15); wk15$wk = "Wk15"; colnames(wk15) <- c("Team", "Prb", "Wk")
wk16 <- select(data, Team, Wk16); wk16$wk = "Wk16"; colnames(wk16) <- c("Team", "Prb", "Wk")

df <- rbind(wk1, wk2); df <- rbind(df, wk3); df <- rbind(df, wk4)
df <- rbind(df, wk5); df <- rbind(df, wk6); df <- rbind(df, wk7)
df <- rbind(df, wk8); df <- rbind(df, wk9); df <- rbind(df, wk10)
df <- rbind(df, wk11); df <- rbind(df, wk12); df <- rbind(df, wk13)
df <- rbind(df, wk14); df <- rbind(df, wk15); df <- rbind(df, wk16)

df <- arrange(df, desc(Prb))
df$Indx <- seq.int(nrow(df))

#df <- df[!(df$Team %in% discard$Team),]
#df <- df[!(df$Wk %in% discard$Wk),]
df2 <- df

pth1 <- df[1,]
discard2 <- discard
a <- select(pth1, Team, Wk, Indx)
discard <- rbind(discard, a)



df <- df[!(df$Team %in% discard$Team),]
#df <- df[!(df$Wk %in% discard$Wk),]
b <- df[1,]
pth1 <- rbind(pth1, b)


for (i in 1:14) {
  a <- select(pth1, Team, Wk, Indx)
    discard <- rbind(discard, a)
    df <- df[!(df$Team %in% discard$tTeam),]
    df <- df[!(df$Wk %in% discard$Wk),]
    b <- df[1,]
    pth1 <- rbind(pth1, b)
        next
}
#pth1 <- arrange(pth1, Wk)
write.csv(pth1, "Path1.csv")


df <- df2

discard <- discard2
d <- df[1,]
d$Wk <- "wk"; d = select(d, Team, Wk, Indx)
discard <- rbind(discard, d)


df <- df[!(df$Indx %in% pth1$Indx),]
#df <- df[!(df$Wk %in% discard$Wk),]
pth2 <- df[1,]


for (i in 1:15) {
  a <- select(pth2, Team, Wk, Indx)
  discard <- rbind(discard, a)
  df <- df[!(df$Team %in% discard$tTeam),]
  df <- df[!(df$Wk %in% discard$Wk),]
  b <- df[1,]
  pth2 <- rbind(pth2, b)
  next
}
#pth2 <- arrange(pth2, Wk)
write.csv(pth2, "Path2.csv")

df <- df2

discard <- discard2
d <- df[1,]
d$Wk <- "wk"; d = select(d, Team, Wk, Indx)
discard <- rbind(discard, d)


df <- df[!(df$Indx %in% pth1$Indx),]
df <- df[!(df$Indx %in% pth2$Indx),]
pth3 <- df[1,]


for (i in 1:15) {
  a <- select(pth3, Team, Wk, Indx)
  discard <- rbind(discard, a)
  df <- df[!(df$Team %in% discard$tTeam),]
  df <- df[!(df$Wk %in% discard$Wk),]
  b <- df[1,]
  pth3 <- rbind(pth3, b)
  next
}

write.csv(pth3, "Path3.csv")


