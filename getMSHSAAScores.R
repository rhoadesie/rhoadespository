# Adapted from script created for NCAA Basketball.  File is NCAA BB scraper.R 
# Scraping GAME SCORES from the Missouri State High School Activities Association website
library(XML)
library(RMySQL)
library(RCurl)
library(reshape2)
library(dplyr)
library(tidyr)
library(stringi)
library(xlsx)
packages <- c('rvest','dplyr','pipeR', 'stringr','pbapply','sqldf') 
lapply(packages, library, character.only = T)
options(sqldf.driver = "SQLite") # as per FAQ #7 force SQLite
options(gsubfn.engine = "R") # as per FAQ #5 use R code rather than tcltk


## Get the scores from the database
conn <- dbConnect(MySQL(), user='root', pwd='password'
                  , dbname= 'mshsaa', host = "127.0.0.1", port=3306)
      all.scores <- suppressWarnings(dbGetQuery(conn, "SELECT * FROM `scores`"))
dbDisconnect(conn=conn)


gameDate <- as.Date("04/08/2018","%m/%d/%Y")
gameDates <- seq.Date(as.Date("2018-05-30"),as.Date("2018-06-02"),by="day")
# Example: https://www.mshsaa.org/Activities/Scoreboard.aspx?alg=34&date=03152019
# alg is the sport.  34 for Girls Soccer
# date is the date in mmddyyyy form


  scoreboard.home.url <- "https://www.mshsaa.org/Activities/Scoreboard.aspx"
 
  scoreboard.home.pg <- read_html(scoreboard.home.url)
  
  scoreboard.home.links <-  html_attr(html_nodes(scoreboard.home.pg, "a"), "href")
  
  test <-   scoreboard.home.pg %>%  
    html_nodes(".formsharp") %>% 
    html_nodes("ul") %>% 
    html_nodes(xpath = ".//li[1]/a") %>% 
         html_attr("href")
  
  test <-   scoreboard.home.pg %>%  
         html_nodes(".formsharp") %>% 
         html_nodes(xpath = ".//ul/li/a") %>% 
         html_attr("href")
  
  

  //*[@id="SelectSport"]/ul/li[1]/a
  
  //*[@id="SelectSport"]/ul/li[1]/a
  

  # ensure urls start with "http" to avoid google references to the search page
  links <- grep("http://", links, fixed = TRUE, value=TRUE)
  return(links)
  
  
  <a href="/Activities/Scoreboard.aspx?alg=66">
    <i class="flaticon flaticon-baseball"></i>
    Baseball - Fall Season
  </a>
    
    //*[@id="SelectSport"]/ul/li[6]/a
  

# Function
getMSHSAAScores <- function(gameDate, sport) {
  # Scrapes and preps the scoreboard data from the Missouri State High School Activities Association website  
  gameDateText <- format(gameDate, format="%m%d%Y")
  
  #### OK for now let's skip the part about getting the different scoreboards for different sports....
  
  url <- paste0("https://www.mshsaa.org/Activities/Scoreboard.aspx?alg=34&date=",gameDateText)

  ###### Reading data
  tabs <- getURL(url)
  tbl <- readHTMLTable(tabs,stringsAsFactors = FALSE)
  tbl <- suppressMessages(melt(tbl[-c(1:2)]))  # first two items in list are garbage
  
  scores <- data.frame(1,2,3,4,5)
  if (length(tbl) > 1) {
    # begin loop
    #  While loop is to keep subsetting the list down 3 rows at a time until done
    colnames(scores) <- c("AwayChunk","AwayScore","HomeChunk","HomeScore","LocationChunk")
    while (nrow(tbl) >= 3) {
      if (!is.na(tbl[3,1]) & !is.na(tbl[1,2])) 
        scores <- rbind(scores,setNames(data.frame(tbl[1,2],tbl[1,3],tbl[2,2],tbl[2,3],tbl[3,1]),names(scores)))
      tbl <- tbl[-(1:3),]
    }
    # end loop
    
    ## parse out the Chunked elements ##
    scores <- transform(scores, Away=colsplit(scores$AwayChunk,c("[(]"),names=c("Team","Chunk")))
    scores <- transform(scores, Home=colsplit(scores$HomeChunk,c("[(]"),names=c("Team","Chunk")))
    
    ## calculate winner and loser based on score
    scores <- transform(scores
                       ,HomeScore=as.numeric(HomeScore)
                       ,AwayScore=as.numeric(AwayScore)
    )
    scores$Loser <- with(scores,ifelse(HomeScore>AwayScore,Away.Team,Home.Team))
    scores$Winner <- with(scores,ifelse(HomeScore>AwayScore,Home.Team,Away.Team))
    scores$Margin <- with(scores,abs(HomeScore-AwayScore))
    scores$HomeWin <- with(scores,ifelse(HomeScore>AwayScore,1,0))
    scores$AwayWin <- with(scores,ifelse(HomeScore>AwayScore,0,1))
    scores$GameDate <- gameDateText
  }
  scores <- scores[-1,]
    
  return(scores)
}

gameDate <- as.Date("03/14/2009","%m/%d/%Y")
gameDates <- gameDate
gameDates <- seq.Date(as.Date("2018-05-18"),as.Date("2018-05-19"),by="day")

scr <- NULL
while (gameDate <= "2019-06-01") {
  print(gameDate)
  df <- as.data.frame(getMSHSAAScores(gameDate)) 
  scr <- rbind(get0("scr"),get0("df"))
  gameDate <- gameDate + 1
  Sys.sleep(2)
}
View(scr)

scr$Indx <- seq.int(nrow(scr))
scr <- separate(scr,AwayChunk, into=c("AwaySchool","AwayRecord","AwayClass"),sep="[\n]")
scr <- separate(scr,HomeChunk, into=c("HomeSchool","HomeRecord","HomeClass"),sep="[\n]")
scr <- separate(scr,LocationChunk, into=c("Event","Location","Status"),sep="[\n]")

scr2 <- sqldf("select Indx, TRIM(SUBSTR(`Home.Team`,1,INSTR(`Home.Team`,'\n'))) as HomeTeamRough
                  , HomeScore
                  , TRIM(SUBSTR(`Away.Team`,1,INSTR(`Away.Team`,'\n'))) as AwayTeamRough
                  , AwayScore
                  , LocationChunk
                  , GameDate
      from scr")
scr2$HomeTeam <- trimws(gsub("[\r\n]","",scr2$HomeTeamRough))
scr2$AwayTeam <- trimws(gsub("[\r\n]","",scr2$AwayTeamRough))
##

# Writing the backup out to .csv for extra protection, as I may screw this data up trying to tidy it
#  note: I first tried write.xlsx2 on this but ran into a "GC overhead limit" memory error
write.csv(scr.backup, "/Users/gregrhoades/Documents/R Stuff/MHSAA/soccer_results_raw.csv")
write.csv(scr, "/Users/gregrhoades/Documents/R Stuff/MHSAA/soccer_results.csv")
scr2 <- read.csv("/Users/gregrhoades/Documents/R Stuff/MHSAA/soccer_results_edited2.csv",stringsAsFactors = FALSE)

scr2$alg <- 34
scr2$activityName <- "Soccer - Girls"

dbconn <- dbConnect(MySQL(), user='root', pwd='password'
                    , dbname='mshsaa', host = "127.0.0.1", port=3306)
dbWriteTable(conn=dbconn, name='scores', value=scr2, row.names=FALSE,overwrite=FALSE,append=TRUE) 
dbDisconnect(dbconn)

## Clean-up
# update player_games set Starters = 'D''Angelo Allen' where teamID = 'missouri' and Starters like 'D%Allen'
# update player_games set Starters = 'JaQuail Townser' where teamID = 'jacksonville-state' and Starters like 'JaQuail Townswer'
# update player_games set Starters = 'Jared Savage' where teamID = 'austin-peay' and Starters = 'Jared Savag'
# update player_games set Starters = 'Jaylen Snead' where teamID = 'cal-poly' and Starters = 'Jaylen Shead'
# update player_games set Starters = 'Michael Thomas' where teamID = 'hawaii' and Starters = 'Mike Thomas'
# update player_games set Starters = 'Phillip Lawrence-Ricks' where teamID = 'marist' and Starters = 'Phillip Lawrence'
# update player_games set Starters = 'Quentin Shropshire' where teamID = 'long-beach-state' and Starters = 'Questin Shropshire'



# Check It!
conn <- dbConnect(MySQL(), user='root', pwd='password'
                  , dbname='hoops_college', host = "127.0.0.1", port=3306)
query <- paste0("
                select GameDate, count(1)
                from scores
                where GameDate >= '"
                ,format(Sys.Date()-7,"%Y-%m-%d"),
                "' group by GameDate
                ORDER by GameDate DESC")
recentScores <- suppressWarnings(dbGetQuery(conn, query))
dbDisconnect(conn=conn)
head(recentScores,10)
##


## Kill all MySQL connections
all_cons <- dbListConnections(MySQL())
for(con in all_cons)
  dbDisconnect(con)

# remove all objects from workspace
#rm(list = ls())


sqldf("select `Home.Team`
      from scr 
      limit 1")

sqldf("select TRIM(`Home.Team`), count(1) 
      from scr 
      group by TRIM(`Home.Team`)
      order by 2 desc")

sqldf("select LENGTH(TRIM(`Home.Team`))
      from scr 
      order by 1 desc")

sqldf("select TRIM(SUBSTR(`Home.Team`,1,INSTR(`Home.Team`,'\n'))),LENGTH(TRIM(SUBSTR(`Home.Team`,1,INSTR(`Home.Team`,'\n'))))
      from scr")


sqldf("select HomeTeam, count(1) 
      from scr2 
      group by HomeTeam
      order by 2 desc")

sqldf("select HomeTeam, LENGTH(HomeTeam), LENGTH(TRIM(HomeTeam)) 
      from scr2 
  ")


sqldf("select TRIM(SUBSTR(`HomeChunk`,1,INSTR(`HomeChunk`,'\n')))
,LENGTH(TRIM(SUBSTR(`HomeChunk`,1,INSTR(`HomeChunk`,'\n'))))
,INSTR(`HomeChunk`,'\n',2)
      from scr
      where Indx = 1")


scr.backup$Indx <- seq.int(nrow(scr.backup))
