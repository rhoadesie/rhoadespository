# Scraping player boxscores from College Basketball Reference
install.packages('dplyr')
library(data.table)
library(RMySQL)
library(RCurl)
library(XML)
library(dplyr)
library(reshape2)
c('rvest','dplyr','pipeR', 'stringr','pbapply','sqldf') -> packages
lapply(packages, library, character.only = T)
options(sqldf.driver = "SQLite") # as per FAQ #7 force SQLite
options(gsubfn.engine = "R") # as per FAQ #5 use R code rather than tcltk


season_start_date <- '2018-11-01'

# CONNECT #####################################################################
dbconn <- dbConnect(MySQL(), user='root', pwd='password'
                    , dbname='hoops_college', host = "127.0.0.1", port=3306)
# Get rosters
query <- paste0("SELECT r.*, t.bbRefTeamName FROM team_rosters r, teams t WHERE t.teamID = r.teamID
          and season = ",season_end,";")
team_rosters <- dbGetQuery(dbconn, query)

# Get names
query <- "SELECT * from all_names;"
all.names <- dbGetQuery(dbconn, query)
###############################################################################

unique(team_rosters$season)

# FUNCTION - now handles LucyIndex and calls to Score scraper
getNCAAbbPlayerGames <- function(gameDate) {
  mm <- as.numeric(format(gameDate, "%m"))
  dd <- as.numeric(format(gameDate, "%d"))
  YYYY <- as.numeric(format(gameDate, "%Y"))
  URL <- paste0("https://www.sports-reference.com/cbb/boxscores/index.cgi?month=",mm,"&day=",dd,"&year=",YYYY)
  pg <- read_html(URL)
  
  # Get the game Scores for the date into a data.frame
  scores <- getNCAAbbScores(gameDate)

  # Add the teamIDs for the Home and Away teams
  scores.with.ids <- sqldf("
                        SELECT scr.*
                           ,ateam.teamID as AwayTeamID
                           ,hteam.teamID as HomeTeamID
                           FROM scores scr
                           ,`all.teams` ateam
                           ,`all.teams` hteam
                           WHERE
                           scr.HomeTeam in (hteam.school,hteam.bbRefTeamName,hteam.kenpomTeamName,hteam.bbRefAltName)
                           AND scr.AwayTeam in (ateam.school,ateam.bbRefTeamName,ateam.kenpomTeamName,ateam.bbRefAltName)
                           ")
    
  # Get the final / boxscore links
  # Note: This step had to be re-done for the 2016-17 season
  links <-   pg %>%  
    html_nodes(".game_summaries") %>% 
    html_nodes(xpath = ".//tr/td[3]/a") %>% 
    html_attr("href")

  # Remove problem links
  links <- links[links != "/cbb/boxscores/2017-11-10-milwaukee.html"]

  # Get the Box Scores using pblapply to get a status bar
  boxscores <- pblapply(links, function(x) {
    url <- paste0("https://www.sports-reference.com", x)
    tabs <- getURL(url)
    tblA1 <- readHTMLTable(tabs,stringsAsFactors = FALSE)
    tblA1 <- tblA1[lapply(tblA1,length)==23]  # filter out the "other scores in this conf" banner table
    if (length(tblA1) != 0) {  
      tblA <- tblA1[[1]]
      tblB <- tblA1[[2]]
      names(tblA)[1] <- "Starters" # every once in awhile this column is labeled "Player"
      names(tblB)[1] <- "Starters" # every once in awhile this column is labeled "Player"
      tblA$gs <- 0
      tblB$gs <- 0
      tblA$gs[1:5] <- 1
      tblB$gs[1:5] <- 1
      tblA <- subset(tblA, !grepl("Reserves",Starters) )
      tblA <- subset(tblA, !grepl("Player",Starters) )
      tblB <- subset(tblB, !grepl("Reserves",Starters) )
      tblB <- subset(tblB, !grepl("Player",Starters) )
      tblA$teamID <- str_replace(names(tblA1)[1],"box-score-basic-","")
      tblA$opponentID <- str_replace(names(tblA1)[2],"box-score-basic-","")
      tblB$teamID <- str_replace(names(tblA1)[2],"box-score-basic-","")
      tblB$opponentID <- str_replace(names(tblA1)[1],"box-score-basic-","")
      tblA$Player <- NULL
      tblB$Player <- NULL
      tblA$Starters.1 <- NULL
      tblB$Starters.1 <- NULL
      tblA$MP.1 <- NULL
      tblB$MP.1 <- NULL
      tbl <- rbind(tblA,tblB)
      tbl$gameDate <-  substr(x,16,25)   
      Sys.sleep(0.5)  # to avoid being blacklisted
      
      tbl
    }
  })
  
  b <- bind_rows(boxscores)
  b <- transform(b
                              ,Starters=as.factor(Starters)
                              ,MP=as.numeric(MP)
                              ,FG=as.numeric(FG)
                              ,FGA=as.numeric(FGA)
                              ,FT=as.numeric(FT)
                              ,FTA=as.numeric(FTA)
                              ,ORB=as.numeric(ORB)
                              ,DRB=as.numeric(DRB)
                              ,TRB=as.numeric(TRB)
                              ,AST=as.numeric(AST)
                              ,STL=as.numeric(STL)
                              ,BLK=as.numeric(BLK)
                              ,TOV=as.numeric(TOV)
                              ,PF=as.numeric(PF)
                              ,PTS=as.numeric(PTS)
  )
  ## Weird - transform turns their colnames into X2p, X3pa....
  b <- transform(b
                 ,FG.=as.numeric(FG.)
                 ,FT.=as.numeric(FT.)
                 ,X2P=as.numeric(X2P)
                 ,X2PA=as.numeric(X2PA)
                 ,X2P.=as.numeric(X2P.)
                 ,X3P=as.numeric(X3P)
                 ,X3PA=as.numeric(X3PA)
                 ,X3P.=as.numeric(X3P.)                         
  )

  season_end <- YYYY
  if (mm >= 9 ) { season_end <- YYYY+1 }
  team_summaries <- sqldf( paste("select b.gameDate, tr.bbRefTeamName, tr.teamID, sum(b.MP) totMP
                                  , sum(tr.totalInches) totIN
                                 , sum(b.MP * tr.totalInches) totMPxIN
                                 , avg(tr.totalInches) avgHeight
                                 , sum(b.MP * tr.totalInches) / sum(b.MP) effHeight
                                 , count(1) as numPlayers
                                 from `b`
                                 ,`team_rosters` tr
                                 where b.teamID = tr.teamID
                                 and IFNULL(tr.boxScorePlayerName,tr.Player) = b.Starters
                                 and tr.season = ",season_end,
                                 "group by b.gameDate, tr.bbRefTeamName,tr.teamID
                                 order by 1,4,2"))
  team_summaries$gameDate <- as.Date(gameDate)
  scores.table <-  sqldf("
        SELECT s.AwayTeam, s.AwayScore,s.HomeTeam, s.HomeScore
              ,s.Winner, s.Loser, s.Margin, s.AwayWin, s.HomeWin
              ,s.GameDate
              ,tsa.totMP as AwayMP
              ,tsh.totMP as HomeMP
              ,tsa.numPlayers as AwayPlayers
              ,tsh.numPlayers as HomePlayers
              ,tsa.avgHeight as AwayAvgHeight
              ,tsh.avgHeight as HomeAvgHeight
              ,tsa.effHeight as AwayEffHeight
              ,tsh.effHeight as HomeEffHeight
              ,case when tsh.avgHeight>tsa.avgHeight then HomeTeam else AwayTeam end as TallerTeamByAvg
              ,case when tsh.effHeight>tsa.effHeight then HomeTeam else AwayTeam end as TallerTeamByEff
              ,case when (tsh.avgHeight>tsa.avgHeight and HomeTeam == Winner) or (tsh.avgHeight<tsa.avgHeight and AwayTeam == Winner) then 1 else 0 end as TallerAvgWin
              ,case when (tsh.avgHeight>tsa.avgHeight and AwayTeam == Winner) or (tsh.avgHeight<tsa.avgHeight and HomeTeam == Winner) then 1 else 0 end as SmallerAvgWin
              ,case when (tsh.effHeight>tsa.effHeight and HomeTeam == Winner) or (tsh.effHeight<tsa.effHeight and AwayTeam == Winner) then 1 else 0 end as TallerEffWin 
              ,case when (tsh.effHeight>tsa.effHeight and AwayTeam == Winner) or (tsh.effHeight<tsa.effHeight and HomeTeam == Winner) then 1 else 0 end as SmallerEffWin 
        FROM `scores.with.ids` s
            ,`team_summaries` tsh
            ,`team_summaries` tsa
        WHERE
          s.GameDate = tsh.gameDate
        AND s.GameDate = tsa.gameDate
        AND s.HomeTeamID = tsh.teamID
        AND s.AwayTeamID = tsa.teamID
        ")
  
  dbconn <- dbConnect(MySQL(), user='root', pwd='password'
                      , dbname='hoops_college', host = "127.0.0.1", port=3306)
  # Write Mizzou and SLU player details only
  mizzou.slu <- subset(b,teamID == "saint-louis" | teamID == "missouri" | opponentID == "saint-louis" | opponentID == "missouri")
  if (nrow(`mizzou.slu`) > 0) {
    dbWriteTable(conn=dbconn, name='player_games', value=mizzou.slu, row.names=FALSE,overwrite=FALSE,append=TRUE) 
    print('Mizzou/SLU')
  }
  
  # Write Scores info to table
  dbWriteTable(conn=dbconn, name='scores', value=scores.table, row.names=FALSE,overwrite=FALSE,append=TRUE) 
  dbDisconnect(dbconn)
  ## End FUNCTION
  }


# getNCAAbbPlayerGames(gameDate) 
getNCAAbbPlayerGames(as.Date("02/05/2019","%m/%d/%Y")) 

# Run It! Loop
# gameDate <- as.Date("12/23/2018","%m/%d/%Y")
gameDate <- Sys.Date() - 1
gameDate
  while (gameDate < Sys.Date()) {
    print(gameDate)
    getNCAAbbPlayerGames(gameDate) 
    gameDate <- gameDate + 1
    Sys.sleep(4)
  }
  
  ########################################################
  # Update season scores df & run the Taller calcs
  conn <- dbConnect(MySQL(), user='root', pwd='password'
                    , dbname= 'hoops_college', host = "127.0.0.1", port=3306)
  season.scores <- suppressWarnings(dbGetQuery(conn, paste0("SELECT * FROM `scores` where gameDate > '",season_start_date,"'")))
  #total year
  suppressWarnings(dbGetQuery(conn,paste0("SELECT sum(TallerEffWin) as tallerWins
                      , sum(SmallerEffWin) as smallerWins
                      , round(sum(TallerEffWin) / (sum(TallerEffWin) + sum(SmallerEffWin)),2) as tallerWPct
                      FROM `scores`
                      where gameDate > '",season_start_date,"'")
  )) 
  # by month
  suppressWarnings(dbGetQuery(conn,paste0("SELECT MONTHNAME(gameDate) as Month
                      , sum(TallerEffWin) as tallerWins
                      , sum(SmallerEffWin) as smallerWins
                      , round(sum(TallerEffWin) / (sum(TallerEffWin) + sum(SmallerEffWin)),2) as tallerWPct
                      FROM `scores`
                      where gameDate > '",season_start_date,"'","
                      GROUP BY MONTHNAME(gameDate)
                      ORDER BY YEAR(gameDate),MONTH(gameDate)")
  )) 
  # most recent
  head(suppressWarnings(dbGetQuery(conn,paste0("SELECT gameDate, sum(TallerEffWin) as tallerWins
                      , sum(SmallerEffWin) as smallerWins
                      , round(sum(TallerEffWin) / (sum(TallerEffWin) + sum(SmallerEffWin)),2) as tallerWPct
                      FROM `scores`
                      where gameDate > '",season_start_date,"'","
                      group by gameDate
                      order by gameDate desc")
  )))
  dbDisconnect(conn=conn)
##########################################################

## scratch paper
tbl <- unlist(links)
while (length(tbl) >= 1) {
  url <- paste0("http://www.sports-reference.com", tbl[1])
  print(url)
  response<- GET(url)
  if (response$status_code!=200){ # HTTP request failed!!
    # do some stuff...
    print(paste("Failure:",i,"Status:",response$status_code))
    next
  }
  tblA1 <- readHTMLTable(url,stringsAsFactors = FALSE)
  print(paste("Success:",i,"Status:",response$status_code))
  tbl <- tbl[-1]
}
tbl

library(httr)
library(XML)

url <- "http://www.cnj.jus.br/improbidade_adm/visualizar_condenacao.php"
for  (i in 575:585){
  response<- GET(url,path="/",query=c(seq_condenacao=as.character(i)))
  if (response$status_code!=200){ # HTTP request failed!!
    # do some stuff...
    print(paste("Failure:",i,"Status:",response$status_code))
    next
  }
  doc <- htmlParse(response, encoding = "UTF-8")
  # do some other stuff
  print(paste("Success:",i,"Status:",response$status_code))
}


## scratch
sqldf(" SELECT s.AwayTeam, s.AwayScore,s.HomeTeam, s.HomeScore
              ,s.Winner, s.Loser, s.Margin, s.AwayWin, s.HomeWin, s.gameDate
      FROM `scores` s
      WHERE
s.HomeTeam = 'Kansas'")

sqldf(" SELECT distinct GameDate
      FROM `scores` s
where GameDate = '2016-11-11'
     ")
scores$HomeTeam
team_summaries$bbRefTeamName


b$gameDate <- as.Date(b$gameDate)
team_summaries$gameDate <- as.Date(team_summaries$gameDate)

library(data.table)
options(sqldf.driver = "SQLite") # as per FAQ #7 force SQLite
options(gsubfn.engine = "R") # as per FAQ #5 use R code rather than tcltk
sqldf("
        SELECT s.AwayTeam, s.AwayScore,s.HomeTeam, s.HomeScore
      ,s.Winner, s.Loser, s.Margin, s.AwayWin, s.HomeWin
      ,s.GameDate
      ,tra.totMP as AwayMP
      ,trh.totMP as HomeMP
      ,tra.numPlayers as AwayPlayers
      ,trh.numPlayers as HomePlayers
      ,tra.avgHeight as AwayAvgHeight
      ,trh.avgHeight as HomeAvgHeight
      ,tra.effHeight as AwayEffHeight
      ,trh.effHeight as HomeEffHeight
      ,case when trh.avgHeight>tra.avgHeight then HomeTeam else AwayTeam end as TallerTeamByAvg
      ,case when trh.effHeight>tra.effHeight then HomeTeam else AwayTeam end as TallerTeamByEff
      ,case when (trh.avgHeight>tra.avgHeight and HomeTeam == Winner) or (trh.avgHeight<tra.avgHeight and AwayTeam == Winner) then 1 else 0 end as TallerAvgWin
      ,case when (trh.avgHeight>tra.avgHeight and AwayTeam == Winner) or (trh.avgHeight<tra.avgHeight and HomeTeam == Winner) then 1 else 0 end as SmallerAvgWin
      ,case when (trh.effHeight>tra.effHeight and HomeTeam == Winner) or (trh.effHeight<tra.effHeight and AwayTeam == Winner) then 1 else 0 end as TallerEffWin 
      ,case when (trh.effHeight>tra.effHeight and AwayTeam == Winner) or (trh.effHeight<tra.effHeight and HomeTeam == Winner) then 1 else 0 end as SmallerEffWin 
      FROM `scores` s
      ,`team_summaries` trh
      ,`team_summaries` tra
      WHERE
      s.GameDate = trh.gameDate
      AND s.GameDate = tra.gameDate
      AND s.HomeTeam = trh.bbRefTeamName
      AND s.AwayTeam = tra.bbRefTeamName
      ")


sqldf("
        SELECT s.GameDate
              ,s.AwayTeam
              ,tra.bbRefTeamName as AwayName
              ,s.HomeTeam
              ,trh.bbRefTeamName as HomeName
        FROM `scores` s
            ,`team_summaries` trh
            ,`all.teams` ath
            ,`team_summaries` tra
            ,`all.teams` ata
        WHERE
          s.GameDate = trh.gameDate
        AND s.GameDate = tra.gameDate
        AND s.HomeTeam = ath.School
        ANd ath.bbRefTeamName = trh.bbRefTeamName
        AND s.AwayTeam = ata.School
        AND ata.bbRefTeamName = tra.bbRefTeamName
        ")


sqldf("
        SELECT s.*, aname.teamID as AwayTeamID
          ,hname.teamID as HomeTeamID
      FROM `scores` s
      ,`all.names` aname
      ,`all.names` hname
      WHERE
          s.HomeTeam = hname.Name
      AND s.AwayTeam = aname.Name
      ")

sqldf( paste("select b.gameDate, tr.bbRefTeamName, tr.teamID, sum(b.MP) totMP
        , sum(tr.totalInches) totIN
             , sum(b.MP * tr.totalInches) totMPxIN
             , avg(tr.totalInches) avgHeight
             , sum(b.MP * tr.totalInches) / sum(b.MP) effHeight
             , count(1) as numPlayers
             from `b`
             ,`team_rosters` tr
             where b.teamID = tr.teamID
             and IFNULL(tr.boxScorePlayerName,tr.Player) = b.Starters
             and tr.season = ",season_end,
             "group by b.gameDate, tr.bbRefTeamName,tr.teamID
             order by 1,4,2"))

links <-links[links != "/cbb/boxscores/2016-11-11-cal-state-fullerton.html"]
links
