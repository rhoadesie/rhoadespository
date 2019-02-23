# Adapted from MUTigers.com Box Scraper
# The SLU site looks similar on the surface, but the construction is completely different.  Uses JSON.

# Load Packages
packages <- c('rvest','dplyr'
#              ,'Hmisc'
              ,'pipeR', 'knitr',
              'XML','RMySQL','stringr','rvest','pbapply','reshape2',
              'dplyr','pbapply','xlsx','ggplot2','stringi','iotools'
              ,'stringr')
lapply(packages, library, character.only = T) #loops through the packages and loads them
install.packages('iotools',type='source')
install.packages("xlsx",type="source")
library(xlsx)
library(rJava)
# Set variables
cg.team_id <- "saint-louis"
cg.season_end <- 2001

#######

# Get Schedule (actually Results) page
# NOTE: For SLU, I am having trouble parsing out the Boxscores links from the Schedule page.  Looks like these pages are bundled in some kind
#  of JSON that I can't easily parse.....
# cg.URL <- "http://www.slubillikens.com/SportSelect.dbml?DB_OEM_ID=27200&SPID=93215&SPSID=632626&q_season=2009&spids=&conf_school_id="
# cg.pg <- read_html(cg.URL)
#
# cg.HTML <- read_html(cg.URL)
# selector_name<-".schedule_game_links"
# cg.links <- NULL
# 
# cg.links <- cg.HTML %>%
#  html_nodes(".schedule_game_links") %>%
#  html_nodes("a") %>% html_attr("href")


# ...so instead I am just going to try to load up cg.links based on the URL pattern I observed
cg.links <- NULL
#for (content_id in 157181:157545)  {
#for (content_id in 157546:157790)  {
#for (content_id in 157205:157208)  {
#for (content_id in 157205:157453)  {
for (content_id in 157248:157453) {
  link <- paste0("http://www.nmnathletics.com/ViewContent.dbml?DB_OEM_ID=27200&CONTENT_ID=",content_id)
  cg.links <- append(cg.links,link)
  next
}

# These are just for troubleshooting
# x <- cg.links[1]
 x <- "http://www.nmnathletics.com/ViewContent.dbml?DB_OEM_ID=27200&CONTENT_ID=157199"
 x <- cg.links[1]
 cg.links <- cg.links[-1]
 cg.links <- cg.links[-(1:4)]


cg.boxscores.raw <- pblapply(cg.links, function(x) {
  print(x)
  tblA1 <- NULL
  tblA1 <- readLines(x, warn = FALSE)
  # 1999-00 to 2005-06 ######################################
   tbl.area.start <- grep("<pre>",tblA1,ignore.case=TRUE)+1
   tbl.area.end <- grep("</pre>",tblA1,ignore.case=TRUE)-1
  ###########################################################
  # 2006-07 onward ######################################
  # tbl.area.start <- grep("<b>Official Basketball Box Score</b>",tblA1,ignore.case=TRUE)
  # tbl.area.end <- grep("Score by Periods",tblA1,ignore.case=FALSE)+2
  ###########################################################
  if (length(tbl.area.end)==0) {
    tbl.area.end <- grep("OFFICIALS:",tblA1,ignore.case=TRUE)
  }
  tblA1 <- trimws(tblA1[tbl.area.start:tbl.area.end])
  tblA1.df <- data.frame(tblA1,stringsAsFactors = FALSE)
  colnames(tblA1.df) <- "text"
  
  # We assume a consistent format, where the Player grid for SLU starts
  #  3 lines after the second reference of Saints.  Then it ends one
  #  line before the next instance of TEAM...
  plyr.df <- NULL
  oppo.df <- NULL
  teamID <- 'saint-louis'
  opponentID <- NULL
  vs.line <- NULL
  vs.line <- grep("vs Saint Louis",tblA1)[1]
  # 1999-00 to 2005-06 ######################################
  # if (is.na(vs.line)) {
  #  opponentID <- trimws(unlist(strsplit(tblA1[grep("-----------------",tblA1)[1]-2]," vs "))[2])
  #} else {
  #  opponentID <- trimws(unlist(strsplit(tblA1[grep("-----------------",tblA1)[1]-2]," vs "))[1])
  #}
  ###########################################################
  # 2006-07 onward ######################################
  if (is.na(vs.line)) {
    opponentID <- trimws(unlist(strsplit(tblA1[grep(" vs ",tblA1)[1]]," vs "))[2])
  } else {
    opponentID <- trimws(unlist(strsplit(tblA1[grep(" vs ",tblA1)[1]]," vs "))[1])
  }
  ###########################################################
  plyr.grid.start <- grep(": Saint Louis",tblA1)[1] + 3
  gameDate <- as.Date(str_replace_all(substr(tblA1[grep(".m. at",tblA1)[1]],1,8),"/","-"),format="%m-%d-%y")
  if (is.na(gameDate)) {
    gameDate <- as.Date(str_replace_all(substr(tblA1[grep("Noon at",tblA1)[1]],1,8),"/","-"),format="%m-%d-%y")
  }
  if (is.na(gameDate)) {
    gameDate <- as.Date(str_replace_all(substr(tblA1[grep("pm at",tblA1,ignore.case=TRUE)[1]],1,8),"/","-"),format="%m-%d-%y")
  }
  if (is.na(gameDate)) {
    gameDate <- as.Date(str_replace_all(substr(tblA1[grep("am at",tblA1,ignore.case=TRUE)[1]],1,8),"/","-"),format="%m-%d-%y")
  }
  if (substr(trimws(tblA1[grep("Saint Louis",tblA1)][2]),1,8) == "VISITORS") {
    plyr.idx = 1
    oppo.idx = 2 
  }  else {
    plyr.idx = 2
    oppo.idx = 1 
  }
  plyr.grid.end <- grep("TEAM .....",tblA1)[plyr.idx] - 1
  if (is.na(plyr.grid.end)) {
    plyr.grid.end <- grep("TEAM",substr(trimws(tblA1),1,4))[plyr.idx] - 1
  }
  plyr.df <- data.frame(tblA1.df$text[plyr.grid.start:plyr.grid.end])
  colnames(plyr.df) <- "text"

  plyr.df$V1 <- substr(plyr.df$text,1,3)   # 3 Num 
  plyr.df$V2 <- trimws(substr(plyr.df$text,4,24))  # 21 Name
  plyr.df$V3 <- substr(plyr.df$text,25,26) # 2 Pos
  plyr.df$V4 <- as.numeric(substr(plyr.df$text,27,28)) # 2  
  plyr.df$V5 <- as.numeric(substr(plyr.df$text,29,29)) # 1
  plyr.df$V6 <- as.numeric(substr(plyr.df$text,30,32)) # 3
  plyr.df$V7 <- as.numeric(substr(plyr.df$text,33,35)) # 3
  plyr.df$V8 <- as.numeric(substr(plyr.df$text,36,36)) # 1
  plyr.df$V9 <- as.numeric(substr(plyr.df$text,37,39)) # 3
  plyr.df$V10 <- as.numeric(substr(plyr.df$text,40,42)) # 3
  plyr.df$V11 <- as.numeric(substr(plyr.df$text,43,43)) # 1
  plyr.df$V12 <- as.numeric(substr(plyr.df$text,44,46)) # 3
  plyr.df$V13 <- as.numeric(substr(plyr.df$text,47,50)) # 4
  plyr.df$V14 <- as.numeric(substr(plyr.df$text,51,53)) # 3
  plyr.df$V15 <- as.numeric(substr(plyr.df$text,54,55)) # 2
  plyr.df$V16 <- as.numeric(substr(plyr.df$text,56,59)) # 4
  plyr.df$V17 <- as.numeric(substr(plyr.df$text,60,64)) # 4
  plyr.df$V18 <- as.numeric(substr(plyr.df$text,65,67)) # 3
  plyr.df$V19 <- as.numeric(substr(plyr.df$text,68,70)) # 3
  plyr.df$V20 <- as.numeric(substr(plyr.df$text,71,73)) # 3
  plyr.df$V21 <- as.numeric(substr(plyr.df$text,74,75)) # 2
  plyr.df$V22 <- as.numeric(substr(plyr.df$text,76,81)) # 6
  plyr.df$text <- NULL
  
  plyr.df$V2 <- gsub(".","",plyr.df$V2,fixed=TRUE)
  plyr.df$V2 <- trimws(plyr.df$V2,which="right")
  plyr.df$V2 <- sub("(\\w+),\\s(\\w+)","\\2 \\1", plyr.df$V2)
  plyr.df$V2 <- str_to_title(plyr.df$V2)
  plyr.df$V3 <- 0
  plyr.df$V3[1:5] <- 1
  plyr.df$V5 <- NULL
  plyr.df$V8 <- NULL
  plyr.df$V11 <- NULL
  plyr.df$teamID <- teamID
  plyr.df$opponentID <- opponentID
  plyr.df$GameDate <- gameDate
  
  # 4. get opponent grid
  oppo.grid.start <- NULL
  oppo.grid.end <- NULL
  oppo.grid.start <- grep(paste(":",opponentID),tblA1)[1] + 3
  oppo.grid.end <- grep("TEAM .....",tblA1)[oppo.idx] - 1
  if (is.na(oppo.grid.end)) {
    oppo.grid.end <- grep("TEAM",substr(trimws(tblA1),1,4))[oppo.idx] - 1
  }
  oppo.df <- data.frame(tblA1.df$text[oppo.grid.start:oppo.grid.end])
  colnames(oppo.df) <- "text"
  oppo.df$V1 <- substr(oppo.df$text,1,3)   # 3 Num 
  oppo.df$V2 <- trimws(substr(oppo.df$text,4,24))  # 21 Name
  oppo.df$V3 <- substr(oppo.df$text,25,26) # 2 Pos
  oppo.df$V4 <- as.numeric(substr(oppo.df$text,27,28)) # 2  
  oppo.df$V5 <- as.numeric(substr(oppo.df$text,29,29)) # 1
  oppo.df$V6 <- as.numeric(substr(oppo.df$text,30,32)) # 3
  oppo.df$V7 <- as.numeric(substr(oppo.df$text,33,35)) # 3
  oppo.df$V8 <- as.numeric(substr(oppo.df$text,36,36)) # 1
  oppo.df$V9 <- as.numeric(substr(oppo.df$text,37,39)) # 3
  oppo.df$V10 <- as.numeric(substr(oppo.df$text,40,42)) # 3
  oppo.df$V11 <- as.numeric(substr(oppo.df$text,43,43)) # 1
  oppo.df$V12 <- as.numeric(substr(oppo.df$text,44,46)) # 3
  oppo.df$V13 <- as.numeric(substr(oppo.df$text,47,50)) # 4
  oppo.df$V14 <- as.numeric(substr(oppo.df$text,51,53)) # 3
  oppo.df$V15 <- as.numeric(substr(oppo.df$text,54,55)) # 2
  oppo.df$V16 <- as.numeric(substr(oppo.df$text,56,59)) # 4
  oppo.df$V17 <- as.numeric(substr(oppo.df$text,60,64)) # 4
  oppo.df$V18 <- as.numeric(substr(oppo.df$text,65,67)) # 3
  oppo.df$V19 <- as.numeric(substr(oppo.df$text,68,70)) # 3
  oppo.df$V20 <- as.numeric(substr(oppo.df$text,71,73)) # 3
  oppo.df$V21 <- as.numeric(substr(oppo.df$text,74,75)) # 2
  oppo.df$V22 <- as.numeric(substr(oppo.df$text,76,81)) # 6
  oppo.df$text <- NULL
  
  # 5. parse opponent grid
  oppo.df$V2 <- gsub(".","",oppo.df$V2,fixed=TRUE)
  oppo.df$V2 <- trimws(oppo.df$V2,which="right")
  oppo.df$V2 <- sub("(\\w+),\\s(\\w+)","\\2 \\1", oppo.df$V2)
  oppo.df$V2 <- str_to_title(oppo.df$V2)
  oppo.df$V3 <- 0
  oppo.df$V3[1:5] <- 1
  oppo.df$V5 <- NULL
  oppo.df$V8 <- NULL
  oppo.df$V11 <- NULL
  oppo.df$teamID <- opponentID
  oppo.df$opponentID <- teamID
  oppo.df$GameDate <- gameDate
  
  colnames.df <-c("Number","Starters","gs","FG","FGA","X3P","X3PA","FT","FTA","ORB","DRB","TRB","PF","PTS","AST","TOV","BLK","STL","MP","teamID","opponentID","GameDate")
  colnames(oppo.df) <- colnames.df
  colnames(plyr.df) <- colnames.df

  plyr.df["FG."] <- NA
  plyr.df$"FG." <- plyr.df$FG/plyr.df$FGA
  plyr.df["FT."] <- NA
  plyr.df$"FT." <- plyr.df$FT/plyr.df$FTA
  plyr.df["X3P."] <- NA
  plyr.df$"X3P." <- plyr.df$X3P/plyr.df$X3PA
  plyr.df["X2P"] <- NA
  plyr.df["X2PA"] <- NA
  plyr.df["X2P."] <- NA
  plyr.df["TS."] <- NA
  plyr.df["eFG."] <- NA
  plyr.df["X3PAr"] <- NA
  plyr.df["FTr"] <- NA
  plyr.df["ORB."] <- NA
  plyr.df["DRB."] <- NA
  plyr.df["TRB."] <- NA
  plyr.df["AST."] <- NA
  plyr.df["STL."] <- NA
  plyr.df["BLK."] <- NA
  plyr.df["TOV."] <- NA
  plyr.df["USG."] <- NA
  plyr.df["ORtg"] <- NA
  plyr.df["DRtg"] <- NA
  
  plyr.df <- plyr.df[c("Starters","MP","FG","FGA","FG.","X2P","X2PA","X2P.","X3P","X3PA","X3P.","FT","FTA","FT.","ORB","DRB","TRB","AST","STL","BLK","TOV","PF","PTS","TS.","eFG.","X3PAr","FTr","ORB.","DRB.","TRB.","AST.","STL.","BLK.","TOV.","USG.","ORtg","DRtg","gs","teamID","opponentID","GameDate")]
  
  oppo.df["FG."] <- NA
  oppo.df$"FG." <- oppo.df$FG/oppo.df$FGA
  oppo.df["FT."] <- NA
  oppo.df$"FT." <- oppo.df$FT/oppo.df$FTA
  oppo.df["X3P."] <- NA
  oppo.df$"X3P." <- oppo.df$X3P/oppo.df$X3PA
  oppo.df["X2P"] <- NA
  oppo.df["X2PA"] <- NA
  oppo.df["X2P."] <- NA
  oppo.df["TS."] <- NA
  oppo.df["eFG."] <- NA
  oppo.df["X3PAr"] <- NA
  oppo.df["FTr"] <- NA
  oppo.df["ORB."] <- NA
  oppo.df["DRB."] <- NA
  oppo.df["TRB."] <- NA
  oppo.df["AST."] <- NA
  oppo.df["STL."] <- NA
  oppo.df["BLK."] <- NA
  oppo.df["TOV."] <- NA
  oppo.df["USG."] <- NA
  oppo.df["ORtg"] <- NA
  oppo.df["DRtg"] <- NA
  
  oppo.df <- oppo.df[c("Starters","MP","FG","FGA","FG.","X2P","X2PA","X2P.","X3P","X3PA","X3P.","FT","FTA","FT.","ORB","DRB","TRB","AST","STL","BLK","TOV","PF","PTS","TS.","eFG.","X3PAr","FTr","ORB.","DRB.","TRB.","AST.","STL.","BLK.","TOV.","USG.","ORtg","DRtg","gs","teamID","opponentID","GameDate")]

  # 6. write both to table
  dbconn <- dbConnect(MySQL(), user='root', pwd='password'
                      , dbname='hoops_college', host = "127.0.0.1", port=3306)
  dbWriteTable(conn=dbconn, name='player_games', value=plyr.df, row.names=FALSE,overwrite=FALSE,append=TRUE) 
  dbWriteTable(conn=dbconn, name='player_games', value=oppo.df, row.names=FALSE,overwrite=FALSE,append=TRUE) 
  dbDisconnect(dbconn)
  
  # 7. create and assign variables for SLU and opponent grid start and stops (to determine skip and n values)
  # 8. make a loop or function
  # 9. call for two games
  # 10. call for rest of season
  plyr.df 
}) 


# SLU Exceptions
#  these are skipped, at least initially
# 1999-00
grepl("http://www.nmnathletics.com/ViewContent.dbml?DB_OEM_ID=27200&CONTENT_ID=157217",cg.links)
http://www.nmnathletics.com/ViewContent.dbml?DB_OEM_ID=27200&CONTENT_ID=157221
http://www.nmnathletics.com/ViewContent.dbml?DB_OEM_ID=27200&CONTENT_ID=157237
http://www.nmnathletics.com/ViewContent.dbml?DB_OEM_ID=27200&CONTENT_ID=157209
http://www.nmnathletics.com/ViewContent.dbml?DB_OEM_ID=27200&CONTENT_ID=157210
http://www.nmnathletics.com/ViewContent.dbml?DB_OEM_ID=27200&CONTENT_ID=157211
http://www.nmnathletics.com/ViewContent.dbml?DB_OEM_ID=27200&CONTENT_ID=157212

# 2001-02
http://www.nmnathletics.com/ViewContent.dbml?DB_OEM_ID=27200&CONTENT_ID=157250
http://www.nmnathletics.com/ViewContent.dbml?DB_OEM_ID=27200&CONTENT_ID=157253

# 2002-03
http://www.nmnathletics.com/ViewContent.dbml?DB_OEM_ID=27200&CONTENT_ID=157292

#2005-06
http://www.nmnathletics.com/ViewContent.dbml?DB_OEM_ID=27200&CONTENT_ID=157384
http://www.nmnathletics.com/ViewContent.dbml?DB_OEM_ID=27200&CONTENT_ID=157407


# Success List
157249
157251
157263
157283
157299
157347
157366
157379
157393
157397

"2001-11-22"
,"2001-11-24"
,"2002-01-12"
,"2002-11-30"
,"2003-02-05"
,"2004-11-19"
,"2005-02-02"
,"2005-11-22"
,"2006-01-14"
,"2006-01-28"

$ Re-do list - NULL GameDate
Dayton
"2003-12-31" http://www.nmnathletics.com/ViewContent.dbml?DB_OEM_ID=27200&CONTENT_ID=157322
"2006-02-11" http://www.nmnathletics.com/ViewContent.dbml?DB_OEM_ID=27200&CONTENT_ID=157400
"2008-01-30" http://www.nmnathletics.com/ViewContent.dbml?DB_OEM_ID=27200&CONTENT_ID=157463
"2009-01-19" http://www.nmnathletics.com/ViewContent.dbml?DB_OEM_ID=27200&CONTENT_ID=157495
"2010-03-06" http://www.nmnathletics.com/ViewContent.dbml?DB_OEM_ID=27200&CONTENT_ID=157539
"2011-03-02" http://www.nmnathletics.com/ViewContent.dbml?DB_OEM_ID=27200&CONTENT_ID=157576

# Variances found in 1999-2000 data
# about 1 in 5 had an extraneous header with the "matchup" X vs Y.  I was counting
#  on the first vs being the key one.  
#  Solution: ignore <h> tags somehow when looking for vs
tblA1[11] <- "header"

# about 1 in 9 had a shorthand vs with just the school name and not the nickname
#  The main problem here is I want to key on Missouri Tigers for some stuff. Keying on 
#  Missouri alone might introduce some noisy lines.
tblA1[31] <- "Missouri Tigers vs Colorado Buffaloes"

# we had 1 instance all season where the date line was given with slashes / instead
#  of dashes -.  I added a str_replace_all wrapper to that grep so we should be good.
#  About seven came through with NULL GameDate, but I believe that was before
#  I adjusted the grep to look for ".m. at".  This will break if they format the
#  time differently.
tblA1[32] <- "2/02/00 7 p.m. at Boulder, Colo. (Coors Events Center)"  

# They were consistent about VISITORS and HOME TEAM tags, and always putting the 
#  VISITORS grid on top.  But this was another case where sometimes it was shorthand.
#  And occasionally it was styled with a tag.
#  e.g. VISITORS: Missouri Tigers       (expected format)
#       VISITORS: Missouri Tigers 11-3  (not expected, but not problematic)
#       VISITORS: Missouri              (problematic)
#       <b>VISITORS: Missouri Tigers    (problematic)
tblA1[34] <- "VISITORS: Missouri Tigers"
tblA1[54] <- "HOME TEAM: Colorado Buffaloes"

# We had one instance all year where the initial dash separator line was missing
#  So this was ALMOST 100% reliable.  I'm not sure why I am keying off the dashes line
#  to get the opponentID.
tblA1[20] <- "-----------------"




# Get Schedule (actually Results) page
cg.URL <- "http://mutigers.com/schedule.aspx?schedule=64&path=mbball"
cg.pg <- read_html(cg.URL)

cg.HTML <- read_html(cg.URL)
selector_name<-".schedule_game_links"
cg.links <- NULL

cg.links <- cg.HTML %>%
  html_nodes(".schedule_game_links") %>%
  html_nodes("a") %>% html_attr("href")

cg.links <- cg.links[grep("http://stats",cg.links)]


x <- cg.links[1]
x <- "http://stats.mutigers.com/sports/m-baskbl/2006-2007/11507-missbox.html"

cg.boxscores.raw <- pblapply(cg.links, function(x) {
# Maybe this would work
#  - Read the whole page
download.file(x,'game.html')
game.lines <- NULL
game.lines <- readLines('game.html',warn=FALSE)
#  - Get the Date
gameDate <- as.Date(str_replace_all(substr(game.lines[grep("^[0-9]+.[0-9]+.[0-9]+[m* at]",game.lines)[1]],1,8),"/","-"),format="%m-%d-%y")
if (is.na(gameDate)){ 
  warning(paste("Could not find GameDate for ",x))
  # game.lines <- NULL
}
#  - Discard everything except VISITORS to second Team....
boxes.start <- grep("VISITORS: ",game.lines)[1]
boxes.end <- grep("  TEAM.....",game.lines)[2]-1
game.lines <- game.lines[boxes.start:boxes.end]
# strip out HTML tags
game.lines <- gsub("<.*?>", "", game.lines)
#  - Get VISITORS and HOME TEAM
vis.team <- gsub(' -','',gsub('[0-9]+', '',unlist(strsplit(game.lines[grep("VISITORS: ",game.lines)],": "))[2]))
vis.start <- grep("## Player Name",game.lines)[1]
vis.end <- grep("  TEAM.....",game.lines)[1]-1
vis.players <- vis.end-vis.start
home.team <- gsub(' -','',gsub('[0-9]+', '',unlist(strsplit(game.lines[grep("HOME TEAM: ",game.lines)],": "))[2]))
home.start <- grep("## Player Name",game.lines)[2]
home.end <- length(game.lines)
home.players <- home.end-home.start
#  - Append teamID and opponentID?
#     - Have to look up official IDs from Schools if doing this before read.fwf equiv
#  - Discard all remaining except Player lines
game.lines <- game.lines[grep('^[0-9]+ ',game.lines)]
#  - Data.frame it (table it?)
box.df <- dstrfw(game.lines,col_types=plyr.df,widths = c(3,21,2,2,1,3,3,1,3,3,1,3,4,3,2,4,4,4,4,3,2,3))
#  - Add columns
box.df$V2 <- gsub(".","",box.df$V2,fixed=TRUE)
box.df$V2 <- trimws(box.df$V2,which="right")
box.df$V2 <- sub("(\\w+),\\s(\\w+)","\\2 \\1", box.df$V2)
box.df$V2 <- str_to_title(box.df$V2)
box.df$V3 <- 0
box.df$V3[1:5] <- 1
box.df$V3[(vis.players+1):(vis.players+5)] <- 1
box.df$V5 <- NULL
box.df$V8 <- NULL
box.df$V11 <- NULL
box.df$GameDate <- gameDate
box.df$teamID <- 'teamID'
box.df$opponentID <- 'opponentID'
vis.idx <- grep("Missouri",vis.team)
if (!length(vis.idx)) {
  box.df$teamID[1:vis.players] <- vis.team
  box.df$opponentID[1:vis.players] <- 'missouri'
  box.df$teamID[-(1:vis.players)] <- 'missouri'
  box.df$opponentID[-(1:vis.players)] <- vis.team
} else {
  box.df$teamID[1:vis.players] <- 'missouri'
  box.df$opponentID[1:vis.players] <- home.team
  box.df$teamID[-(1:vis.players)] <- home.team
  box.df$opponentID[-(1:vis.players)] <- 'missouri'
}
#  - Reorder
colnames(box.df) <-c("Number","Starters","gs","FG","FGA","X3P","X3PA","FT","FTA","ORB","DRB","TRB","PF","PTS","AST","TOV","BLK","STL","MP","GameDate","teamID","opponentID")
box.df["FG."] <- NA
box.df$"FG." <- box.df$FG/box.df$FGA
box.df["FT."] <- NA
box.df$"FT." <- box.df$FT/box.df$FTA
box.df["X3P."] <- NA
box.df$"X3P." <- box.df$X3P/box.df$X3PA
box.df["X2P"] <- NA
box.df["X2PA"] <- NA
box.df["X2P."] <- NA
box.df["TS."] <- NA
box.df["eFG."] <- NA
box.df["X3PAr"] <- NA
box.df["FTr"] <- NA
box.df["ORB."] <- NA
box.df["DRB."] <- NA
box.df["TRB."] <- NA
box.df["AST."] <- NA
box.df["STL."] <- NA
box.df["BLK."] <- NA
box.df["TOV."] <- NA
box.df["USG."] <- NA
box.df["ORtg"] <- NA
box.df["DRtg"] <- NA
box.df <- box.df[c("Starters","MP","FG","FGA","FG.","X2P","X2PA","X2P.","X3P","X3PA","X3P.","FT","FTA","FT.","ORB","DRB","TRB","AST","STL","BLK","TOV","PF","PTS","TS.","eFG.","X3PAr","FTr","ORB.","DRB.","TRB.","AST.","STL.","BLK.","TOV.","USG.","ORtg","DRtg","gs","teamID","opponentID","GameDate")]

#  - Write
dbconn <- dbConnect(MySQL(), user='root', pwd='password'
                    , dbname='hoops_college', host = "127.0.0.1", port=3306)
dbWriteTable(conn=dbconn, name='player_games', value=box.df, row.names=FALSE,overwrite=FALSE,append=TRUE) 
dbDisconnect(dbconn)
#  - Print something
box.df 
}) 


#2001-02 Skip KU - no gameDate, home/vis may be tagged
"http://stats.mutigers.com/sports/m-baskbl/013002aaa.html"

#2001-02 Skip OU - TEAM?
"http://stats.mutigers.com/sports/m-baskbl/032302aaa.html"

#2003-04 Skip Indiana - dumpster fire
"http://stats.mutigers.com/sports/m-baskbl/120603aaa.html"
#2003-04 Skip OU - dumpster fire
"http://stats.mutigers.com/sports/m-baskbl/011704aaa.html"

#2004-05 Skip Texas A&M - different format
"http://stats.mutigers.com/sports/m-baskbl/020505aaa.html"

#2005-06 Skip Arkansas
"http://stats.mutigers.com/sports/m-baskbl/120205aaa.html"
# Skip Davidson
"http://stats.mutigers.com/sports/m-baskbl/2005-2006/12-07-05.html"
# Skip OU
"http://stats.mutigers.com/sports/m-baskbl/011006aaa.html"

#2006-07 Skip Coppin St
"http://stats.mutigers.com/sports/m-baskbl/112706aaa.html"
# Skip Arkansas
"http://stats.mutigers.com/sports/m-baskbl/120106aaa.html"
# Skip K State
"http://stats.mutigers.com/sports/m-baskbl/2006-2007/ks22.html"
# Skip Iowa St
"http://stats.mutigers.com/sports/m-baskbl/020607aaa.html"

#2007-08 Skip K State
"http://stats.mutigers.com/sports/m-baskbl/2007-2008/ksgame24.html"

#2008-09 Skip first 3 games
"http://stats.mutigers.com/sports/m-baskbl/2008-2009/11-11-08.html"
"http://stats.mutigers.com/sports/m-baskbl/2008-2009/11-15-08.html"
"http://stats.mutigers.com/sports/m-baskbl/2008-2009/11-17-08.html"
# Skip Oral Roberts
"http://stats.mutigers.com/sports/m-baskbl/2008-2009/11-30-08.html"
# And 4 more in a row
"http://stats.mutigers.com/sports/m-baskbl/2008-2009/12-02-08.html"
"http://stats.mutigers.com/sports/m-baskbl/2008-2009/12-07-08.html"
"http://stats.mutigers.com/sports/m-baskbl/2008-2009/12-13-08.html"
"http://stats.mutigers.com/sports/m-baskbl/2008-2009/12-20-08.html"
# and a few others...
# and a bunch more.  maybe got half?

#2009-10 skip the first 3
"http://stats.mutigers.com/sports/m-baskbl/2009-2010/11-17-09.html"
"http://stats.mutigers.com/sports/m-baskbl/2009-2010/11-22-09.html"
"http://stats.mutigers.com/sports/m-baskbl/2009-2010/11-24-09.html"
# and a ton in the middle
# and an NCAA
"http://stats.mutigers.com/sports/m-baskbl/2009-2010/ncaa2.html"

cg.links <- cg.links[11]
cg.links[1]
cg.links

"http://stats.mutigers.com/sports/m-baskbl/2008-2009/02-21-09.html"

cg.links <- cg.links[-9]

tblA1[30:40]

head(game.lines,95)

grep(":30 p.m. at Sullivan Arena -- Anchorage, Ak.",game.lines)
grep("1-02-01 7:00 p.m. at Hearnes Center, Columbia, Mo.",game.lines)
grep("^[0-9]+.[0-9]+.[0-9]+[m* at]",game.lines)
str_replace_all(game.lines(grep("mutigers.com",fixed=TRUE,game.lines)),"[a-z]","")


grep("[<*>]",game.lines[34:67])

game.lines[13] <- "01-28-02 7 p.m. at The Shitbox"
game.lines[3] <- "## Player Name FG-FGA FG-FGA FT-FTA OF DE TOT PF  TP   A TO BLK S MIN"
game.lines[21] <- "## Player Name FG-FGA FG-FGA FT-FTA OF DE TOT PF  TP   A TO BLK S MIN"
game.lines[43] <- "Stephen F. Austin vs Missouri Tigers"
game.lines[71] <- "HOME TEAM: Missouri Tigers 6-0"
game.lines[38] <- "  TEAM........."
game.lines[54] <- "  TEAM........."

game.lines <- game.lines[-11]
game.file <- '/Users/gregrhoades/College Basketball/game.xlsx'
game.lines <- read.xlsx2(game.file,sheetIndex=1)
wd()
game.lines
list.files(, pattern = ".xlsx")

box.df$MP[10] <- 0

box.df$teamID[11:19] <- 'clemson'
box.df$opponentID[11:19] <- 'missouri'

game.file <- '/Users/gregrhoades/Documents/R Stuff/College Basketball/Mizzou Oddball Box Scores.xlsx'
box.df <- NULL
box.df <- read.xlsx2(game.file,sheetIndex=11,stringsAsFactors=FALSE)
box.df

#  - Reorder
colnames(box.df) <-c("Number","Starters","gs","FG","FGA","X3P","X3PA","FT","FTA","ORB","DRB","TRB","PF","PTS","AST","TOV","BLK","STL","MP","GameDate","teamID","opponentID")
box.df$Starters <- gsub(".","",box.df$Starters,fixed=TRUE)
box.df$Starters <- trimws(box.df$Starters,which="right")
box.df$Starters <- sub("(\\w+),\\s(\\w+)","\\2 \\1", box.df$Starters)
box.df$Starters <- str_to_title(box.df$Starters)
box.df["FG."] <- NA
box.df$FG <- as.numeric(box.df$FG)
box.df$FGA <- as.numeric(box.df$FGA)
box.df$FT <- as.numeric(box.df$FT)
box.df$FTA <- as.numeric(box.df$FTA)
box.df$X3P <- as.numeric(box.df$X3P)
box.df$X3PA <- as.numeric(box.df$X3PA)
box.df$"FG." <- box.df$FG/box.df$FGA
box.df["FT."] <- NA
box.df$"FT." <- box.df$FT/box.df$FTA
box.df["X3P."] <- NA
box.df$"X3P." <- box.df$X3P/box.df$X3PA
box.df["X2P"] <- NA
box.df["X2PA"] <- NA
box.df["X2P."] <- NA
box.df["TS."] <- NA
box.df["eFG."] <- NA
box.df["X3PAr"] <- NA
box.df["FTr"] <- NA
box.df["ORB."] <- NA
box.df["DRB."] <- NA
box.df["TRB."] <- NA
box.df["AST."] <- NA
box.df["STL."] <- NA
box.df["BLK."] <- NA
box.df["TOV."] <- NA
box.df["USG."] <- NA
box.df["ORtg"] <- NA
box.df["DRtg"] <- NA
box.df <- box.df[c("Starters","MP","FG","FGA","FG.","X2P","X2PA","X2P.","X3P","X3PA","X3P.","FT","FTA","FT.","ORB","DRB","TRB","AST","STL","BLK","TOV","PF","PTS","TS.","eFG.","X3PAr","FTr","ORB.","DRB.","TRB.","AST.","STL.","BLK.","TOV.","USG.","ORtg","DRtg","gs","teamID","opponentID","GameDate")]

dbconn <- dbConnect(MySQL(), user='root', pwd='password'
                    , dbname='hoops_college', host = "127.0.0.1", port=3306)
dbWriteTable(conn=dbconn, name='player_games', value=box.df, row.names=FALSE,overwrite=FALSE,append=TRUE) 
dbDisconnect(dbconn)

gameDate <- "2007-01-15"

box.df$Starters <- gsub(" ","",box.df$Starters,fixed=TRUE)
box.df$Starters <- trimws(box.df$Starters,which="right")
box.df$Starters <- sub("(\\w+),\\s(\\w+)","\\2 \\1", box.df$Starters)
box.df$Starters <- str_to_title(box.df$Starters)




game.file <- '/Users/gregrhoades/Documents/R Stuff/College Basketball/Mizzou Oddball Box Scores.xlsx'
box.df <- NULL
box.df <- read.xlsx2(game.file,sheetIndex=11,stringsAsFactors=FALSE)
box.df

#  - Reorder
colnames(box.df) <-c("Number","Starters","MP","POS","PTS","FG","FGA","X3P","X3PA","FT","FTA","AST","ORB","DRB","TRB","BLK","STL","TOV","PF","GameDate","teamID","opponentID")

box.df$Starters <- gsub(".","",box.df$Starters,fixed=TRUE)
box.df$Starters <- trimws(box.df$Starters,which="right")
box.df$Starters <- sub("(\\w+),\\s(\\w+)","\\2 \\1", box.df$Starters)
box.df$Starters <- str_to_title(box.df$Starters)
box.df["FG."] <- NA
box.df$FG <- as.numeric(box.df$FG)
box.df$FGA <- as.numeric(box.df$FGA)
box.df$FT <- as.numeric(box.df$FT)
box.df$FTA <- as.numeric(box.df$FTA)
box.df$X3P <- as.numeric(box.df$X3P)
box.df$X3PA <- as.numeric(box.df$X3PA)
box.df$"FG." <- box.df$FG/box.df$FGA
box.df["FT."] <- NA
box.df$"FT." <- box.df$FT/box.df$FTA
box.df["X3P."] <- NA
box.df$"X3P." <- box.df$X3P/box.df$X3PA
box.df["X2P"] <- NA
box.df["X2PA"] <- NA
box.df["X2P."] <- NA
box.df["TS."] <- NA
box.df["eFG."] <- NA
box.df["X3PAr"] <- NA
box.df["FTr"] <- NA
box.df["ORB."] <- NA
box.df["DRB."] <- NA
box.df["TRB."] <- NA
box.df["AST."] <- NA
box.df["STL."] <- NA
box.df["BLK."] <- NA
box.df["TOV."] <- NA
box.df["USG."] <- NA
box.df["ORtg"] <- NA
box.df["DRtg"] <- NA
box.df["gs"] <- NA
box.df <- box.df[c("Starters","MP","FG","FGA","FG.","X2P","X2PA","X2P.","X3P","X3PA","X3P.","FT","FTA","FT.","ORB","DRB","TRB","AST","STL","BLK","TOV","PF","PTS","TS.","eFG.","X3PAr","FTr","ORB.","DRB.","TRB.","AST.","STL.","BLK.","TOV.","USG.","ORtg","DRtg","gs","teamID","opponentID","GameDate")]

dbconn <- dbConnect(MySQL(), user='root', pwd='password'
                    , dbname='hoops_college', host = "127.0.0.1", port=3306)
dbWriteTable(conn=dbconn, name='player_games', value=box.df, row.names=FALSE,overwrite=FALSE,append=TRUE) 
dbDisconnect(dbconn)


#### special to deal with problem records  ############################################################################

x <- "http://www.nmnathletics.com/ViewContent.dbml?DB_OEM_ID=27200&CONTENT_ID=157199"

print(x)
tblA1 <- NULL
tblA1 <- readLines(x, warn = FALSE)
# 1999-00 to 2005-06 ######################################
tbl.area.start <- grep("<pre>",tblA1,ignore.case=TRUE)+1
tbl.area.end <- grep("</pre>",tblA1,ignore.case=TRUE)-1
###########################################################
# 2006-07 onward ######################################
# tbl.area.start <- grep("<b>Official Basketball Box Score</b>",tblA1,ignore.case=TRUE)
# tbl.area.end <- grep("Score by Periods",tblA1,ignore.case=FALSE)+2
###########################################################
if (length(tbl.area.end)==0) {
  tbl.area.end <- grep("OFFICIALS:",tblA1,ignore.case=TRUE)
}
tblA1 <- trimws(tblA1[tbl.area.start:tbl.area.end])
tblA1.df <- data.frame(tblA1,stringsAsFactors = FALSE)
colnames(tblA1.df) <- "text"

# We assume a consistent format, where the Player grid for SLU starts
#  3 lines after the second reference of Saints.  Then it ends one
#  line before the next instance of TEAM...
plyr.df <- NULL
oppo.df <- NULL
teamID <- 'saint-louis'
opponentID <- NULL
vs.line <- NULL
vs.line <- grep("vs Saint Louis",tblA1)[1]
# 1999-00 to 2005-06 ######################################
# if (is.na(vs.line)) {
#  opponentID <- trimws(unlist(strsplit(tblA1[grep("-----------------",tblA1)[1]-2]," vs "))[2])
#} else {
#  opponentID <- trimws(unlist(strsplit(tblA1[grep("-----------------",tblA1)[1]-2]," vs "))[1])
#}
###########################################################
# 2006-07 onward ######################################
if (is.na(vs.line)) {
  opponentID <- trimws(unlist(strsplit(tblA1[grep(" vs ",tblA1)[1]]," vs "))[2])
} else {
  opponentID <- trimws(unlist(strsplit(tblA1[grep(" vs ",tblA1)[1]]," vs "))[1])
}
###########################################################
plyr.grid.start <- grep(": Saint Louis",tblA1)[1] + 3
gameDate <- as.Date(str_replace_all(substr(tblA1[grep(".m. at",tblA1)[1]],1,8),"/","-"),format="%m-%d-%y")
if (is.na(gameDate)) {
  gameDate <- as.Date(str_replace_all(substr(tblA1[grep("Noon at",tblA1)[1]],1,8),"/","-"),format="%m-%d-%y")
}
if (is.na(gameDate)) {
  gameDate <- as.Date(str_replace_all(substr(tblA1[grep("pm at",tblA1,ignore.case=TRUE)[1]],1,8),"/","-"),format="%m-%d-%y")
}
if (is.na(gameDate)) {
  gameDate <- as.Date(str_replace_all(substr(tblA1[grep("am at",tblA1,ignore.case=TRUE)[1]],1,8),"/","-"),format="%m-%d-%y")
}
if (substr(trimws(tblA1[grep("Saint Louis",tblA1)][2]),1,8) == "VISITORS") {
  plyr.idx = 1
  oppo.idx = 2 
}  else {
  plyr.idx = 2
  oppo.idx = 1 
}
plyr.grid.end <- grep("TEAM .....",tblA1)[plyr.idx] - 1
if (is.na(plyr.grid.end)) {
  plyr.grid.end <- grep("TEAM",substr(trimws(tblA1),1,4))[plyr.idx] - 1
}
plyr.df <- data.frame(tblA1.df$text[plyr.grid.start:plyr.grid.end])
colnames(plyr.df) <- "text"

plyr.df$V1 <- substr(plyr.df$text,1,3)   # 3 Num 
plyr.df$V2 <- trimws(substr(plyr.df$text,4,24))  # 21 Name
plyr.df$V3 <- substr(plyr.df$text,25,26) # 2 Pos
plyr.df$V4 <- as.numeric(substr(plyr.df$text,27,28)) # 2  
plyr.df$V5 <- as.numeric(substr(plyr.df$text,29,29)) # 1
plyr.df$V6 <- as.numeric(substr(plyr.df$text,30,32)) # 3
plyr.df$V7 <- as.numeric(substr(plyr.df$text,33,35)) # 3
plyr.df$V8 <- as.numeric(substr(plyr.df$text,36,36)) # 1
plyr.df$V9 <- as.numeric(substr(plyr.df$text,37,39)) # 3
plyr.df$V10 <- as.numeric(substr(plyr.df$text,40,42)) # 3
plyr.df$V11 <- as.numeric(substr(plyr.df$text,43,43)) # 1
plyr.df$V12 <- as.numeric(substr(plyr.df$text,44,46)) # 3
plyr.df$V13 <- as.numeric(substr(plyr.df$text,47,50)) # 4
plyr.df$V14 <- as.numeric(substr(plyr.df$text,51,53)) # 3
plyr.df$V15 <- as.numeric(substr(plyr.df$text,54,55)) # 2
plyr.df$V16 <- as.numeric(substr(plyr.df$text,56,59)) # 4
plyr.df$V17 <- as.numeric(substr(plyr.df$text,60,64)) # 4
plyr.df$V18 <- as.numeric(substr(plyr.df$text,65,67)) # 3
plyr.df$V19 <- as.numeric(substr(plyr.df$text,68,70)) # 3
plyr.df$V20 <- as.numeric(substr(plyr.df$text,71,73)) # 3
plyr.df$V21 <- as.numeric(substr(plyr.df$text,75,76)) 
plyr.df$V22 <- as.numeric(substr(plyr.df$text,77,81)) 
plyr.df$text <- NULL

plyr.df$V2 <- gsub(".","",plyr.df$V2,fixed=TRUE)
plyr.df$V2 <- trimws(plyr.df$V2,which="right")
plyr.df$V2 <- sub("(\\w+),\\s(\\w+)","\\2 \\1", plyr.df$V2)
plyr.df$V2 <- str_to_title(plyr.df$V2)
plyr.df$V3 <- 0
plyr.df$V3[1:5] <- 1
plyr.df$V5 <- NULL
plyr.df$V8 <- NULL
plyr.df$V11 <- NULL
plyr.df$teamID <- teamID
plyr.df$opponentID <- opponentID
plyr.df$GameDate <- gameDate

# 4. get opponent grid
oppo.grid.start <- NULL
oppo.grid.end <- NULL
oppo.grid.start <- grep(paste(":",opponentID),tblA1)[1] + 3
oppo.grid.end <- grep("TEAM .....",tblA1)[oppo.idx] - 1
if (is.na(oppo.grid.end)) {
  oppo.grid.end <- grep("TEAM",substr(trimws(tblA1),1,4))[oppo.idx] - 1
}
oppo.df <- data.frame(tblA1.df$text[oppo.grid.start:oppo.grid.end])
colnames(oppo.df) <- "text"
oppo.df$V1 <- substr(oppo.df$text,1,3)   # 3 Num 
oppo.df$V2 <- trimws(substr(oppo.df$text,4,24))  # 21 Name
oppo.df$V3 <- substr(oppo.df$text,25,26) # 2 Pos
oppo.df$V4 <- as.numeric(substr(oppo.df$text,27,28)) # 2  
oppo.df$V5 <- as.numeric(substr(oppo.df$text,29,29)) # 1
oppo.df$V6 <- as.numeric(substr(oppo.df$text,30,32)) # 3
oppo.df$V7 <- as.numeric(substr(oppo.df$text,33,35)) # 3
oppo.df$V8 <- as.numeric(substr(oppo.df$text,36,36)) # 1
oppo.df$V9 <- as.numeric(substr(oppo.df$text,37,39)) # 3
oppo.df$V10 <- as.numeric(substr(oppo.df$text,40,42)) # 3
oppo.df$V11 <- as.numeric(substr(oppo.df$text,43,43)) # 1
oppo.df$V12 <- as.numeric(substr(oppo.df$text,44,46)) # 3
oppo.df$V13 <- as.numeric(substr(oppo.df$text,47,50)) # 4
oppo.df$V14 <- as.numeric(substr(oppo.df$text,51,53)) # 3
oppo.df$V15 <- as.numeric(substr(oppo.df$text,54,55)) # 2
oppo.df$V16 <- as.numeric(substr(oppo.df$text,56,59)) # 4
oppo.df$V17 <- as.numeric(substr(oppo.df$text,60,64)) # 4
oppo.df$V18 <- as.numeric(substr(oppo.df$text,65,67)) # 3
oppo.df$V19 <- as.numeric(substr(oppo.df$text,68,70)) # 3
oppo.df$V20 <- as.numeric(substr(oppo.df$text,71,73)) # 3
oppo.df$V21 <- as.numeric(substr(oppo.df$text,75,76))
oppo.df$V22 <- as.numeric(substr(oppo.df$text,77,81)) 
oppo.df$text <- NULL

# 5. parse opponent grid
oppo.df$V2 <- gsub(".","",oppo.df$V2,fixed=TRUE)
oppo.df$V2 <- trimws(oppo.df$V2,which="right")
oppo.df$V2 <- sub("(\\w+),\\s(\\w+)","\\2 \\1", oppo.df$V2)
oppo.df$V2 <- str_to_title(oppo.df$V2)
oppo.df$V3 <- 0
oppo.df$V3[1:5] <- 1
oppo.df$V5 <- NULL
oppo.df$V8 <- NULL
oppo.df$V11 <- NULL
oppo.df$teamID <- opponentID
oppo.df$opponentID <- teamID
oppo.df$GameDate <- gameDate

colnames.df <-c("Number","Starters","gs","FG","FGA","X3P","X3PA","FT","FTA","ORB","DRB","TRB","PF","PTS","AST","TOV","BLK","STL","MP","teamID","opponentID","GameDate")
colnames(oppo.df) <- colnames.df
colnames(plyr.df) <- colnames.df

plyr.df["FG."] <- NA
plyr.df$"FG." <- plyr.df$FG/plyr.df$FGA
plyr.df["FT."] <- NA
plyr.df$"FT." <- plyr.df$FT/plyr.df$FTA
plyr.df["X3P."] <- NA
plyr.df$"X3P." <- plyr.df$X3P/plyr.df$X3PA
plyr.df["X2P"] <- NA
plyr.df["X2PA"] <- NA
plyr.df["X2P."] <- NA
plyr.df["TS."] <- NA
plyr.df["eFG."] <- NA
plyr.df["X3PAr"] <- NA
plyr.df["FTr"] <- NA
plyr.df["ORB."] <- NA
plyr.df["DRB."] <- NA
plyr.df["TRB."] <- NA
plyr.df["AST."] <- NA
plyr.df["STL."] <- NA
plyr.df["BLK."] <- NA
plyr.df["TOV."] <- NA
plyr.df["USG."] <- NA
plyr.df["ORtg"] <- NA
plyr.df["DRtg"] <- NA

plyr.df <- plyr.df[c("Starters","MP","FG","FGA","FG.","X2P","X2PA","X2P.","X3P","X3PA","X3P.","FT","FTA","FT.","ORB","DRB","TRB","AST","STL","BLK","TOV","PF","PTS","TS.","eFG.","X3PAr","FTr","ORB.","DRB.","TRB.","AST.","STL.","BLK.","TOV.","USG.","ORtg","DRtg","gs","teamID","opponentID","GameDate")]

oppo.df["FG."] <- NA
oppo.df$"FG." <- oppo.df$FG/oppo.df$FGA
oppo.df["FT."] <- NA
oppo.df$"FT." <- oppo.df$FT/oppo.df$FTA
oppo.df["X3P."] <- NA
oppo.df$"X3P." <- oppo.df$X3P/oppo.df$X3PA
oppo.df["X2P"] <- NA
oppo.df["X2PA"] <- NA
oppo.df["X2P."] <- NA
oppo.df["TS."] <- NA
oppo.df["eFG."] <- NA
oppo.df["X3PAr"] <- NA
oppo.df["FTr"] <- NA
oppo.df["ORB."] <- NA
oppo.df["DRB."] <- NA
oppo.df["TRB."] <- NA
oppo.df["AST."] <- NA
oppo.df["STL."] <- NA
oppo.df["BLK."] <- NA
oppo.df["TOV."] <- NA
oppo.df["USG."] <- NA
oppo.df["ORtg"] <- NA
oppo.df["DRtg"] <- NA

oppo.df <- oppo.df[c("Starters","MP","FG","FGA","FG.","X2P","X2PA","X2P.","X3P","X3PA","X3P.","FT","FTA","FT.","ORB","DRB","TRB","AST","STL","BLK","TOV","PF","PTS","TS.","eFG.","X3PAr","FTr","ORB.","DRB.","TRB.","AST.","STL.","BLK.","TOV.","USG.","ORtg","DRtg","gs","teamID","opponentID","GameDate")]

# 6. write both to table
dbconn <- dbConnect(MySQL(), user='root', pwd='password'
                    , dbname='hoops_college', host = "127.0.0.1", port=3306)
dbWriteTable(conn=dbconn, name='player_games', value=plyr.df, row.names=FALSE,overwrite=FALSE,append=TRUE) 
dbWriteTable(conn=dbconn, name='player_games', value=oppo.df, row.names=FALSE,overwrite=FALSE,append=TRUE) 
dbDisconnect(dbconn)
