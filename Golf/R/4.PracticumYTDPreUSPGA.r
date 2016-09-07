### Install any necessary Packages - only needs to be installed once ###
### Can find more packages and R installers at https://www.r-project.org/ ###
### Need to load library whenever you want to use commands ###
install.packages("epiR") # only need to run this once
install.packages("arules")
install.packages("tree")
install.packages("RWeka")
install.packages("partykit")
library(epiR) # need to run this every R session in order to use package
library(tree)
library(RWeka)
library(partykit)

system("java -version")
install.packages('rJava', .libPaths()[1], 'http://www.rforge.net/')
help(package=epiR)

system("java -version")
install.packages('rJava', .libPaths()[1], 'http://www.rforge.net/')

########################################################################################
## load data from ShotlinkIntelligence Extract
srEventData2016ToUSPGA <- read.csv("~/UCD/Business Analytics/Practicum (Dissertation)/Golf/ShotlinkIntelligence/PracticumData/srEventData20016ToUSPGA/srEvent2016ToUSPGA392372.TXT", sep=";")
summary(srEventData2016ToUSPGA)
# Write srEventData to csv file
write.csv(srEventData2016ToUSPGA, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/srEventData2016ToUSPGA.csv", row.names=T)
########################################################################################
########################################################################################
# Now that we have performed some feature reduction, we can load the appropriate features as a subset of the data
toUSPGASubset <- srEventData2016ToUSPGA[,c(4,7,8,10,11,14,16,18,20,22,29,30,31,32,33,34,36,38,39,41,42,43,93,112,113,188,189,191,193,195,197,199)]
tail(toUSPGASubset)

# Give IDs to tournament names - see PracticumTournamentNames.r
levels(toUSPGASubset$Event.Name)
# write to csv for inspection
write.csv(toUSPGASubset, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/toUSPGASubset.csv", row.names=T)
# Check that minus signs are correct - if not trim them and swap them to front of number
# Change 999 to 99 for finish positions
# Read the file in when ready and attach, check to make sure we have the correct features
toUSPGASubset <- read.csv(file=file.choose(), header=T)
attach(toUSPGASubset)
# Remove some events from toUSPGASubset depending on which tournament you need team for
toUSPGASubset <- toUSPGASubset[toUSPGASubset$Permanent.Tournament.Number < 38,]
tail(toUSPGASubset)
########################################################################################

# Aggregate the data for mining and creating the fitness function

# Aggregate by Player Name, Tournamanet Number, Event Name and show Final Position (99th means missed cut/joint last)
statsByAllPlayerNameAndEventPreUSPGA <- aggregate(cbind(Finish.Position.numeric.,Total.Strokes,Total.Rounds.,
                                             Scoring.Avg.Total.Adjustment.,Birdie_Or_Better,Total.Holes.Over.Par,
                                             Scrambling.Par.or.Better,Scrambling.Missed.GIR,
                                             TTL.SG.T2G,Total.Putts.Gained,Official.Event.Y.N.)
                                       ~Event.Name+Permanent.Tournament.Number+Player.Name, data=toUSPGASubset, FUN=sum)
write.csv(statsByAllPlayerNameAndEventPreUSPGA, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/PreUSPGA/statsByAllPlayerNameAndEventPreUSPGA.csv", row.names=T)
PlayerFormAllStatsUSPGA <- statsByAllPlayerNameAndEventPreUSPGA
# Inspect PlayerFormAllStats for particular players

PlayerFormAllStatsUSPGA[PlayerFormAllStatsUSPGA$Player.Name %in% c("Aiken, Thomas", "Thomas, Justin"),c(3,6,7)]
# create a subset of PlayerFormAllStats for Fantasy Golf players only
# see PracticumTournamentNames.r for FGPlayerFormAllStats = xxx
tail(FGPlayerFormAllStatsUSPGA)
# export FGPlayerFormAllStats for inspection
write.csv(FGPlayerFormAllStatsUSPGA, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/PreUSPGA/FGPlayerFormAllStatsUSPGA.csv", row.names=T)
# Add "Player_Tournament_No"  after "Permanent.Tournament.Number" to csv (for previous 5, label most recent 5 as 5,5,5,5,5)
# Read it back in after adding "Player_Tournament_No"
FGPlayerFormAllStatsUSPGA <- read.csv(file=file.choose(), header=T)
head(FGPlayerFormAllStatsUSPGA)
tail(FGPlayerFormAllStatsUSPGA)
attach(FGPlayerFormAllStatsUSPGA)


## This aggregate uses mean to find average finishing position
FGPlayerFormPositionAvgUSPGA <- aggregate(cbind(Finish.Position.numeric.)~Player.Name, data=FGPlayerFormAllStatsUSPGA, FUN=mean)
FGPlayerFormPositionAvgUSPGA[FGPlayerFormPositionAvgUSPGA$Player.Name==c("Johnson, Dustin"),]
tail(FGPlayerFormPositionAvgUSPGA)
# FGAvg_Position = PlayerFormPositionAvg$Finish.Position.numeric.
FGAvg_PositionUSPGA = FGPlayerFormPositionAvgUSPGA$Finish.Position.numeric.

## These Aggregates use sum - Stroke (score) averages, Scrambling %, Birdie:Bogey ratio
FGPlayerFormYTDUSPGA <- aggregate(cbind(Total.Strokes, Scoring.Avg.Total.Adjustment.,Total.Rounds.,
                                 Scrambling.Par.or.Better, Scrambling.Missed.GIR,
                                 Birdie_Or_Better, Total.Holes.Over.Par)
                           ~Player.Name, data=FGPlayerFormAllStatsUSPGA, FUN=sum)
FGPlayerFormYTDUSPGA[FGPlayerFormYTDUSPGA$Player.Name==c("Kuchar, Matt"),]
# FGAvg_Score = (Total.Strokes + Adjusted.Strokes) / #rounds
FGAvg_ScoreUSPGA = (FGPlayerFormYTDUSPGA$Total.Strokes + FGPlayerFormYTDUSPGA$Scoring.Avg.Total.Adjustment.) / FGPlayerFormYTDUSPGA$Total.Rounds.
# FGScambling % = Par.or.Better/Missed.GIR
FGScrambing_RatioUSPGA = (FGPlayerFormYTDUSPGA$Scrambling.Par.or.Better/FGPlayerFormYTDUSPGA$Scrambling.Missed.GIR)
# FGBirdie:Bogey Ratio = Birdie.or.Better/Total.Holes.Over.Par
FGBirdieBogey_RatioUSPGA = (FGPlayerFormYTDUSPGA$Birdie_Or_Better/FGPlayerFormYTDUSPGA$Total.Holes.Over.Par)


############## Shots Gained #############################
FGPlayerFormSGYTDUSPGA <- aggregate(cbind(TTL.SG.T2G, Total.Putts.Gained)
                             ~Player.Name, data=FGPlayerFormAllStatsUSPGA, FUN=mean)
write.csv(FGPlayerFormSGYTDUSPGA, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIN/PreUSPGA/FGPlayerFormSGYTDUSPGA.csv", row.names=T)

FGPlayerFormSGYTDUSPGA[FGPlayerFormSGYTDUSPGA$Player.Name=="Scott, Adam",]
# FGSGT2G = Total SGT2G / #rounds
FGSGT2GUSPGA = FGPlayerFormSGYTDUSPGA$TTL.SG.T2G
# FGSGPutting = Total SGPutting / #rounds
FGSGPuttingUSPGA = FGPlayerFormSGYTDUSPGA$Total.Putts.Gained

# Append Avg_Score, Scrambling_Ratio, BirdieBogey_Ratio, SGT2G, SGPutting to FGPlayerFormYTD
# Append Form Indicator
FGFormIndUSPGA = (FGAvg_ScoreUSPGA - FGBirdieBogey_RatioUSPGA 
                  - FGSGT2GUSPGA - FGSGPuttingUSPGA) / FGAvg_PositionUSPGA
FGPlayerFormYTDUSPGA[FGPlayerFormYTDUSPGA$Player.Name=="Koepka, Brooks",]
FGPlayerFormYTDUSPGA <- cbind(FGPlayerFormYTDUSPGA, FGAvg_PositionUSPGA, FGAvg_ScoreUSPGA, 
                        FGScrambing_RatioUSPGA, FGBirdieBogey_RatioUSPGA,
                         FGSGT2GUSPGA, FGSGPuttingUSPGA, FGFormIndUSPGA)
tail(FGPlayerFormYTDUSPGA)
FGPlayerFormYTDUSPGA[FGPlayerFormYTDUSPGA$Player.Name==c("Johnson, Dustin"),]
write.csv(FGPlayerFormYTDUSPGA, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/PreUSPGA/FGPlayerFormYTDUSPGA.csv", row.names=T)
# Open file and add FG_Points for available players and resale - see R folder and PracticumTournamentNames.r

FGPlayerFormYTDUSPGA$FG_Points <- 0
# Play with FormInd, FormInd2 to create useful form indicator.. this can be edited/used for GA
# e.g. FormIndWithPoints = (FG_Points/200)+FormInd
FGPlayerFormYTDUSPGA$FGFormIndWithPoints <- 0
FGPlayerFormYTDUSPGA$FGFormIndWithPoints <- (FGPlayerFormYTDUSPGA$FG_Points/200) + FGPlayerFormYTDUSPGA$FGFormIndUSPGA
head(FGPlayerFormYTDUSPGA)
# set values in PracticumTournamentNames.r
FGPlayerFormYTDUSPGA$Value <- 0
# export for inspection
write.csv(FGPlayerFormYTDUSPGA, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/PreUSPGA/FGPlayerFormYTDUSPGA.csv", row.names=T)



########################################################################################
## load data from ShotlinkIntelligence Extract For Course History
srEventDataUSPGAHistory5Yr <- read.csv("~/UCD/Business Analytics/Practicum (Dissertation)/Golf/ShotlinkIntelligence/PracticumData/srEventHistory/USPGA/srEventUSPGAHistory5Yr.TXT", sep=";")
summary(srEventDataUSPGAHistory5Yr)
# Write srEventDataHistory5Yr to csv file
write.csv(srEventDataUSPGAHistory5Yr, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/CourseHistory/USPGA/srEventDataUSPGAHistory5Yr.csv", row.names=T)
########################################################################################

# Use PracicumCourseHistoryPlayerSubset.r to filter Course History data to FG players
# creating FGTTheOpenHistory5Yr
# Write FGTheOpenHistory5Yr to csv file
write.csv(FGUSPGAHistory5Yr, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/CourseHistory/USPGA/FGUSPGAHistory5Yr.csv", row.names=T)
# create column for history points
## Top10 - 10 for every year
## 11th-30th - 5 for every year
## 31th-CUTLINE - 2 for every year
## MC/DNP - 0 for every year
FGUSPGAHistory5Yr$CourseHistoryPts <- 0
FGUSPGAHistory5Yr$CourseHistoryPts[FGUSPGAHistory5Yr$Finish.Position.numeric. <= 10] <- 10
FGUSPGAHistory5Yr$CourseHistoryPts[FGUSPGAHistory5Yr$Finish.Position.numeric. > 10 
                                       & FGUSPGAHistory5Yr$Finish.Position.numeric. <= 30] <- 5
FGUSPGAHistory5Yr$CourseHistoryPts[FGUSPGAHistory5Yr$Finish.Position.numeric. > 30
                                       & FGUSPGAHistory5Yr$Finish.Position.numeric. <= 99] <- 2
FGUSPGAHistory5Yr$CourseHistoryPts[FGUSPGAHistory5Yr$Finish.Position.numeric.== 999] <- 0
class(FGUSPGAHistory5Yr$CourseHistoryPts)
# FGTravelersHistory5Yr$CourseHistoryPts <- as.numeric(FGTravelersHistory5Yr$CourseHistoryPts)


# Aggregate by Player Name and show sum CourseHistoryPoints
FGUSPGAHistory5YrAggPlayerName <- aggregate(FGUSPGAHistory5Yr$CourseHistoryPts
                                          ~FGUSPGAHistory5Yr$Player.Name, data=FGUSPGAHistory5Yr, FUN=sum)
FGUSPGAHistory5Yr[FGUSPGAHistory5Yr$Player.Name=="Johnson, Zach", c(8,202)]
head(FGUSPGAHistory5YrAggPlayerName)
FGUSPGAHistory5YrAggPlayerName$PlayerName[FGUSPGAHistory5YrAggPlayerName$FGUSPGAHistory5YrAggPlayerName$Player.Name=="Johnson, Zach",]
# create new column and set all values to 0
FGPlayerFormYTDUSPGA$Course_History <- 0
# export to csv and copy data from FGTravelersHistory5YrAggPlayerName$CourseHistoryPts to FGPlayerFormYTD$CourseHistoryPts
write.csv(FGUSPGAHistory5YrAggPlayerName, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/CourseHistory/USPGA/FGUSPGAHistory5YrAggPlayerName.csv", row.names=T)
write.csv(FGPlayerFormYTDUSPGA, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/PreUSPGA/FGPlayerFormYTDUSPGA.csv", row.names=T)
# re-import FGPlayerFormYTD and check
FGPlayerFormYTDUSPGA <- read.csv(file=file.choose(), header=T)
head(FGPlayerFormYTDUSPGA)

# FGPlayerFormYTD$OpenFinishPos <- 0
FGPlayerFormYTDUSPGA$GA_Form <- 0
### Check to make sure columns are numeric and change if necessary
class(FGPlayerFormYTDUSPGA$GA_Form)
class(FGPlayerFormYTDUSPGA$FGFormIndWithPoints)
class(FGPlayerFormYTDUSPGA$Course_History)
# FGPlayerFormYTD$Course_History <- as.numeric(FGPlayerFormYTD$Course_History)
head(FGPlayerFormYTDUSPGA)
FGPlayerFormYTDUSPGA$GA_Form <-  FGPlayerFormYTDUSPGA$FGFormIndWithPoints + FGPlayerFormYTDUSPGA$Course_History
colnames(FGPlayerFormYTDUSPGA)
# remove a column
# PlayerFormYTD$Player.Name.1 <- NULL
FGPlayerFormYTDUSPGA[1:5,]
write.csv(FGPlayerFormYTD, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/FGPlayerFormYTD.csv", row.names=T)
## extra potential GA_Form columns with different weightings for CourseHistory
FGPlayerFormYTDUSPGA$GA_Form <-  FGPlayerFormYTDUSPGA$FGFormIndWithPoints + (FGPlayerFormYTDUSPGA$Course_History/10)
FGPlayerFormYTDUSPGA$GA_Form2 <- 0
FGPlayerFormYTDUSPGA$GA_Form2 <-  FGPlayerFormYTDUSPGA$FGFormIndWithPoints + (FGPlayerFormYTDUSPGA$Course_History/8)
FGPlayerFormYTDUSPGA$GA_Form3 <- 0
FGPlayerFormYTDUSPGA$GA_Form3 <-  FGPlayerFormYTDUSPGA$FGFormIndWithPoints + (FGPlayerFormYTDUSPGA$Course_History/6)
head(FGPlayerFormYTDUSPGA)
tail(FGPlayerFormYTDUSPGA)
write.csv(FGPlayerFormYTDUSPGA, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/PreUSPGA/FGPlayerFormYTDUSPGA.csv", row.names=T)
## Need to update PlayYN column
# Add "PlayYN" after Player.Name
# Y => Golfer playing this week
# N => Golfer not playing this week
# Import Back in
FGPlayerFormYTDUSPGA <- read.csv(file=file.choose(), header=T)


############ For GA ################
GA_InputUSPGA <- FGPlayerFormYTDUSPGA[FGPlayerFormYTDUSPGA$PlayYN=="Y",c(4,20,21,22,23,24,25)]
head(GA_InputUSPGA)
tail(GA_InputUSPGA)
write.csv(GA_InputUSPGA, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/3.PreUSPGA/GA_InputUSPGA.csv", row.names=T)


###################################################
