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
write.csv(toUSPGASubset, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/toUSPGASubsetNEW.csv", row.names=T)
# Check that minus signs are correct - if not trim them and swap them to front of number
# Change 999 to 99 for finish positions
# Read the file in when ready and attach, check to make sure we have the correct features
toUSPGASubset <- read.csv(file=file.choose(), header=T)
attach(toUSPGASubset)
# Remove some events from toUSPGASubset depending on which tournament you need team for
toUSPGASubset <- toUSPGASubsetPreUSPGA ## reset toUSPGASubset back to PreUSPGA data
Birdie_Or_Better = (toUSPGASubset$Birdies + toUSPGASubset$Eagles)
toUSPGASubset = cbind(toUSPGASubset, Birdie_Or_Better)
toUSPGASubset <- toUSPGASubset[toUSPGASubset$Permanent.Tournament.Number < 23,]
tail(toUSPGASubset)
########################################################################################

# Aggregate the data for mining and creating the fitness function

# Aggregate by Player Name, Tournamanet Number, Event Name and show Final Position (99th means missed cut/joint last)
statsByAllPlayerNameAndEventPreHeritage <- aggregate(cbind(Finish.Position.numeric.,Total.Strokes,Total.Rounds.,
                                                          Scoring.Avg.Total.Adjustment.,Birdie_Or_Better,Total.Holes.Over.Par,
                                                          TTL.SG.T2G,Total.Putts.Gained,Official.Event.Y.N.)
                                                    ~Event.Name+Permanent.Tournament.Number+Player.Name, data=toUSPGASubset, FUN=sum)
write.csv(statsByAllPlayerNameAndEventPreHeritage, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/2.PreHeritage/statsByAllPlayerNameAndEventPreHeritage.csv", row.names=T)
PlayerFormAllStatsHeritage <- statsByAllPlayerNameAndEventPreHeritage
# Inspect PlayerFormAllStats for particular players

PlayerFormAllStatsHeritage[PlayerFormAllStatsHeritage$Player.Name %in% c("Aiken, Thomas", "Thomas, Justin"),c(3,6,7)]
# create a subset of PlayerFormAllStats for Fantasy Golf players only
# see PracticumTournamentNames.r for FGPlayerFormAllStats = xxx
head(FGPlayerFormAllStatsHeritage)
# export FGPlayerFormAllStats for inspection
write.csv(FGPlayerFormAllStatsHeritage, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/2.PreHeritage/FGPlayerFormAllStatsHeritage.csv", row.names=T)
# Add "Player_Tournament_No"  after "Permanent.Tournament.Number" to csv (for previous 5, label most recent 5 as 5,5,5,5,5)
# Read it back in after adding "Player_Tournament_No"
FGPlayerFormAllStatsHeritage <- read.csv(file=file.choose(), header=T)
head(FGPlayerFormAllStatsHeritage)
tail(FGPlayerFormAllStatsHeritage)
attach(FGPlayerFormAllStatsHeritage)


## This aggregate uses mean to find average finishing position
FGPlayerFormPositionAvgHeritage <- aggregate(cbind(Finish.Position.numeric.)~Player.Name, data=FGPlayerFormAllStatsHeritage, FUN=mean)
FGPlayerFormPositionAvgHeritage[FGPlayerFormPositionAvgHeritage$Player.Name==c("Johnson, Dustin"),]
tail(FGPlayerFormPositionAvgHeritage)
# FGAvg_Position = PlayerFormPositionAvg$Finish.Position.numeric.
FGAvg_PositionHeritage = FGPlayerFormPositionAvgHeritage$Finish.Position.numeric.

## These Aggregates use sum - Stroke (score) averages, Scrambling %, Birdie:Bogey ratio
FGPlayerFormYTDHeritage <- aggregate(cbind(Total.Strokes, Scoring.Avg.Total.Adjustment.,Total.Rounds.,
                                          Birdie_Or_Better, Total.Holes.Over.Par)
                                    ~Player.Name, data=FGPlayerFormAllStatsHeritage, FUN=sum)
FGPlayerFormYTDHeritage[FGPlayerFormYTDHeritage$Player.Name==c("Kuchar, Matt"),]
# FGAvg_Score = (Total.Strokes + Adjusted.Strokes) / #rounds
FGAvg_ScoreHeritage = (FGPlayerFormYTDHeritage$Total.Strokes + FGPlayerFormYTDHeritage$Scoring.Avg.Total.Adjustment.) / FGPlayerFormYTDHeritage$Total.Rounds.
# FGScambling % = Par.or.Better/Missed.GIR
# FGScrambing_RatioUSPGA = (FGPlayerFormYTDMasters$Scrambling.Par.or.Better/FGPlayerFormYTDMasters$Scrambling.Missed.GIR)
# FGBirdie:Bogey Ratio = Birdie.or.Better/Total.Holes.Over.Par
FGBirdieBogey_Ratio = (FGPlayerFormYTDHeritage$Birdie_Or_Better/FGPlayerFormYTDHeritage$Total.Holes.Over.Par)


############## Shots Gained #############################
FGPlayerFormSGYTDHeritage <- aggregate(cbind(TTL.SG.T2G, Total.Putts.Gained)
                                      ~Player.Name, data=FGPlayerFormAllStatsHeritage, FUN=mean)
write.csv(FGPlayerFormSGYTDHeritage, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIN/2.PreHeritage/FGPlayerFormSGYTDHeritage.csv", row.names=T)

FGPlayerFormSGYTDMasters[FGPlayerFormSGYTDMasters$Player.Name=="Scott, Adam",]
# FGSGT2G = Total SGT2G / #rounds
FGSGT2GHeritage = FGPlayerFormSGYTDHeritage$TTL.SG.T2G
# FGSGPutting = Total SGPutting / #rounds
FGSGPuttingHeritage = FGPlayerFormSGYTDHeritage$Total.Putts.Gained

# Append Avg_Score, Scrambling_Ratio, BirdieBogey_Ratio, SGT2G, SGPutting to FGPlayerFormYTD
# Append Form Indicator
FGFormIndHeritage = (FGAvg_ScoreHeritage - FGBirdieBogey_Ratio 
                    - FGSGT2GHeritage - FGSGPuttingHeritage) / FGAvg_PositionHeritage
FGPlayerFormYTDHeritage[FGPlayerFormYTDHeritage$Player.Name=="Koepka, Brooks",]
FGPlayerFormYTDHeritage <- cbind(FGPlayerFormYTDHeritage, FGAvg_PositionHeritage, FGAvg_ScoreHeritage, 
                                FGBirdieBogey_Ratio,
                                FGSGT2GHeritage, FGSGPuttingHeritage, FGFormIndHeritage)
tail(FGPlayerFormYTDHeritage)
FGPlayerFormYTDHeritage[FGPlayerFormYTDHeritage$Player.Name==c("Johnson, Dustin"),]
write.csv(FGPlayerFormYTDHeritage, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/2.PreHeritage/FGPlayerFormYTDHeritage.csv", row.names=T)
# Open file and add FG_Points for available players and resave - see R folder and PracticumTournamentNames.r
FGPlayerFormYTDHeritage$FG_Points <- 0
# Play with FormInd, FormInd2 to create useful form indicator.. this can be edited/used for GA
# e.g. FormIndWithPoints = (FG_Points/200)+FormInd
FGPlayerFormYTDHeritage$FGFormIndWithPoints <- 0
FGPlayerFormYTDHeritage$FGFormIndWithPoints <- (FGPlayerFormYTDHeritage$FG_Points/200) + FGPlayerFormYTDHeritage$FGFormIndHeritage
head(FGPlayerFormYTDHeritage)
# set values in PracticumTournamentNames.r
FGPlayerFormYTDHeritage$Value <- 0
# export for inspection
write.csv(FGPlayerFormYTDHeritage, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/2.PreHeritage/FGPlayerFormYTDHeritage.csv", row.names=T)



########################################################################################
## load data from ShotlinkIntelligence Extract For Course History
srEventDataHeritageHistory5Yr <- read.csv("~/UCD/Business Analytics/Practicum (Dissertation)/Golf/ShotlinkIntelligence/PracticumData/srEventHistory/Heritage/srEventHeritageHistory5Yr.TXT", sep=";")
summary(srEventDataHeritageHistory5Yr)
# Write srEventDataHistory5Yr to csv file
write.csv(srEventDataHeritageHistory5Yr, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/CourseHistory/Heritage/srEventDataHeritageHistory5Yr.csv", row.names=T)
########################################################################################

# Use PracicumCourseHistoryPlayerSubset.r to filter Course History data to FG players
# creating FGTTheOpenHistory5Yr
# Write FGTheOpenHistory5Yr to csv file
write.csv(FGHeritageHistory5Yr, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/CourseHistory/Heritage/FGHeritageHistory5Yr.csv", row.names=T)
# create column for history points
## Top10 - 10 for every year
## 11th-30th - 5 for every year
## 31th-CUTLINE - 2 for every year
## MC/DNP - 0 for every year
FGHeritageHistory5Yr$CourseHistoryPts <- 0
FGHeritageHistory5Yr$CourseHistoryPts[FGHeritageHistory5Yr$Finish.Position.numeric. <= 10] <- 10
FGHeritageHistory5Yr$CourseHistoryPts[FGHeritageHistory5Yr$Finish.Position.numeric. > 10 
                                     & FGHeritageHistory5Yr$Finish.Position.numeric. <= 30] <- 5
FGHeritageHistory5Yr$CourseHistoryPts[FGHeritageHistory5Yr$Finish.Position.numeric. > 30
                                     & FGHeritageHistory5Yr$Finish.Position.numeric. <= 99] <- 2
FGHeritageHistory5Yr$CourseHistoryPts[FGHeritageHistory5Yr$Finish.Position.numeric.== 999] <- 0
class(FGHeritageHistory5Yr$CourseHistoryPts)
# FGTravelersHistory5Yr$CourseHistoryPts <- as.numeric(FGTravelersHistory5Yr$CourseHistoryPts)


# Aggregate by Player Name and show sum CourseHistoryPoints
FGHeritageHistory5YrAggPlayerName <- aggregate(FGHeritageHistory5Yr$CourseHistoryPts
                                              ~FGHeritageHistory5Yr$Player.Name, data=FGHeritageHistory5Yr, FUN=sum)
FGHeritageHistory5Yr[FGHeritageHistory5Yr$Player.Name=="Johnson, Zach", c(8,202)]
tail(FGHeritageHistory5YrAggPlayerName)
FGHeritageHistory5YrAggPlayerName$PlayerName[FGHeritageHistory5YrAggPlayerName$FGHeritageHistory5YrAggPlayerName$Player.Name=="Johnson, Zach",]
# create new column and set all values to 0
FGPlayerFormYTDHeritage$Course_History <- 0
# export to csv and copy data from FGTravelersHistory5YrAggPlayerName$CourseHistoryPts to FGPlayerFormYTD$CourseHistoryPts
write.csv(FGHeritageHistory5YrAggPlayerName, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/CourseHistory/Heritage/FGHeritageHistory5YrAggPlayerName.csv", row.names=T)
write.csv(FGPlayerFormYTDHeritage, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/2.PreHeritage/FGPlayerFormYTDHeritage.csv", row.names=T)
# re-import FGPlayerFormYTD and check
FGPlayerFormYTDHeritage <- read.csv(file=file.choose(), header=T)
head(FGPlayerFormYTDHeritage)

# FGPlayerFormYTD$OpenFinishPos <- 0
FGPlayerFormYTDHeritage$SG_Total <- FGPlayerFormYTDHeritage$FGSGT2GHeritage + FGPlayerFormYTDHeritage$FGSGPuttingHeritage
FGPlayerFormYTDHeritage$GA_Form <- 0
### Check to make sure columns are numeric and change if necessary
class(FGPlayerFormYTDHeritage$GA_Form)
class(FGPlayerFormYTDHeritage$FGFormIndWithPoints)
class(FGPlayerFormYTDHeritage$Course_History)
# FGPlayerFormYTD$Course_History <- as.numeric(FGPlayerFormYTD$Course_History)
tail(FGPlayerFormYTDHeritage)
FGPlayerFormYTDHeritage$GA_Form <-  FGPlayerFormYTDHeritage$SG_Total + FGPlayerFormYTDHeritage$Course_History
colnames(FGPlayerFormYTDHeritage)
# remove a column
# PlayerFormYTD$Player.Name.1 <- NULL
FGPlayerFormYTDHeritage[1:5,]
## extra potential GA_Form columns with different weightings for CourseHistory
FGPlayerFormYTDHeritage$GA_Form <-  FGPlayerFormYTDHeritage$FGFormIndWithPoints + (FGPlayerFormYTDHeritage$Course_History/10)
FGPlayerFormYTDHeritage$GA_Form2 <- 0
FGPlayerFormYTDHeritage$GA_Form2 <-  FGPlayerFormYTDHeritage$SG_Total + (FGPlayerFormYTDHeritage$Course_History/10)
FGPlayerFormYTDHeritage$GA_Form3 <- 0
FGPlayerFormYTDHeritage$GA_Form3 <-  FGPlayerFormYTDHeritage$FGFormIndWithPoints
head(FGPlayerFormYTDHeritage)
tail(FGPlayerFormYTDHeritage)
write.csv(FGPlayerFormYTDHeritage, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/2.PreHeritage/FGPlayerFormYTDHeritage.csv", row.names=T)
## Need to update PlayYN column
# Add "PlayYN" after Player.Name
# Y => Golfer playing this week
# N => Golfer not playing this week
# Import Back in
FGPlayerFormYTDHeritage <- read.csv(file=file.choose(), header=T)


############ For GA #################
names(FGPlayerFormYTDHeritage)
FGPlayerFormYTDHeritage[FGPlayerFormYTDHeritage$PlayYN=="Y",c(3,18,21,20,23,19)]
GA_InputHeritage <- FGPlayerFormYTDHeritage[FGPlayerFormYTDHeritage$PlayYN=="Y",c(3,18,21,20,23,19)]
head(GA_InputHeritage)
tail(GA_InputHeritage)
write.csv(GA_InputHeritage, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/2.PreHeritage/GA_InputHeritageNEW.csv", row.names=T)
######################################