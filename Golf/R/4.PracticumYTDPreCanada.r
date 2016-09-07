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
write.csv(srEventData2016PreOpen, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/srEventData2016PreOpen.csv", row.names=T)
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
toCanadaSubset <- toUSPGASubset[toUSPGASubset$Permanent.Tournament.Number < 37,]
tail(toCanadaSubset)
########################################################################################

# Aggregate the data for mining and creating the fitness function

# Aggregate by Player Name, Tournamanet Number, Event Name and show Final Position (99th means missed cut/joint last)
statsByAllPlayerNameAndEventPreCan <- aggregate(cbind(Finish.Position.numeric.,Total.Strokes,Total.Rounds.,
                                             Scoring.Avg.Total.Adjustment.,Birdie_Or_Better,Total.Holes.Over.Par,
                                             Scrambling.Par.or.Better,Scrambling.Missed.GIR,
                                             TTL.SG.T2G,Total.Putts.Gained,Official.Event.Y.N.)
                                       ~Event.Name+Permanent.Tournament.Number+Player.Name, data=toCanadaSubset, FUN=sum)
write.csv(statsByAllPlayerNameAndEventPreCan, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/PreCanada/statsByAllPlayerNameAndEventPreCan.csv", row.names=T)
PlayerFormAllStatsCanada <- statsByAllPlayerNameAndEventPreCan
# Inspect PlayerFormAllStats for particular players

PlayerFormAllStatsCanada[PlayerFormAllStatsCanada$Player.Name %in% c("Aiken, Thomas", "Thomas, Justin"),c(3,6,7)]
# create a subset of PlayerFormAllStatsCanada for Fantasy Golf players only
# see PracticumTournamentNames.r for FGPlayerFormAllStats = xxx
tail(FGPlayerFormAllStatsCanada)
# export FGPlayerFormAllStats for inspection
write.csv(FGPlayerFormAllStatsCanada, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/PreCanada/FGPlayerFormAllStatsCanada.csv", row.names=T)
# Add "Player_Tournament_No"  after "Permanent.Tournament.Number" to csv (for previous 5, label most recent 5 as 5,5,5,5,5)
# Read it back in after adding "Player_Tournament_No"
FGPlayerFormAllStatsCanada <- read.csv(file=file.choose(), header=T)
head(FGPlayerFormAllStatsCanada)
tail(FGPlayerFormAllStatsCanada)
attach(FGPlayerFormAllStatsCanada)


## This aggregate uses mean to find average finishing position
FGPlayerFormPositionAvgCan <- aggregate(cbind(Finish.Position.numeric.)~Player.Name, data=FGPlayerFormAllStatsCanada, FUN=mean)
FGPlayerFormPositionAvgCan[FGPlayerFormPositionAvgCan$Player.Name==c("Johnson, Dustin"),]
tail(FGPlayerFormPositionAvgCan)
# FGAvg_Position = PlayerFormPositionAvg$Finish.Position.numeric.
FGAvg_PositionCan = FGPlayerFormPositionAvgCan$Finish.Position.numeric.

## These Aggregates use sum - Stroke (score) averages, Scrambling %, Birdie:Bogey ratio
FGPlayerFormYTDCan <- aggregate(cbind(Total.Strokes, Scoring.Avg.Total.Adjustment.,Total.Rounds.,
                                 Scrambling.Par.or.Better, Scrambling.Missed.GIR,
                                 Birdie_Or_Better, Total.Holes.Over.Par)
                           ~Player.Name, data=FGPlayerFormAllStatsCanada, FUN=sum)
FGPlayerFormYTDCan[FGPlayerFormYTDCan$Player.Name==c("Kuchar, Matt"),]
# FGAvg_Score = (Total.Strokes + Adjusted.Strokes) / #rounds
FGAvg_ScoreCan = (FGPlayerFormYTDCan$Total.Strokes + FGPlayerFormYTDCan$Scoring.Avg.Total.Adjustment.) / FGPlayerFormYTDCan$Total.Rounds.
# FGScambling % = Par.or.Better/Missed.GIR
FGScrambing_RatioCan = (FGPlayerFormYTDCan$Scrambling.Par.or.Better/FGPlayerFormYTDCan$Scrambling.Missed.GIR)
# FGBirdie:Bogey Ratio = Birdie.or.Better/Total.Holes.Over.Par
FGBirdieBogey_RatioCan = (FGPlayerFormYTDCan$Birdie_Or_Better/FGPlayerFormYTDCan$Total.Holes.Over.Par)


############## Shots Gained #############################
FGPlayerFormSGYTDCan <- aggregate(cbind(TTL.SG.T2G, Total.Putts.Gained)
                             ~Player.Name, data=FGPlayerFormAllStatsCanada, FUN=mean)
write.csv(FGPlayerFormSGYTDCan, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIN/PreCanada/FGPlayerFormSGYTDCan.csv", row.names=T)

FGPlayerFormSGYTDCan[FGPlayerFormSGYTDCan$Player.Name=="Scott, Adam",]
# FGSGT2G = Total SGT2G / #rounds
FGSGT2GCan = FGPlayerFormSGYTDCan$TTL.SG.T2G
# FGSGPutting = Total SGPutting / #rounds
FGSGPuttingCan = FGPlayerFormSGYTDCan$Total.Putts.Gained

# Append Avg_Score, Scrambling_Ratio, BirdieBogey_Ratio, SGT2G, SGPutting to FGPlayerFormYTD
# Append Form Indicator
FGFormIndCan = (FGAvg_ScoreCan - FGBirdieBogey_RatioCan - FGSGT2GCan - FGSGPuttingCan) / FGAvg_PositionCan
FGPlayerFormYTDCan[FGPlayerFormYTDCan$Player.Name=="Koepka, Brooks",]
FGPlayerFormYTDCan <- cbind(FGPlayerFormYTDCan, FGAvg_PositionCan, FGAvg_ScoreCan, FGScrambing_RatioCan, FGBirdieBogey_RatioCan,
                         FGSGT2GCan, FGSGPuttingCan, FGFormIndCan)
tail(FGPlayerFormYTDCan)
FGPlayerFormYTDCan[FGPlayerFormYTDCan$Player.Name==c("Johnson, Dustin"),]
write.csv(FGPlayerFormYTDCan, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/PreCanada/FGPlayerFormYTDCan.csv", row.names=T)
# Open file and add FG_Points for available players and resale - see R folder and PracticumTournamentNames.r
FGPlayerFormYTDCan$FG_Points <- 0
# Play with FormInd, FormInd2 to create useful form indicator.. this can be edited/used for GA
# e.g. FormIndWithPoints = (FG_Points/200)+FormInd
FGPlayerFormYTDCan$FGFormIndWithPoints <- 0
FGPlayerFormYTDCan$FGFormIndWithPoints <- (FGPlayerFormYTDCan$FG_Points/200) + FGPlayerFormYTDCan$FGFormIndCan
head(FGPlayerFormYTDCan)
# set values in PracticumTournamentNames.r
FGPlayerFormYTDCan$Value <- 0
# export for inspection
write.csv(FGPlayerFormYTDCan, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/PreCanada/FGPlayerFormYTDCan.csv", row.names=T)



########################################################################################
## load data from ShotlinkIntelligence Extract For Course History
srEventDataCanadaHistory5Yr <- read.csv("~/UCD/Business Analytics/Practicum (Dissertation)/Golf/ShotlinkIntelligence/PracticumData/srEventHistory/Canada/srEventCanadaHistory5Yr.TXT", sep=";")
summary(srEventDataCanadaHistory5Yr)
# Write srEventDataTheOpenHistory5Yr to csv file
write.csv(srEventDataCanadaHistory5Yr, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/CourseHistory/Canada/srEventDataCanadaHistory5Yr.csv", row.names=T)
########################################################################################

# Use PracicumCourseHistoryPlayerSubset to filter Course History data to FG players
# creating FGTTheOpenHistory5Yr
# Write FGTheOpenHistory5Yr to csv file
write.csv(FGCanadaHistory5Yr, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/CourseHistory/Canada/FGCanadaHistory5Yr.csv", row.names=T)
# create column for history points
## Top10 - 10 for every year
## 11th-30th - 5 for every year
## 31th-CUTLINE - 2 for every year
## MC/DNP - 0 for every year
FGCanadaHistory5Yr$CourseHistoryPts <- 0
FGCanadaHistory5Yr$CourseHistoryPts[FGCanadaHistory5Yr$Finish.Position.numeric. <= 10] <- 10
FGCanadaHistory5Yr$CourseHistoryPts[FGCanadaHistory5Yr$Finish.Position.numeric. > 10 
                                       & FGCanadaHistory5Yr$Finish.Position.numeric. <= 30] <- 5
FGCanadaHistory5Yr$CourseHistoryPts[FGCanadaHistory5Yr$Finish.Position.numeric. > 30
                                       & FGCanadaHistory5Yr$Finish.Position.numeric. <= 99] <- 2
FGCanadaHistory5Yr$CourseHistoryPts[FGCanadaHistory5Yr$Finish.Position.numeric.== 999] <- 0
class(FGCanadaHistory5Yr$CourseHistoryPts)
# FGTravelersHistory5Yr$CourseHistoryPts <- as.numeric(FGTravelersHistory5Yr$CourseHistoryPts)


# Aggregate by Player Name and show sum CourseHistoryPoints
FGCanadaHistory5YrAggPlayerName <- aggregate(FGCanadaHistory5Yr$CourseHistoryPts
                                          ~FGCanadaHistory5Yr$Player.Name, data=FGCanadaHistory5Yr, FUN=sum)
FGCanadaHistory5Yr[FGCanadaHistory5Yr$Player.Name=="Johnson, Zach", c(8,202)]
head(FGCanadaHistory5YrAggPlayerName)
FGCanadaHistory5YrAggPlayerName$PlayerName[FGCanadaHistory5YrAggPlayerName$FGCanadaHistory5YrAggPlayerName$Player.Name=="Johnson, Zach",]
# create new column and set all values to 0
FGPlayerFormYTDCan$Course_History <- 0
# export to csv and copy data from FGTravelersHistory5YrAggPlayerName$CourseHistoryPts to FGPlayerFormYTD$CourseHistoryPts
write.csv(FGCanadaHistory5YrAggPlayerName, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/CourseHistory/Canada/FGCanadaHistory5YrAggPlayerName.csv", row.names=T)
write.csv(FGPlayerFormYTDCan, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/PreCanada/FGPlayerFormYTDCan.csv", row.names=T)
# re-import FGPlayerFormYTD and check
FGPlayerFormYTDCan <- read.csv(file=file.choose(), header=T)
head(FGPlayerFormYTDCan)

# FGPlayerFormYTD$OpenFinishPos <- 0
FGPlayerFormYTDCan$GA_Form <- 0
### Check to make sure columns are numeric and change if necessary
class(FGPlayerFormYTDCan$GA_Form)
class(FGPlayerFormYTDCan$FGFormIndWithPoints)
class(FGPlayerFormYTDCan$Course_History)
# FGPlayerFormYTD$Course_History <- as.numeric(FGPlayerFormYTD$Course_History)
head(FGPlayerFormYTDCan)
FGPlayerFormYTDCan$GA_Form <-  FGPlayerFormYTDCan$FGFormIndWithPoints + FGPlayerFormYTDCan$Course_History
colnames(FGPlayerFormYTD)
# remove a column
# PlayerFormYTD$Player.Name.1 <- NULL
FGPlayerFormYTD[1:5,]
write.csv(FGPlayerFormYTD, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/FGPlayerFormYTD.csv", row.names=T)
## extra potential GA_Form columns with different weightings for CourseHistory
FGPlayerFormYTDCan$GA_Form <-  FGPlayerFormYTDCan$FGFormIndWithPoints + (FGPlayerFormYTDCan$Course_History/10)
FGPlayerFormYTDCan$GA_Form2 <- 0
FGPlayerFormYTDCan$GA_Form2 <-  FGPlayerFormYTDCan$FGFormIndWithPoints + (FGPlayerFormYTDCan$Course_History/8)
FGPlayerFormYTDCan$GA_Form3 <- 0
FGPlayerFormYTDCan$GA_Form3 <-  FGPlayerFormYTDCan$FGFormIndWithPoints + (FGPlayerFormYTDCan$Course_History/6)
head(FGPlayerFormYTDCan)
tail(FGPlayerFormYTDCan)
write.csv(FGPlayerFormYTDCan, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/PreCanada/FGPlayerFormYTDCan.csv", row.names=T)
## Update Course_History column using FGTheOpenHistory5YrAggPlayerName (copy and paste)
## Need to update PlayYN column
# Add "PlayYN" after Player.Name
# Y => Golfer playing this week
# N => Golfer not playing this week
# Import Back in
FGPlayerFormYTDCan <- read.csv(file=file.choose(), header=T)


############ For GA ################
GA_InputCan <- FGPlayerFormYTDCan[FGPlayerFormYTDCan$PlayYN=="Y",c(3,20,21,22,23,24,25)]
head(GA_InputCan)
tail(GA_InputCan)
write.csv(GA_InputCan, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/2.PreCanada/GA_InputCan.csv", row.names=T)


###################################################
