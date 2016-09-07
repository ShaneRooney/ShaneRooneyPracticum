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
## load data from ShotlinkIntelligence Extract
srEventData2016ToTravelers <- read.csv("~/UCD/Business Analytics/Practicum (Dissertation)/Golf/ShotlinkIntelligence/PracticumData/srEventTravelers/srEventTravelers.txt", sep=";")
summary(srEventData2016ToTravelers)
# Write srEventData to csv file
write.csv(srEventData2016ToTravelers, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/4.PreTravelers/srEventData2016ToTravelers.csv", row.names=T)
########################################################################################
# Use Deision Trees to understand which features are important to predicting tournament outcomes
# once data has been fixed (minus signs), re-read the event csv for speific event
preUSOpen5WithPosPlayers <- read.csv(file=file.choose(), header=T)
OpenTree <- read.csv(file=file.choose(), header=T)

# read individual events
preUSOpen5 <- srEventData[Event.Name=="THE PLAYERS Championship",c(9,11,15,17,19,21,23,31,33,44,45,50,51,113,189,192,194,196,198,200,203,204)]

length(preUSOpen5$Player.Name)
levels(preUSOpen5$Player.Name)
head(preUSOpen5)
preUSOpen5[1:3, c(1,4,10,20,21)]

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
########################################################################################

# Aggregate the data for mining and creating the fitness function

# Aggregate by Player Name, Tournamanet Number, Event Name and show Final Position (99th means missed cut/joint last)
statsByAllPlayerNameAndEvent <- aggregate(cbind(Finish.Position.numeric.,Total.Strokes,Total.Rounds.,
                                             Scoring.Avg.Total.Adjustment.,Birdie_Or_Better,Total.Holes.Over.Par,
                                             Scrambling.Par.or.Better,Scrambling.Missed.GIR,
                                             TTL.SG.T2G,Total.Putts.Gained,Official.Event.Y.N.)
                                       ~Event.Name+Permanent.Tournament.Number+Player.Name, data=toUSPGASubset, FUN=sum)
write.csv(statsByAllPlayerNameAndEvent, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/statsByAllPlayerNameAndEvent.csv", row.names=T)
# Import file that now includes "Player_Tournament_No" - this will be the basis of subset aggregates for inspection
PlayerFormAllStats <- read.csv(file=file.choose(), header=T)
# Inspect PlayerFormAllStats for particular players

PlayerFormAllStats[PlayerFormAllStats$Player.Name %in% c("Aiken, Thomas", "Thomas, Justin"),c(4,7,8)]
# create a subset of PlayerFormAllStats for Fantasy Golf players only
# see PracticumTournamentNames.r for FGPlayerFormAllStats = xxx
tail(FGPlayerFormAllStats)
# export FGPlayerFormAllStats for inspection
write.csv(FGPlayerFormAllStats, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/FGPlayerFormAllStats.csv", row.names=T)
# Add "Player_Tournament_No"  after "Permanent.Tournament.Number" to csv (for previous 5, label most recent 5 as 5,5,5,5,5)
# Read it back in after adding "Player_Tournament_No"
FGPlayerFormAllStats <- read.csv(file=file.choose(), header=T)
head(FGPlayerFormAllStats)
tail(FGPlayerFormAllStats)
attach(FGPlayerFormAllStats)


## This aggregate uses mean to find average finishing position
FGPlayerFormPositionAvg <- aggregate(cbind(Finish.Position.numeric.)~Player.Name, data=FGPlayerFormAllStats, FUN=mean)
FGPlayerFormPositionAvg[FGPlayerFormPositionAvg$Player.Name==c("Johnson, Dustin"),]
tail(PlayerFormPositionAvg)
# FGAvg_Position = PlayerFormPositionAvg$Finish.Position.numeric.
FGAvg_Position = FGPlayerFormPositionAvg$Finish.Position.numeric.

## These Aggregates use sum - Stroke (score) averages, Scrambling %, Birdie:Bogey ratio
FGPlayerFormYTD <- aggregate(cbind(Total.Strokes, Scoring.Avg.Total.Adjustment.,Total.Rounds.,
                                 Scrambling.Par.or.Better, Scrambling.Missed.GIR,
                                 Birdie_Or_Better, Total.Holes.Over.Par)
                           ~Player.Name, data=FGPlayerFormAllStats, FUN=sum)
FGPlayerFormYTD[FGPlayerFormYTD$Player.Name==c("Kuchar, Matt"),]
# FGAvg_Score = (Total.Strokes + Adjusted.Strokes) / #rounds
FGAvg_Score = (FGPlayerFormYTD$Total.Strokes + FGPlayerFormYTD$Scoring.Avg.Total.Adjustment.) / FGPlayerFormYTD$Total.Rounds.
# FGScambling % = Par.or.Better/Missed.GIR
FGScrambing_Ratio = (FGPlayerFormYTD$Scrambling.Par.or.Better/FGPlayerFormYTD$Scrambling.Missed.GIR)
# FGBirdie:Bogey Ratio = Birdie.or.Better/Total.Holes.Over.Par
FGBirdieBogey_Ratio = (FGPlayerFormYTD$Birdie_Or_Better/FGPlayerFormYTD$Total.Holes.Over.Par)


############## Shots Gained #############################
FGPlayerFormSGYTD <- aggregate(cbind(TTL.SG.T2G, Total.Putts.Gained)
                             ~Player.Name, data=FGPlayerFormAllStats, FUN=mean)
FGPlayerFormSGYTDsd <- aggregate(cbind(FGPlayerFormAllStats$Finish.Position.numeric.)
                               ~Player.Name, data=FGPlayerFormAllStats, FUN=sd)
write.csv(FGPlayerFormSGYTD, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIN/FGPlayerFormSGYTD.csv", row.names=T)

FGPlayerFormSGYTD[FGPlayerFormSGYTD$Player.Name=="Scott, Adam",]
# FGSGT2G = Total SGT2G / #rounds
FGSGT2G = FGPlayerFormSGYTD$TTL.SG.T2G
# FGSGPutting = Total SGPutting / #rounds
FGSGPutting = FGPlayerFormSGYTD$Total.Putts.Gained

# Append Avg_Score, Scrambling_Ratio, BirdieBogey_Ratio, SGT2G, SGPutting to FGPlayerFormYTD
# Append Form Indicator
FGFormInd = (FGAvg_Score - FGBirdieBogey_Ratio - FGSGT2G - FGSGPutting) / FGAvg_Position
FGPlayerFormYTD[FGPlayerFormYTD$Player.Name=="Koepka, Brooks",]
FGPlayerFormYTD <- cbind(FGPlayerFormYTD, FGAvg_Position, FGAvg_Score, FGScrambing_Ratio, FGBirdieBogey_Ratio,
                         FGSGT2G, FGSGPutting, FGFormInd)
tail(FGPlayerFormYTD)
FGPlayerFormYTD[FGPlayerFormYTD$Player.Name==c("Johnson, Dustin"),]
write.csv(FGPlayerFormYTD, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/FGPlayerFormYTD.csv", row.names=T)
# Open file and add FG_Points for available players and resale - see R folder and PracticumTournamentNames.r
FGPlayerFormYTD$FG_Points <- 0
# Play with FormInd, FormInd2 to create useful form indicator.. this can be edited/used for GA
# e.g. FormInd2 = (FG_Points/200)+FormInd
FGPlayerFormYTD$FGFormIndWithPoints <- 0
FGPlayerFormYTD$FGFormIndWithPoints <- (FGPlayerFormYTD$FG_Points/200) + FGPlayerFormYTD$FGFormInd
# set values in PracticumTournamentNames.r
FGPlayerFormYTD$Value <- 0
# export for inspection
write.csv(FGPlayerFormYTD, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/FGPlayerFormYTD.csv", row.names=T)



########################################################################################
## load data from ShotlinkIntelligence Extract For Course History
srEventDataTravelersHistory5Yr <- read.csv("~/UCD/Business Analytics/Practicum (Dissertation)/Golf/ShotlinkIntelligence/PracticumData/srEventHistory/Travelers/srEventTravelersHistory5Yr.TXT", sep=";")
summary(srEventDataTravelersHistory5Yr)
# Write srEventDataTravelersHistory5Yr to csv file
write.csv(srEventDataTravelersHistory5Yr, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/CourseHistory/srEventDataTravelersHistory5Yr.csv", row.names=T)
########################################################################################

# Use PracicumCourseHistoryPlayerSubset to filter Course History data to FG players
# creating FGTravelersHistory5Yr
# Write FGTravelersHistory5Yr to csv file
write.csv(FGTravelersHistory5Yr, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/CourseHistory/FGTravelersHistory5Yr.csv", row.names=T)
# create column for history points
## Top10 - 10 for every year
## 11th-30th - 5 for every year
## 31th-CUTLINE - 2 for every year
## MC/DNP - 0 for every year
FGTravelersHistory5Yr$CourseHistoryPts <- 0
FGTravelersHistory5Yr$CourseHistoryPts[FGTravelersHistory5Yr$Finish.Position.numeric. <= 10] <- 10
FGTravelersHistory5Yr$CourseHistoryPts[FGTravelersHistory5Yr$Finish.Position.numeric. > 10 
                                       & FGTravelersHistory5Yr$Finish.Position.numeric. <= 30] <- 5
FGTravelersHistory5Yr$CourseHistoryPts[FGTravelersHistory5Yr$Finish.Position.numeric. > 30
                                       & FGTravelersHistory5Yr$Finish.Position.numeric. <= 99] <- 2
FGTravelersHistory5Yr$CourseHistoryPts[FGTravelersHistory5Yr$Finish.Position.numeric.== 999] <- 0
class(FGTravelersHistory5Yr$CourseHistoryPts)
# FGTravelersHistory5Yr$CourseHistoryPts <- as.numeric(FGTravelersHistory5Yr$CourseHistoryPts)


# Aggregate by Player Name and show sum CourseHistoryPoints
FGTravelersHistory5YrAggPlayerName <- aggregate(FGTravelersHistory5Yr$CourseHistoryPts
                                          ~FGTravelersHistory5Yr$Player.Name, data=FGTravelersHistory5Yr, FUN=sum)
FGTravelersHistory5Yr[FGTravelersHistory5Yr$Player.Name=="Johnson, Zach", c(8,202)]
head(FGTravelersHistory5YrAggPlayerName)
FGTravelersHistory5YrAggPlayerName[FGTravelersHistory5YrAggPlayerName$FGTravelersHistory5Yr$Player.Name=="Johnson, Zach",]
# export to csv and copy data from FGTravelersHistory5YrAggPlayerName$CourseHistoryPts to FGPlayerFormYTD$CourseHistoryPts
write.csv(FGTravelersHistory5YrAggPlayerName, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/CourseHistory/FGTravelersHistory5YrAggPlayerName.csv", row.names=T)
# re-import FGPlayerFormYTD
# create new column and set all values to 0
FGPlayerFormYTD$Course_History <- 0
# FGPlayerFormYTD$OpenFinishPos <- 0
FGPlayerFormYTD$GA_Form <- 0
### Check to make sure columns are numeric and change if necessary
class(FGPlayerFormYTD$GA_Form)
class(FGPlayerFormYTD$FGFormIndWithPoints)
class(PlayerFormYTD$Course_History)
# FGPlayerFormYTD$Course_History <- as.numeric(FGPlayerFormYTD$Course_History)
FGPlayerFormYTD <- read.csv(file=file.choose(), header=T)
tail(FGPlayerFormYTD)
FGPlayerFormYTD$GA_Form <-  FGPlayerFormYTD$FGFormIndWithPoints +FGPlayerFormYTD$Course_History
colnames(PlayerFormYTD)
# remove a column
# PlayerFormYTD$Player.Name.1 <- NULL
FGPlayerFormYTD[1:5,]
write.csv(FGPlayerFormYTD, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/FGPlayerFormYTD.csv", row.names=T)
## extra potential GA_Form columns with different weightings for CourseHistory
FGPlayerFormYTD$GA_Form2 <- 0
FGPlayerFormYTD$GA_Form2 <-  FGPlayerFormYTD$FGFormIndWithPoints + (FGPlayerFormYTD$Course_History/2)
FGPlayerFormYTD$GA_Form3 <- 0
FGPlayerFormYTD$GA_Form3 <-  FGPlayerFormYTD$FGFormIndWithPoints + (FGPlayerFormYTD$Course_History/3)
head(FGPlayerFormYTD)
write.csv(FGPlayerFormYTD, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/FGPlayerFormYTD.csv", row.names=T)
## Need to update X column
# 1 => Golfer playing this week
# 0 => Golfer not playing this week
# Import Back in
FGPlayerFormYTDTrav <- read.csv(file=file.choose(), header=T)


############ For GA ################
GA_InputTrav <- FGPlayerFormYTDTrav[FGPlayerFormYTDTrav$X==1,c(3,20,22,23,24)]
tail(GA_InputTrav)
write.csv(GA_InputTrav, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/4.PreTravelers/GA_InputTrav.csv", row.names=T)

###################################################
