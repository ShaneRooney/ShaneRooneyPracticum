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
toTheOpenSubset <- toUSPGASubset[toUSPGASubset$Permanent.Tournament.Number < 36,]
tail(toTheOpenSubset)
########################################################################################

# Aggregate the data for mining and creating the fitness function

# Aggregate by Player Name, Tournamanet Number, Event Name and show Final Position (99th means missed cut/joint last)
statsByAllPlayerNameAndEvent <- aggregate(cbind(Finish.Position.numeric.,Total.Strokes,Total.Rounds.,
                                             Scoring.Avg.Total.Adjustment.,Birdie_Or_Better,Total.Holes.Over.Par,
                                             Scrambling.Par.or.Better,Scrambling.Missed.GIR,
                                             TTL.SG.T2G,Total.Putts.Gained,Official.Event.Y.N.)
                                       ~Event.Name+Permanent.Tournament.Number+Player.Name, data=toTheOpenSubset, FUN=sum)
write.csv(statsByAllPlayerNameAndEvent, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/PreTheOpen/statsByAllPlayerNameAndEvent.csv", row.names=T)
PlayerFormAllStats <- statsByAllPlayerNameAndEvent
# Inspect PlayerFormAllStats for particular players

PlayerFormAllStats[PlayerFormAllStats$Player.Name %in% c("Aiken, Thomas", "Thomas, Justin"),c(3,6,7)]
# create a subset of PlayerFormAllStats for Fantasy Golf players only
# see PracticumTournamentNames.r for FGPlayerFormAllStats = xxx
tail(FGPlayerFormAllStats)
# export FGPlayerFormAllStats for inspection
write.csv(FGPlayerFormAllStats, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/PreTheOpen/FGPlayerFormAllStats.csv", row.names=T)
# Add "Player_Tournament_No"  after "Permanent.Tournament.Number" to csv (for previous 5, label most recent 5 as 5,5,5,5,5)
# Read it back in after adding "Player_Tournament_No"
FGPlayerFormAllStats <- read.csv(file=file.choose(), header=T)
head(FGPlayerFormAllStats)
tail(FGPlayerFormAllStats)
attach(FGPlayerFormAllStats)


## This aggregate uses mean to find average finishing position
FGPlayerFormPositionAvg <- aggregate(cbind(Finish.Position.numeric.)~Player.Name, data=FGPlayerFormAllStats, FUN=mean)
FGPlayerFormPositionAvg[FGPlayerFormPositionAvg$Player.Name==c("Johnson, Dustin"),]
tail(FGPlayerFormPositionAvg)
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
write.csv(FGPlayerFormSGYTD, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIN/PreTheOpen/FGPlayerFormSGYTD.csv", row.names=T)

FGPlayerFormSGYTD[FGPlayerFormSGYTD$Player.Name=="Scott, Adam",]
# FGSGT2G = Total SGT2G / #rounds
FGSGT2G = FGPlayerFormSGYTD$TTL.SG.T2G
# FGSGPutting = Total SGPutting / #rounds
FGSGPutting = FGPlayerFormSGYTD$Total.Putts.Gained

# Append Avg_Score, Scrambling_Ratio, BirdieBogey_Ratio, SGT2G, SGPutting to FGPlayerFormYTD
# Append Form Indicator
# FGFormInd = (FGAvg_Score - FGScrambing_Ratio - FGBirdieBogey_Ratio - FGSGT2G - FGSGPutting) / FGAvg_Position
FGFormIndTheOpen = (FGAvg_Score - FGBirdieBogey_Ratio - FGSGT2G - FGSGPutting) / FGAvg_Position
FGPlayerFormYTD[FGPlayerFormYTD$Player.Name=="Koepka, Brooks",]
FGPlayerFormYTD <- cbind(FGPlayerFormYTD, FGAvg_Position, FGAvg_Score, FGScrambing_Ratio, FGBirdieBogey_Ratio,
                         FGSGT2G, FGSGPutting, FGFormInd)
tail(FGPlayerFormYTD)
FGPlayerFormYTD[FGPlayerFormYTD$Player.Name==c("Johnson, Dustin"),]
write.csv(FGPlayerFormYTD, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/PreTheOpen/FGPlayerFormYTD.csv", row.names=T)
# Open file and add FG_Points for available players and resale - see R folder and PracticumTournamentNames.r
FGPlayerFormYTD$FG_Points <- 0
# Play with FormInd, FormInd2 to create useful form indicator.. this can be edited/used for GA
# e.g. FormIndWithPoints = (FG_Points/200)+FormInd
FGPlayerFormYTD$FGFormIndWithPoints <- 0
FGPlayerFormYTD$FGFormIndWithPoints <- (FGPlayerFormYTD$FG_Points/200) + FGPlayerFormYTD$FGFormInd
# set values in PracticumTournamentNames.r
FGPlayerFormYTD$Value <- 0
# export for inspection
write.csv(FGPlayerFormYTD, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/PreTheOpen/FGPlayerFormYTD.csv", row.names=T)



########################################################################################
## load data from ShotlinkIntelligence Extract For Course History
srEventDataTheOpenHistory5Yr <- read.csv("~/UCD/Business Analytics/Practicum (Dissertation)/Golf/ShotlinkIntelligence/PracticumData/srEventHistory/TheOpen/srEventTheOpenHistory5Yr.TXT", sep=";")
summary(srEventDataTheOpenHistory5Yr)
# Write srEventDataTheOpenHistory5Yr to csv file
write.csv(srEventDataTheOpenHistory5Yr, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/CourseHistory/TheOpen/srEventDataTheOpenHistory5Yr.csv", row.names=T)
########################################################################################

# Use PracicumCourseHistoryPlayerSubset to filter Course History data to FG players
# creating FGTTheOpenHistory5Yr
# Write FGTheOpenHistory5Yr to csv file
write.csv(FGTheOpenHistory5Yr, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/CourseHistory/TheOpen/FGTheOpenHistory5Yr.csv", row.names=T)
# create column for history points
## Top10 - 10 for every year
## 11th-30th - 5 for every year
## 31th-CUTLINE - 2 for every year
## MC/DNP - 0 for every year
FGTheOpenHistory5Yr$CourseHistoryPts <- 0
FGTheOpenHistory5Yr$CourseHistoryPts[FGTheOpenHistory5Yr$Finish.Position.numeric. <= 10] <- 10
FGTheOpenHistory5Yr$CourseHistoryPts[FGTheOpenHistory5Yr$Finish.Position.numeric. > 10 
                                       & FGTheOpenHistory5Yr$Finish.Position.numeric. <= 30] <- 5
FGTheOpenHistory5Yr$CourseHistoryPts[FGTheOpenHistory5Yr$Finish.Position.numeric. > 30
                                       & FGTheOpenHistory5Yr$Finish.Position.numeric. <= 99] <- 2
FGTheOpenHistory5Yr$CourseHistoryPts[FGTheOpenHistory5Yr$Finish.Position.numeric.== 999] <- 0
class(FGTheOpenHistory5Yr$CourseHistoryPts)
# FGTravelersHistory5Yr$CourseHistoryPts <- as.numeric(FGTravelersHistory5Yr$CourseHistoryPts)


# Aggregate by Player Name and show sum CourseHistoryPoints
FGTheOpenHistory5YrAggPlayerName <- aggregate(FGTheOpenHistory5Yr$CourseHistoryPts
                                          ~FGTheOpenHistory5Yr$Player.Name, data=FGTheOpenHistory5Yr, FUN=sum)
FGTheOpenHistory5Yr[FGTheOpenHistory5Yr$Player.Name=="Johnson, Zach", c(8,202)]
head(FGTheOpenHistory5YrAggPlayerName)
FGTheOpenHistory5YrAggPlayerName[FGTheOpenHistory5YrAggPlayerName$FGTheOpenHistory5Yr$Player.Name=="Johnson, Zach",]
# export to csv and copy data from FGTravelersHistory5YrAggPlayerName$CourseHistoryPts to FGPlayerFormYTD$CourseHistoryPts
write.csv(FGTheOpenHistory5YrAggPlayerName, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/CourseHistory/TheOpen/FGTheOpenHistory5YrAggPlayerName.csv", row.names=T)
# re-import FGPlayerFormYTD and check
# FGPlayerFormYTD <- read.csv(file=file.choose(), header=T)
head(FGPlayerFormYTD)
# create new column and set all values to 0
FGPlayerFormYTD$Course_History <- 0
# FGPlayerFormYTD$OpenFinishPos <- 0
FGPlayerFormYTD$GA_Form <- 0
### Check to make sure columns are numeric and change if necessary
class(FGPlayerFormYTD$GA_Form)
class(FGPlayerFormYTD$FGFormIndWithPoints)
class(PlayerFormYTD$Course_History)
# FGPlayerFormYTD$Course_History <- as.numeric(FGPlayerFormYTD$Course_History)
head(FGPlayerFormYTD)
FGPlayerFormYTD$GA_Form <-  FGPlayerFormYTD$FGFormIndWithPoints + FGPlayerFormYTD$Course_History
colnames(FGPlayerFormYTD)
# remove a column
# PlayerFormYTD$Player.Name.1 <- NULL
FGPlayerFormYTD[1:5,]
write.csv(FGPlayerFormYTD, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/FGPlayerFormYTD.csv", row.names=T)
## extra potential GA_Form columns with different weightings for CourseHistory
FGPlayerFormYTD$GA_Form <-  FGPlayerFormYTD$FGFormIndWithPoints + (FGPlayerFormYTD$Course_History/10)
FGPlayerFormYTD$GA_Form2 <- 0
FGPlayerFormYTD$GA_Form2 <-  FGPlayerFormYTD$FGFormIndWithPoints + (FGPlayerFormYTD$Course_History/8)
FGPlayerFormYTD$GA_Form3 <- 0
FGPlayerFormYTD$GA_Form3 <-  FGPlayerFormYTD$FGFormIndWithPoints + (FGPlayerFormYTD$Course_History/6)
head(FGPlayerFormYTD)
tail(FGPlayerFormYTD)
write.csv(FGPlayerFormYTD, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/PreTheOpen/FGPlayerFormYTD.csv", row.names=T)
## Update Course_History column using FGTheOpenHistory5YrAggPlayerName (copy and paste)
## Need to update PlayYN column
# Add "PlayYN" after Player.Name
# Y => Golfer playing this week
# N => Golfer not playing this week
# Import Back in
FGPlayerFormYTDTheOpen <- read.csv(file=file.choose(), header=T)



############ For GA ################
GA_InputTheOpen <- FGPlayerFormYTDTheOpen[FGPlayerFormYTDTheOpen$PlayYN=="Y",c(3,20,22,23,24)]
head(GA_InputTheOpen)
tail(GA_InputTheOpen)
write.csv(GA_InputTheOpen, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/1.PreTheOpen/GA_InputTheOpen.csv", row.names=T)


########################################################################################################################
