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
toUSPGASubset <- toUSPGASubset[toUSPGASubset$Permanent.Tournament.Number < 22,]
tail(toUSPGASubset)
########################################################################################

# Aggregate the data for mining and creating the fitness function

# Aggregate by Player Name, Tournamanet Number, Event Name and show Final Position (99th means missed cut/joint last)
statsByAllPlayerNameAndEventPreMasters <- aggregate(cbind(Finish.Position.numeric.,Total.Strokes,Total.Rounds.,
                                                        Scoring.Avg.Total.Adjustment.,Birdie_Or_Better,Total.Holes.Over.Par,
                                                        TTL.SG.T2G,Total.Putts.Gained,Official.Event.Y.N.)
                                                  ~Event.Name+Permanent.Tournament.Number+Player.Name, data=toUSPGASubset, FUN=sum)
write.csv(statsByAllPlayerNameAndEventPreMasters, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/1.PreMasters/statsByAllPlayerNameAndEventPreMasters.csv", row.names=T)
PlayerFormAllStatsMasters <- statsByAllPlayerNameAndEventPreMasters
# Inspect PlayerFormAllStats for particular players

PlayerFormAllStatsMasters[PlayerFormAllStatsMasters$Player.Name %in% c("Aiken, Thomas", "Thomas, Justin"),c(3,6,7)]
# create a subset of PlayerFormAllStats for Fantasy Golf players only
# see PracticumTournamentNames.r for FGPlayerFormAllStats = xxx
head(FGPlayerFormAllStatsMasters)
# export FGPlayerFormAllStats for inspection
write.csv(FGPlayerFormAllStatsMasters, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/1.PreMasters/FGPlayerFormAllStatsMasters.csv", row.names=T)
# Add "Player_Tournament_No"  after "Permanent.Tournament.Number" to csv (for previous 5, label most recent 5 as 5,5,5,5,5)
# Read it back in after adding "Player_Tournament_No"
FGPlayerFormAllStatsMasters <- read.csv(file=file.choose(), header=T)
head(FGPlayerFormAllStatsMasters)
tail(FGPlayerFormAllStatsMasters)
attach(FGPlayerFormAllStatsMasters)


## This aggregate uses mean to find average finishing position
FGPlayerFormPositionAvgMasters <- aggregate(cbind(Finish.Position.numeric.)~Player.Name, data=FGPlayerFormAllStatsMasters, FUN=mean)
FGPlayerFormPositionAvgMasters[FGPlayerFormPositionAvgMasters$Player.Name==c("Johnson, Dustin"),]
tail(FGPlayerFormPositionAvgMasters)
# FGAvg_Position = PlayerFormPositionAvg$Finish.Position.numeric.
FGAvg_PositionMasters = FGPlayerFormPositionAvgMasters$Finish.Position.numeric.

## These Aggregates use sum - Stroke (score) averages, Scrambling %, Birdie:Bogey ratio
FGPlayerFormYTDMasters <- aggregate(cbind(Total.Strokes, Scoring.Avg.Total.Adjustment.,Total.Rounds.,
                                        Birdie_Or_Better, Total.Holes.Over.Par)
                                  ~Player.Name, data=FGPlayerFormAllStatsMasters, FUN=sum)
FGPlayerFormYTDMasters[FGPlayerFormYTDMasters$Player.Name==c("Kuchar, Matt"),]
# FGAvg_Score = (Total.Strokes + Adjusted.Strokes) / #rounds
FGAvg_ScoreMasters = (FGPlayerFormYTDMasters$Total.Strokes + FGPlayerFormYTDMasters$Scoring.Avg.Total.Adjustment.) / FGPlayerFormYTDMasters$Total.Rounds.
# FGScambling % = Par.or.Better/Missed.GIR
# FGScrambing_RatioUSPGA = (FGPlayerFormYTDMasters$Scrambling.Par.or.Better/FGPlayerFormYTDMasters$Scrambling.Missed.GIR)
# FGBirdie:Bogey Ratio = Birdie.or.Better/Total.Holes.Over.Par
FGBirdieBogey_Ratio = (FGPlayerFormYTDMasters$Birdie_Or_Better/FGPlayerFormYTDMasters$Total.Holes.Over.Par)


############## Shots Gained #############################
FGPlayerFormSGYTDMasters <- aggregate(cbind(TTL.SG.T2G, Total.Putts.Gained)
                                    ~Player.Name, data=FGPlayerFormAllStatsMasters, FUN=mean)
FGPlayerFormSGYTDMastersSD <- aggregate(cbind(FGPlayerFormAllStatsMasters$Finish.Position.numeric.)
                                      ~Player.Name, data=FGPlayerFormAllStatsMasters, FUN=sd)
write.csv(FGPlayerFormSGYTDMasters, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIN/1.PreMasters/FGPlayerFormSGYTDMasters.csv", row.names=T)

FGPlayerFormSGYTDMasters[FGPlayerFormSGYTDMasters$Player.Name=="Scott, Adam",]
# FGSGT2G = Total SGT2G / #rounds
FGSGT2GMasters = FGPlayerFormSGYTDMasters$TTL.SG.T2G
# FGSGPutting = Total SGPutting / #rounds
FGSGPuttingMasters = FGPlayerFormSGYTDMasters$Total.Putts.Gained

# Append Avg_Score, Scrambling_Ratio, BirdieBogey_Ratio, SGT2G, SGPutting to FGPlayerFormYTD
# Append Form Indicator
FGFormIndMasters = (FGAvg_ScoreMasters - FGBirdieBogey_RatioMasters 
                  - FGSGT2GMasters - FGSGPuttingMasters) / FGAvg_PositionMasters
FGPlayerFormYTDMasters[FGPlayerFormYTDMasters$Player.Name=="Koepka, Brooks",]
FGPlayerFormYTDMasters <- cbind(FGPlayerFormYTDMasters, FGAvg_PositionMasters, FGAvg_ScoreMasters, 
                              FGBirdieBogey_RatioMasters,
                              FGSGT2GMasters, FGSGPuttingMasters, FGFormIndMasters)
tail(FGPlayerFormYTDMasters)
FGPlayerFormYTDMasters[FGPlayerFormYTDMasters$Player.Name==c("Johnson, Dustin"),]
write.csv(FGPlayerFormYTDMasters, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/1.PreMasters/FGPlayerFormYTDMasters.csv", row.names=T)
# Open file and add FG_Points for available players and resave - see R folder and PracticumTournamentNames.r
FGPlayerFormYTDMasters$FG_Points <- 0
# Play with FormInd, FormInd2 to create useful form indicator.. this can be edited/used for GA
# e.g. FormIndWithPoints = (FG_Points/200)+FormInd
FGPlayerFormYTDMasters$FGFormIndWithPoints <- 0
FGPlayerFormYTDMasters$FGFormIndWithPoints <- (FGPlayerFormYTDMasters$FG_Points/200) + FGPlayerFormYTDMasters$FGFormIndUSPGA
head(FGPlayerFormYTDMasters)
# set values in PracticumTournamentNames.r
FGPlayerFormYTDMasters$Value <- 0
# export for inspection
write.csv(FGPlayerFormYTDMasters, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/1.PreMasters/FGPlayerFormYTDMasters.csv", row.names=T)



########################################################################################
## load data from ShotlinkIntelligence Extract For Course History
srEventDataMastersHistory5Yr <- read.csv("~/UCD/Business Analytics/Practicum (Dissertation)/Golf/ShotlinkIntelligence/PracticumData/srEventHistory/Masters/srEventMasters5Yr.TXT", sep=";")
summary(srEventDataMastersHistory5Yr)
# Write srEventDataHistory5Yr to csv file
write.csv(srEventDataMastersHistory5Yr, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/CourseHistory/Masters/srEventDataMastersHistory5Yr.csv", row.names=T)
########################################################################################

# Use PracicumCourseHistoryPlayerSubset.r to filter Course History data to FG players
# creating FGTTheOpenHistory5Yr
# Write FGTheOpenHistory5Yr to csv file
write.csv(FGMastersHistory5Yr, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/CourseHistory/Masters/FGMastersHistory5Yr.csv", row.names=T)
# create column for history points
## Top10 - 10 for every year
## 11th-30th - 5 for every year
## 31th-CUTLINE - 2 for every year
## MC/DNP - 0 for every year
FGMastersHistory5Yr$CourseHistoryPts <- 0
FGMastersHistory5Yr$CourseHistoryPts[FGMastersHistory5Yr$Finish.Position.numeric. <= 10] <- 10
FGMastersHistory5Yr$CourseHistoryPts[FGMastersHistory5Yr$Finish.Position.numeric. > 10 
                                   & FGMastersHistory5Yr$Finish.Position.numeric. <= 30] <- 5
FGMastersHistory5Yr$CourseHistoryPts[FGMastersHistory5Yr$Finish.Position.numeric. > 30
                                   & FGMastersHistory5Yr$Finish.Position.numeric. <= 99] <- 2
FGMastersHistory5Yr$CourseHistoryPts[FGMastersHistory5Yr$Finish.Position.numeric.== 999] <- 0
class(FGMastersHistory5Yr$CourseHistoryPts)
# FGTravelersHistory5Yr$CourseHistoryPts <- as.numeric(FGTravelersHistory5Yr$CourseHistoryPts)


# Aggregate by Player Name and show sum CourseHistoryPoints
FGMastersHistory5YrAggPlayerName <- aggregate(FGMastersHistory5Yr$CourseHistoryPts
                                            ~FGMastersHistory5Yr$Player.Name, data=FGMastersHistory5Yr, FUN=sum)
FGMastersHistory5Yr[FGMastersHistory5Yr$Player.Name=="Johnson, Zach", c(8,202)]
tail(FGMastersHistory5YrAggPlayerName)
FGMastersHistory5YrAggPlayerName$PlayerName[FGMastersHistory5YrAggPlayerName$FGMastersHistory5YrAggPlayerName$Player.Name=="Johnson, Zach",]
# create new column and set all values to 0
FGPlayerFormYTDMasters$Course_History <- 0
# export to csv and copy data from FGTravelersHistory5YrAggPlayerName$CourseHistoryPts to FGPlayerFormYTD$CourseHistoryPts
write.csv(FGMastersHistory5YrAggPlayerName, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/CourseHistory/Masters/FGMastersHistory5YrAggPlayerName.csv", row.names=T)
write.csv(FGPlayerFormYTDMasters, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/1.PreMasters/FGPlayerFormYTDMasters.csv", row.names=T)
# re-import FGPlayerFormYTD and check
FGPlayerFormYTDMasters <- read.csv(file=file.choose(), header=T)
head(FGPlayerFormYTDMasters)

# FGPlayerFormYTD$OpenFinishPos <- 0
FGPlayerFormYTDMasters$SG_Total <- FGPlayerFormYTDMasters$FGSGT2GMasters + FGPlayerFormYTDMasters$FGSGPuttingMasters
FGPlayerFormYTDMasters$GA_Form <- 0
### Check to make sure columns are numeric and change if necessary
class(FGPlayerFormYTDMasters$GA_Form)
class(FGPlayerFormYTDMasters$FGFormIndWithPoints)
class(FGPlayerFormYTDMasters$Course_History)
# FGPlayerFormYTD$Course_History <- as.numeric(FGPlayerFormYTD$Course_History)
tail(FGPlayerFormYTDMasters)
FGPlayerFormYTDMasters$GA_Form <-  FGPlayerFormYTDMasters$SG_Total + FGPlayerFormYTDMasters$Course_History
colnames(FGPlayerFormYTDMasters)
# remove a column
# PlayerFormYTD$Player.Name.1 <- NULL
FGPlayerFormYTDMasters[1:5,]
## extra potential GA_Form columns with different weightings for CourseHistory
FGPlayerFormYTDMasters$GA_Form <-  FGPlayerFormYTDMasters$FGFormIndWithPoints + (FGPlayerFormYTDMasters$Course_History/10)
FGPlayerFormYTDMasters$GA_Form2 <- 0
FGPlayerFormYTDMasters$GA_Form2 <-  FGPlayerFormYTDMasters$SG_Total + (FGPlayerFormYTDMasters$Course_History/10)
FGPlayerFormYTDMasters$GA_Form3 <- 0
FGPlayerFormYTDMasters$GA_Form3 <-  FGPlayerFormYTDMasters$FGFormIndWithPoints + (FGPlayerFormYTDMasters$Course_History/6)
head(FGPlayerFormYTDMasters)
tail(FGPlayerFormYTDMasters)
write.csv(FGPlayerFormYTDMasters, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/1.PreMasters/FGPlayerFormYTDMasters.csv", row.names=T)
## Need to update PlayYN column
# Add "PlayYN" after Player.Name
# Y => Golfer playing this week
# N => Golfer not playing this week
# Import Back in
FGPlayerFormYTDMasters <- read.csv(file=file.choose(), header=T)


############ For GA #################
names(FGPlayerFormYTDMasters)
FGPlayerFormYTDMasters[FGPlayerFormYTDMasters$PlayYN=="Y",c(3,17,20,19,21)]
GA_InputMasters <- FGPlayerFormYTDMasters[FGPlayerFormYTDMasters$PlayYN=="Y",c(3,17,20,19,21)]
head(GA_InputMasters)
tail(GA_InputMasters)
write.csv(GA_InputMasters, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/1.PreMasters/GA_InputMasters.csv", row.names=T)
######################################