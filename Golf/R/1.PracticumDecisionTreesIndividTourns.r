########################################################################################
# Use Deision Trees to understand which features are important to predicting tournament outcomes
# once data has been fixed (minus signs), re-read the event csv for speific event
#srEventData2016ToUSPGA <- read.csv("~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/srEventData2016ToUSPGA.csv", header=TRUE)
srEventData2016ToUSPGA <- read.csv(file=file.choose(), header=T)
# create categories for finishing positions
catFinalPositions <- cut(srEventData2016ToUSPGA$Finish.Position.numeric.,
                         breaks=c(0,25,90,999),
                         labels=c("Top25","26th-CUTLINE","MC"))
Scrambling_Success = (srEventData2016ToUSPGA$Scrambling.Par.or.Better/srEventData2016ToUSPGA$Scrambling.Missed.GIR)
Birdie_Or_Better = (srEventData2016ToUSPGA$Birdies + srEventData2016ToUSPGA$Eagles)
BirdieBogey_Rat = (srEventData2016ToUSPGA$Birdie_Or_Better/srEventData2016ToUSPGA$Total.Holes.Over.Par)
tail(BirdieBogey_Rat)
#PuttInside10_Success = (srEventData2016ToUSPGA$Putting.Inside.10..putts.made./srEventData2016ToUSPGA$Putting.Inside.10..attempts.)
srEventData2016ToUSPGA = cbind(srEventData2016ToUSPGA, Scrambling_Success, BirdieBogey_Rat, catFinalPositions)
srEventData2016ToUSPGA = cbind(srEventData2016ToUSPGA, Birdie_Or_Better)
tail(srEventData2016ToUSPGA)
write.csv(srEventData2016ToUSPGA, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/srEventData2016ToUSPGAWithCatFinalPositions.csv", row.names=T)

# read individual events
srEventDataPlayers <- srEventData2016ToUSPGA[Event.Name=="THE PLAYERS Championship",]
srEventDataMemorial <- srEventData2016ToUSPGA[Event.Name=="the Memorial Tournament presented by Nationwide",]
srEventDataSony <- srEventData2016ToUSPGA[Event.Name=="Sony Open in Hawaii",]
srEventDataDD <- srEventData2016ToUSPGA[Event.Name=="DEAN & DELUCA Invitational",]
srEventDataSandFarm <- srEventData2016ToUSPGA[Event.Name=="Sanderson Farms Championship",]
srEventDataMasters <- srEventData2016ToUSPGA[Event.Name=="Masters Tournament",]
srEventDataPebbleB <- srEventData2016ToUSPGA[Event.Name=="AT&T Pebble Beach Pro-Am",]


#srEventDataPlayers <- srEventData2016ToUSPGA[Event.Name=="THE PLAYERS Championship",c(9,11,15,17,19,21,23,31,33,44,45,50,51,113,189,192,194,196,198,200,203,204)]
length(srEventDataPlayers$Player.Name)
levels(srEventDataPlayers$Player.Name)
head(srEventDataPlayers)
srEventDataPlayers[1:3, c(1,4,10,20,21)]

# create IDs for datasets
sfID <- seq(1,131)
tmID <- seq(1,120)
soID <- seq(1, 144)
ddID <- seq(1,121)
bnID <- seq(1,156)
tpID <- seq(1,144)
MastID <- seq(1,89)
pbID <- seq(1,156)
# create average score per round
tpAvgScorePerRound <- srEventDataPlayers$Total.Strokes/srEventDataPlayers$Total.Rounds.
tmAvgScorePerRound <- srEventDataMemorial$Total.Strokes/srEventDataMemorial$Total.Rounds.
soAvgScorePerRound <- srEventDataSony$Total.Strokes/srEventDataSony$Total.Rounds.
ddAvgScorePerRound <- srEventDataDD$Total.Strokes/srEventDataDD$Total.Rounds.
sfAvgScorePerRound <- srEventDataSandFarm$Total.Strokes/srEventDataSandFarm$Total.Rounds.
pbAvgScorePerRound <- srEventDataPebbleB$Total.Strokes/srEventDataPebbleB$Total.Rounds.

srEventDataPebbleB = cbind(srEventDataPebbleB,pbAvgScorePerRound)
srEventDataPlayers = cbind(srEventDataPlayers,tpAvgScorePerRound)
srEventDataMemorial = cbind(srEventDataMemorial,tmAvgScorePerRound)
srEventDataSony = cbind(srEventDataSony,soAvgScorePerRound)
srEventDataDD = cbind(srEventDataDD,ddAvgScorePerRound)
srEventDataSandFarm = cbind(srEventDataSandFarm,sfAvgScorePerRound)
# write to a file for inspection
write.csv(srEventDataSandFarm, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/IndividualTournaments/srEventDataSandFarm.csv", row.names=T)

library(tree)
library(RWeka)
library(partykit)
# set.seed(101000101)
# ind = sample(2,nrow(dataByMonthTOTAL), replace=TRUE,prob=c(0.7,0.3))
# trainData = data_1516_cluster[ind==1,]
# testData = data_1516_cluster[ind==2,]
summary(srEventDataPlayers)
head(srEventDataPlayers)
srEventDataPlayers[1:4,c(1,2,8,11)]
srEventDataPlayers[tpID == "2",c(1,2,8,11)]
# create training and test datasets
ind = sample(2,nrow(srEventDataPlayers), replace=TRUE,prob=c(0.7,0.3))
trainData = srEventDataPlayers[ind==1,]
testData = srEventDataPlayers[ind==2,]


tail(trainData)
tail(testData)

# Try out various decision trees
# The most useful attributes appear to be Score (obviously), SGPutting, SGT2G, Bogey Avoidance, No. of birdies
trainGolfers_j48 = J48(catFinalPositions ~ ., data = trainData)
trainGolfers_j48 = J48(catFinalPositions ~ Total.Putts.Gained, data = trainData)
trainGolfers_j48 = J48(catFinalPositions ~ TTL.SG.T2G, data = trainData)
trainGolfers_j48 = J48(catFinalPositions ~ OTT.SG.Avg., data = trainData)
trainGolfers_j48 = J48(catFinalPositions ~ APP.SG.Avg., data = trainData)
trainGolfers_j48 = J48(catFinalPositions ~ ARG.SG.Avg., data = trainData)
trainGolfers_j48 = J48(catFinalPositions ~ TTL.SG.Total, data = trainData)
trainGolfers_j48 = J48(catFinalPositions ~ TTL.SG.T2G+Total.Putts.Gained+Total.Putts.Gained, data = trainData)

trainGolfers_j48 = J48(catFinalPositions ~ Bogey.Avoidance.Rank, data = trainData)
trainGolfers_j48 = J48(catFinalPositions ~ Birdie_Or_Better, data = trainData)
trainGolfers_j48 = J48(catFinalPositions ~ Total.Holes.Over.Par, data = trainData)
trainGolfers_j48 = J48(catFinalPositions ~ BirdieBogey_Rat, data = trainData)
trainGolfers_j48 = J48(catFinalPositions ~ Driving.Distance.Total.Distance., data = trainData)
trainGolfers_j48 = J48(catFinalPositions ~ Total.Greens.in.Regulation, data = trainData)
trainGolfers_j48 = J48(catFinalPositions ~ Scrambling_Success, data = trainData)

trainGolfers_j48 = J48(catFinalPositions ~ TTL.SG.T2G+Total.Putts.Gained+BirdieBogey_Rat, data = trainData)
trainGolfers_j48 = J48(catFinalPositions ~ TTL.SG.Total+BirdieBogey_Rat, data = trainData)
trainGolfers_j48 = J48(catFinalPositions ~ TTL.SG.T2G+Total.Putts.Gained+Driving.Distance.Total.Distance., data = trainData)
trainGolfers_j48 = J48(catFinalPositions ~ Total.Greens.in.Regulation+Driving.Distance.Total.Distance., data = trainData)

trainGolfers_j48 = J48(catFinalPositions ~ TTL.SG.T2G+Total.Putts.Gained
                       +Total.Greens.in.Regulation+Driving.Distance.Total.Distance.
                       +Scrambling_Success+BirdieBogey_Rat, data = trainData)
trainGolfers_j48 = J48(catFinalPositions ~ TTL.SG.T2G+Total.Putts.Gained
                       +Scrambling_Success+BirdieBogey_Rat, data = trainData)
trainGolfers_j48 = J48(catFinalPositions ~ TTL.SG.T2G+Total.Putts.Gained
                       +Total.Greens.in.Regulation+Driving.Distance.Total.Distance., data = trainData)
trainGolfers_j48 = J48(catFinalPositions ~ TTL.SG.T2G+Total.Putts.Gained
                       +BirdieBogey_Rat, data = trainData)

trainGolfers_j48
summary((trainGolfers_j48))
plot(trainGolfers_j48)
testGolfers_j48 = evaluate_Weka_classifier(trainGolfers_j48, newdata=testData, numFolds = 0, complexity = FALSE, seed = 1, class = TRUE)
testGolfers_j48

colnames(srEventDataSandFarm)

########################################################################################################
## Check Correlations and Plot
# Read Back in after changing 999 -> 99

## Check SG Correlations and Plot
srEventDataPlayers <- read.csv(file=file.choose(), header=T)
colnames(srEventDataPlayers)

cor(srEventDataPlayers$TTL.SG.T2G, srEventDataPlayers$tpAvgScorePerRound)
cor(srEventDataPlayers$Total.Putts.Gained, srEventDataPlayers$tpAvgScorePerRound)
cor(srEventDataPlayers$TTL.SG.Total, srEventDataPlayers$tpAvgScorePerRound)
plot(srEventDataPlayers$TTL.SG.Total, srEventDataPlayers$tpAvgScorePerRound)
plot(srEventDataPlayers$tpAvgScorePerRound, srEventDataPlayers$TTL.SG.Total)
abline(lm(srEventDataPlayers$TTL.SG.Total~srEventDataPlayers$tpAvgScorePerRound), col="red")
lines(smooth.spline(srEventDataPlayers$TTL.SG.Total~srEventDataPlayers$sfAvgScorePerRound))
plot(srEventDataPlayers$Total.Putts.Gained, srEventDataPlayers$tpAvgScorePerRound)
abline(lm(srEventDataPlayers$tpAvgScorePerRound~srEventDataPlayers$Total.Putts.Gained), col="red")
plot(srEventDataPlayers$TTL.SG.T2G, srEventDataPlayers$tpAvgScorePerRound)
abline(lm(srEventDataPlayers$tpAvgScorePerRound~srEventDataPlayers$TTL.SG.T2G), col="red")

## Check BirdieBoget_Ratio Correlations and Plot
cor(srEventDataPlayers$BirdieBogey_Rat, srEventDataPlayers$tpAvgScorePerRound)
plot(srEventDataPlayers$BirdieBogey_Rat, srEventDataPlayers$tpAvgScorePerRound)
abline(lm(srEventDataPlayers$tpAvgScorePerRound~srEventDataPlayers$BirdieBogey_Rat), col="red")
## Check DrivingDistance Correlation and Plot
cor(srEventDataPlayers$Driving.Distance.Total.Distance., srEventDataPlayers$tpAvgScorePerRound)
plot(srEventDataPlayers$Driving.Distance.Total.Distance., srEventDataPlayers$tpAvgScorePerRound)
## Check GIR Correlation and Plot
cor(srEventDataPlayers$Total.Greens.in.Regulation, srEventDataPlayers$tpAvgScorePerRound)
plot(srEventDataPlayers$Total.Greens.in.Regulation, srEventDataPlayers$tpAvgScorePerRound)
## Check Scrambling Correlation and Plot
cor(srEventDataPlayers$Scrambling_Success, srEventDataPlayers$tpAvgScorePerRound)
plot(srEventDataPlayers$Scrambling_Success, srEventDataPlayers$tpAvgScorePerRound)
## Check correlation between SG & Driving Distance, SG & GIR
cor(srEventDataPlayers$TTL.SG.T2G, srEventDataPlayers$Driving.Distance.Total.Distance.)
plot(srEventDataPlayers$Driving.Distance.Total.Distance., srEventDataPlayers$TTL.SG.T2G)
cor(srEventDataPlayers$TTL.SG.T2G, srEventDataPlayers$Total.Greens.in.Regulation)
plot(srEventDataPlayers$Total.Greens.in.Regulation, srEventDataPlayers$TTL.SG.T2G)

## Correlation Matrix and pairs() for individual tournaments
colnames(srEventDataPlayers)
srEventDataPlayers[1:5,c(10,208,195,57,140)]
pairs(srEventDataPlayers[,c(208,190,193,195,206)], col="blue")
pairs(srEventDataPlayers[,c(208,190,193,195,57,140)], col="blue")
cor(srEventDataPlayers[,c(208,190,193,195,206)])
cor(srEventDataPlayers[,c(208,190,193,195,57,140)])
colnames(srEventDataPebbleB)
cor(srEventDataPebbleB[,c(208,190,193,195,206)])
cor(srEventDataMemorial[,c(208,190,193,195,206)])
cor(srEventDataSony[,c(208,190,193,195,206)])
cor(srEventDataDD[,c(208,190,193,195,206)])
cor(srEventDataSandFarm[,c(208,190,193,195,206)])

cor(srEventDataPebbleB[,c(208,190,193,195,206,57,140)])
cor(srEventDataPebbleB[,c(208,190,193,195,57,140)])
#colnames(FGPlayerFormYTDTravAvgScoreRnd)
#cor(FGPlayerFormYTDTravAvgScoreRnd[,c(26,13,15,16,17,20,22,23)])

## Regression in Individual Tournaments
# head(srEventDataPlayers)
srEventDataPlayers$Finish.Position.numeric.[srEventDataPlayers$Finish.Position.numeric.==999]<-	99
cor(srEventDataPlayers$TTL.SG.Total, srEventDataPlayers$Driving.Acc....Fairways.Hit.)
cor.test(srEventDataPlayers$TTL.SG.Total, srEventDataPlayers$Driving.Acc....Fairways.Hit.)
cor(srEventDataPlayers$TTL.SG.Total, srEventDataPlayers$Total.Greens.in.Regulation)
cor.test(srEventDataPlayers$TTL.SG.Total, srEventDataPlayers$Total.Greens.in.Regulation)
cor(srEventDataPlayers$TTL.SG.T2G, srEventDataPlayers$Total.Putts.Gained)
mod <- lm(srEventDataSandFarm$sfAvgScorePerRound~
            srEventDataSandFarm$TTL.SG.T2G+srEventDataSandFarm$Total.Putts.Gained)
mod <- lm(srEventDataSandFarm$sfAvgScorePerRound~
            srEventDataSandFarm$TTL.SG.Total)
mod <- lm(srEventDataSandFarm$sfAvgScorePerRound~
            srEventDataSandFarm$TTL.SG.Total
          +srEventDataSandFarm$BirdieBogey_Rat)
mod <- lm(srEventDataPebbleB$pbAvgScorePerRound~
            srEventDataPlayers$TTL.SG.T2G+srEventDataPlayers$Total.Putts.Gained
          +srEventDataPlayers$BirdieBogey_Rat)
summary(mod)
plot(mod)




#################################################################################
## Check Correlations and Regression in YTD Data
FGPlayerFormYTDMasters <- read.csv(file=file.choose(), header=T)
tail(FGPlayerFormYTDMasters)
FGPlayerFormYTDMastersAvgScoreRnd <- FGPlayerFormYTDMasters[FGPlayerFormYTDMasters$PlayYN=="Y",]
tail(FGPlayerFormYTDMastersAvgScoreRnd)
write.csv(FGPlayerFormYTDMastersAvgScoreRnd, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/1.PreMasters/FGPlayerFormYTDMastersAvgScoreRnd.csv", row.names=T)
## Add "FG_AvgScoreRnd" to FGPlayerFormYTDXXXAvgScoreRnd and import
## Use srEventData2016ToUSPGAWithCatFinalPositions (Total strokes/No. of rounds)
FGPlayerFormYTDMastersAvgScoreRnd <- read.csv(file=file.choose(), header=T)
FGPlayerFormYTDMastersAvgScoreRnd$FGSG_Total <- 0
FGPlayerFormYTDMastersAvgScoreRnd$FGSG_Total <- FGPlayerFormYTDMastersAvgScoreRnd$FGSGT2G + FGPlayerFormYTDMastersAvgScoreRnd$FGSGPutting
class(FGPlayerFormYTDMastersAvgScoreRnd$FG_AvgScoreRnd)
names(FGPlayerFormYTDMastersAvgScoreRnd)
FGPlayerFormYTDMastersAvgScoreRnd[1:5,c(7,13,14,15,16,17,23)]
pairs(FGPlayerFormYTDMastersAvgScoreRnd[,c(7,13,14,15,16,17,23,21)], col="blue")  ## used in presentation/report
cor(FGPlayerFormYTDMastersAvgScoreRnd[,c(7,13,14,15,16,17,23,21)]) ## use in report
names(FGPlayerFormYTDMastersAvgScoreRnd)
pairs(FGPlayerFormYTDMastersAvgScoreRnd[,c(28,18,19,29,17)], col="blue")
cor(FGPlayerFormYTDMastersAvgScoreRnd[,c(28,18,19,29,17)]) ## use in report

## Regression in YTD
mod <- lm(FGPlayerFormYTDMastersAvgScoreRnd$FG_AvgScoreRnd ~
            FGPlayerFormYTDMastersAvgScoreRnd$FGSGPutting+FGPlayerFormYTDMastersAvgScoreRnd$FGSGT2G)
mod <- lm(FGPlayerFormYTDMastersAvgScoreRnd$FG_AvgScoreRnd ~
            FGPlayerFormYTDMastersAvgScoreRnd$FGSG_Total)
mod <- lm(FGPlayerFormYTDMastersAvgScoreRnd$FG_AvgScoreRnd ~
            FGPlayerFormYTDMastersAvgScoreRnd$FGSG_Total
          +FGPlayerFormYTDMastersAvgScoreRnd$FGBirdieBogey_Ratio)
mod <- lm(FGPlayerFormYTDMastersAvgScoreRnd$FG_AvgScoreRnd~
            FGPlayerFormYTDMastersAvgScoreRnd$GA_Form)
summary(mod)
plot(mod)


# Write YTD data with new columns
write.csv(FGPlayerFormYTDMastersAvgScoreRnd, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/1.PreMasters/FGPlayerFormYTDMastersAvgScoreRnd.csv", row.names=T)
#################################################################################
#################################################################################
## Check Correlations and Regression in YTD Data Masters
FGPlayerFormYTDHeritage <- read.csv(file=file.choose(), header=T)
tail(FGPlayerFormYTDHeritage)
FGPlayerFormYTDHeritageAvgScoreRnd <- FGPlayerFormYTDHeritage[FGPlayerFormYTDHeritage$PlayYN=="Y",]
tail(FGPlayerFormYTDHeritageAvgScoreRnd)
write.csv(FGPlayerFormYTDHeritageAvgScoreRnd, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/2.PreHeritage/FGPlayerFormYTDHeritageAvgScoreRnd.csv", row.names=T)
## Add "FG_AvgScoreRnd" to FGPlayerFormYTDXXXAvgScoreRnd and import
## Use srEventData2016ToUSPGAWithCatFinalPositions (Total strokes/No. of rounds)
FGPlayerFormYTDHeritageAvgScoreRnd <- read.csv(file=file.choose(), header=T)
FGPlayerFormYTDHeritageAvgScoreRnd$FGSG_Total <- 0
FGPlayerFormYTDHeritageAvgScoreRnd$FGSG_Total <- FGPlayerFormYTDHeritageAvgScoreRnd$FGSGT2G + FGPlayerFormYTDHeritageAvgScoreRnd$FGSGPutting
class(FGPlayerFormYTDHeritageAvgScoreRnd$FG_AvgScoreRnd)
names(FGPlayerFormYTDMastersAvgScoreRnd)
FGPlayerFormYTDHeritageAvgScoreRnd[1:5,c(5,13,14,15,16,17,23,22)]
pairs(FGPlayerFormYTDMastersAvgScoreRnd[,c(10,16,17,19,20,18,24)], col="blue")  ## used in presentation/report
cor(FGPlayerFormYTDMastersAvgScoreRnd[,c(10,16,17,19,20,18,24)])  ## used in report/report
names(FGPlayerFormYTDHeritageAvgScoreRnd)
pairs(FGPlayerFormYTDHeritageAvgScoreRnd[,c(28,18,19,29,17)], col="blue")
cor(FGPlayerFormYTDHeritageAvgScoreRnd[,c(6,17,18,16,14,15,23)]) ## use in report

## Regression in YTD
mod <- lm(FGPlayerFormYTDMastersAvgScoreRnd$FG_AvgScoreRnd ~
            FGPlayerFormYTDMastersAvgScoreRnd$FGSGPutting+FGPlayerFormYTDMastersAvgScoreRnd$FGSGT2G)
mod <- lm(FGPlayerFormYTDMastersAvgScoreRnd$FG_AvgScoreRnd ~
            FGPlayerFormYTDMastersAvgScoreRnd$FGSG_Total)
mod <- lm(FGPlayerFormYTDMastersAvgScoreRnd$FG_AvgScoreRnd ~
            FGPlayerFormYTDMastersAvgScoreRnd$FGSG_Total
          +FGPlayerFormYTDMastersAvgScoreRnd$FGBirdieBogey_Ratio)
mod <- lm(FGPlayerFormYTDMastersAvgScoreRnd$FG_AvgScoreRnd~
            FGPlayerFormYTDMastersAvgScoreRnd$FGAvg_ScoreMasters
          )
mod <- lm(FGPlayerFormYTDMastersAvgScoreRnd$FG_AvgScoreRnd~
            FGPlayerFormYTDMastersAvgScoreRnd$GA_Form)
summary(mod)
plot(mod)


# Write YTD data with new columns
write.csv(FGPlayerFormYTDMastersAvgScoreRnd, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/1.PreMasters/FGPlayerFormYTDMastersAvgScoreRnd.csv", row.names=T)
#################################################################################

## Check Correlations and Regression in YTD Data
FGPlayerFormYTDCan <- read.csv(file=file.choose(), header=T)
tail(FGPlayerFormYTDCan)
FGPlayerFormYTDCanAvgScoreRndAvgScoreRnd <- FGPlayerFormYTDCan[FGPlayerFormYTDCan$PlayYN=="Y",]
tail(FGPlayerFormYTDCan)
write.csv(FGPlayerFormYTDCanAvgScoreRndAvgScoreRnd, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/2.PreCanada/FGPlayerFormYTDCanAvgScoreRndAvgScoreRnd.csv", row.names=T)
## Add "FG_AvgScoreRnd" to FGPlayerFormYTDXXXAvgScoreRnd and import
## Use srEventData2016ToUSPGAWithCatFinalPositions (Total strokes/No. of rounds)
FGPlayerFormYTDCanAvgScoreRndAvgScoreRnd <- read.csv(file=file.choose(), header=T)
FGPlayerFormYTDCanAvgScoreRndAvgScoreRnd$FGSG_Total <- 0
FGPlayerFormYTDCanAvgScoreRndAvgScoreRnd$FGSG_Total <- FGPlayerFormYTDCanAvgScoreRndAvgScoreRnd$FGSGT2G + FGPlayerFormYTDCanAvgScoreRndAvgScoreRnd$FGSGPutting
class(FGPlayerFormYTDCanAvgScoreRndAvgScoreRnd$FG_AvgScoreRnd)
FGPlayerFormYTDCanAvgScoreRndAvgScoreRnd[1:5,c(26,13,15,16,17,28,20,22,23)]
pairs(FGPlayerFormYTDCanAvgScoreRndAvgScoreRnd[,c(28,14,15,17,18,19,29,24,25)], col="blue")  ## used in presentation/report
cor(FGPlayerFormYTDCanAvgScoreRnd[,c(28,14,15,17,18,19,29,24,25)]) ## use in report
names(FGPlayerFormYTDCanAvgScoreRndAvgScoreRnd)
pairs(FGPlayerFormYTDCanAvgScoreRndAvgScoreRnd[,c(28,18,19,29,17)], col="blue")
cor(FGPlayerFormYTDCanAvgScoreRnd[,c(28,18,19,29,17)]) ## use in report

## Regression in YTD
mod <- lm(FGPlayerFormYTDTheOpenAvgScoreRnd$FG_AvgScoreRnd ~
            FGPlayerFormYTDTheOpenAvgScoreRnd$FGSGPutting+FGPlayerFormYTDTheOpenAvgScoreRnd$FGSGT2G)
mod <- lm(FGPlayerFormYTDTheOpenAvgScoreRnd$FG_AvgScoreRnd ~
            FGPlayerFormYTDTheOpenAvgScoreRnd$FGSG_Total)
mod <- lm(FGPlayerFormYTDTheOpenAvgScoreRnd$FG_AvgScoreRnd ~
            FGPlayerFormYTDTheOpenAvgScoreRnd$FGSG_Total
          +FGPlayerFormYTDTheOpenAvgScoreRnd$FGBirdieBogey_Ratio)
mod <- lm(FGPlayerFormYTDTheOpenAvgScoreRnd$FG_AvgScoreRnd~
            FGPlayerFormYTDTheOpenAvgScoreRnd$GA_Form)
summary(mod)
plot(mod)

# Write YTD data with new columns
write.csv(FGPlayerFormYTDTheOpenAvgScoreRnd, "~/UCD/Business Analytics/Practicum (Dissertation)/Golf/CSVData/HandIn/1.PreTheOpen/FGPlayerFormYTDTheOpenAvgScoreRnd.csv", row.names=T)
##################################################################################
## Check Decision Trees in YTD Data Heritage
library(tree)
library(RWeka)
library(partykit)
## make sure correct "catFinalPositions" has been added.. can copy and paste from "srEventData2016ToUSPGAWithCatFinalPositions"
FGPlayerFormYTDHeritageAvgScoreRnd <- read.csv(file=file.choose(), header=T)
tail(FGPlayerFormYTDHeritageAvgScoreRnd)
tail(FGPlayerFormYTDHeritageAvgScoreRnd)
FGPlayerFormYTDHeritageAvgScoreRnd[1:4,c(1,2,8,11)]
length(FGPlayerFormYTDHeritageAvgScoreRnd$Player.Name)

# create IDs for datasets
toID <- seq(1,103)
coID <- seq(1,90)
uspgaID <- seq(1,123)
travID <- seq(1,103)
herID <- seq(1,91)

FGPlayerFormYTDHeritageAvgScoreRnd = cbind(herID,FGPlayerFormYTDHeritageAvgScoreRnd)
# create training and test datasets
# The Open Seed = set.seed(1103)
set.seed(562963645)
ind = sample(2,nrow(FGPlayerFormYTDHeritageAvgScoreRnd), replace=TRUE,prob=c(0.70,0.30))
trainData = FGPlayerFormYTDHeritageAvgScoreRnd[ind==1,]
testData = FGPlayerFormYTDHeritageAvgScoreRnd[ind==2,]
tail(trainData)
tail(testData)
names(FGPlayerFormYTDHeritageAvgScoreRnd)
# Try out various decision trees
# The most useful attributes appear to be Score (obviously), SGPutting, SGT2G, Bogey Avoidance, No. of birdies
trainGolfers_j48 = J48(catFinalPositions ~ SG_Total, data = trainData)
trainGolfers_j48 = J48(catFinalPositions ~ FGSGT2G, data = trainData)
trainGolfers_j48 = J48(catFinalPositions ~ FGSGPutting, data = trainData)
trainGolfers_j48 = J48(catFinalPositions ~ FGSGT2GHeritage+FGSGPuttingHeritage, data = trainData)
trainGolfers_j48 = J48(catFinalPositions ~ FGSGT2GHeritage+FGSGPuttingHeritage+FGBirdieBogey_Ratio, data = trainData)

trainGolfers_j48 = J48(catFinalPositions ~ FGSGT2G+FGSGPutting+FGBirdieBogey_Ratio
                       +FGAvg_Position+FGAvg_Score
                       +FG_Points
                       +Course_History, data = trainData)
trainGolfers_j48 = J48(catFinalPositions ~ FGSGT2GHeritage+FGSGPuttingHeritage
                       +FGBirdieBogey_Ratio
                       +FGAvg_PositionHeritage+FGAvg_ScoreHeritage
                       +FG_Points+
                       +Course_History, data = trainData)
trainGolfers_j48 = J48(catFinalPositions ~ SG_Total, data = trainData)
trainGolfers_j48 = J48(catFinalPositions ~ SG_Total
                       +FGAvg_ScoreHeritage
                       , data = trainData)

trainGolfers_j48
summary((trainGolfers_j48))
plot(trainGolfers_j48)
testGolfers_j48 = evaluate_Weka_classifier(trainGolfers_j48, newdata=testData, numFolds = 0, complexity = FALSE, seed = 1, class = TRUE)
testGolfers_j48
##################################################################################
## Check Decision Trees in YTD Data The Masters
library(tree)
library(RWeka)
library(partykit)
## make sure correct "catFinalPositions" has been added.. can copy and paste from "srEventData2016ToUSPGAWithCatFinalPositions"
FGPlayerFormYTDMastersAvgScoreRnd <- read.csv(file=file.choose(), header=T)
tail(FGPlayerFormYTDMastersAvgScoreRnd)
tail(FGPlayerFormYTDMastersAvgScoreRnd)
FGPlayerFormYTDMastersAvgScoreRnd[1:4,c(1,2,8,11)]
length(FGPlayerFormYTDMastersAvgScoreRnd$Player.Name)

# create IDs for datasets
toID <- seq(1,103)
coID <- seq(1,90)
uspgaID <- seq(1,123)
travID <- seq(1,103)
mstID <- seq(1,69)

FGPlayerFormYTDMastersAvgScoreRnd = cbind(mstID,FGPlayerFormYTDMastersAvgScoreRnd)
# create training and test datasets
# The Open Seed = set.seed(1103)
set.seed(22708890)
ind = sample(2,nrow(FGPlayerFormYTDMastersAvgScoreRnd), replace=TRUE,prob=c(0.7,0.3))
trainData = FGPlayerFormYTDMastersAvgScoreRnd[ind==1,]
testData = FGPlayerFormYTDMastersAvgScoreRnd[ind==2,]
tail(trainData)
tail(testData)

# Try out various decision trees
# The most useful attributes appear to be Score (obviously), SGPutting, SGT2G, Bogey Avoidance, No. of birdies
trainGolfers_j48 = J48(catFinalPositions ~ FGSG_Total, data = trainData)
trainGolfers_j48 = J48(catFinalPositions ~ FGSGT2G, data = trainData)
trainGolfers_j48 = J48(catFinalPositions ~ FGSGPutting, data = trainData)
trainGolfers_j48 = J48(catFinalPositions ~ FGSGT2GMasters+FGSGPuttingMasters, data = trainData)
trainGolfers_j48 = J48(catFinalPositions ~ FGSGT2GMasters+FGSGPuttingMasters+FGBirdieBogey_RatioMasters, data = trainData)

trainGolfers_j48 = J48(catFinalPositions ~ FGSGT2G+FGSGPutting+FGBirdieBogey_Ratio
                       +FGAvg_Position+FGAvg_Score
                       +FG_Points
                       +Course_History, data = trainData)
trainGolfers_j48 = J48(catFinalPositions ~ GA_Form+FGSGT2G+FGSGPutting+FGBirdieBogey_Ratio  ## can add GA_Form before FGSGT2G for better results
                       +FGAvg_Position
                       +FG_Points+FGFormIndWithPoints
                       +Course_History, data = trainData)
trainGolfers_j48 = J48(catFinalPositions ~ GA_Form, data = trainData)
trainGolfers_j48 = J48(catFinalPositions ~ FGSGT2GMasters+FGSGPuttingMasters
                       +FGBirdieBogey_RatioMasters
                       +FGAvg_PositionMasters+FGAvg_ScoreMasters
                       +FG_Points
                       +Course_History, data = trainData)

trainGolfers_j48
summary((trainGolfers_j48))
plot(trainGolfers_j48)
testGolfers_j48 = evaluate_Weka_classifier(trainGolfers_j48, newdata=testData, numFolds = 0, complexity = FALSE, seed = 1, class = TRUE)
testGolfers_j48
  ##################################################################################
## Check Decision Trees in YTD Data Canadian Open
library(tree)
library(RWeka)
library(partykit)
## make sure correct "catFinalPositions" has been added.. can copy and paste from "srEventData2016ToUSPGAWithCatFinalPositions"
FGPlayerFormYTDCanAvgScoreRnd <- read.csv(file=file.choose(), header=T)
tail(FGPlayerFormYTDTheOpenAvgScoreRnd)
tail(FGPlayerFormYTDCanAvgScoreRnd)
FGPlayerFormYTDTheOpenAvgScoreRnd[1:4,c(1,2,8,11)]
length(FGPlayerFormYTDTheOpenAvgScoreRnd$Player.Name)

# create IDs for datasets
toID <- seq(1,103)
coID <- seq(1,90)
uspgaID <- seq(1,123)
travID <- seq(1,103)

FGPlayerFormYTDTheOpenAvgScoreRnd = cbind(toID,FGPlayerFormYTDTheOpenAvgScoreRnd)
# create training and test datasets
# The Open Seed = set.seed(1103)
set.seed(1112)
ind = sample(2,nrow(FGPlayerFormYTDCanAvgScoreRnd), replace=TRUE,prob=c(0.7,0.3))
trainData = FGPlayerFormYTDCanAvgScoreRnd[ind==1,]
testData = FGPlayerFormYTDCanAvgScoreRnd[ind==2,]
tail(trainData)
tail(testData)

# Try out various decision trees
# The most useful attributes appear to be Score (obviously), SGPutting, SGT2G, Bogey Avoidance, No. of birdies
trainGolfers_j48 = J48(catFinalPositions ~ FGSG_Total, data = trainData)
trainGolfers_j48 = J48(catFinalPositions ~ FGSGT2G, data = trainData)
trainGolfers_j48 = J48(catFinalPositions ~ FGSGPutting, data = trainData)
trainGolfers_j48 = J48(catFinalPositions ~ FGSGT2GCan+FGSGPuttingCan, data = trainData)
trainGolfers_j48 = J48(catFinalPositions ~ FGSGT2GCan+FGSGPuttingCan+FGBirdieBogey_RatioCan, data = trainData)

trainGolfers_j48 = J48(catFinalPositions ~ FGSGT2GCan+FGSGPuttingCan+FGBirdieBogey_RatioCan
                       +FGAvg_PositionCan+FGAvg_ScoreCan
                       +FG_Points
                       +Course_History, data = trainData)
trainGolfers_j48 = J48(catFinalPositions ~ GA_Form+FGSGT2G+FGSGPutting+FGBirdieBogey_Ratio  ## can add GA_Form before FGSGT2G for better results
                       +FGAvg_Position
                       +FG_Points+FGFormIndWithPoints
                       +Course_History, data = trainData)
trainGolfers_j48 = J48(catFinalPositions ~ GA_Form, data = trainData)
trainGolfers_j48 = J48(catFinalPositions ~ FGSGT2G+FGSGPutting+FGBirdieBogey_Ratio  ## can add GA_Form before FGSGT2G for better results
                       +FGAvg_Position
                       +FG_Points+FGFormIndWithPoints
                       +Course_History, data = trainData)

trainGolfers_j48
summary((trainGolfers_j48))
plot(trainGolfers_j48)
testGolfers_j48 = evaluate_Weka_classifier(trainGolfers_j48, newdata=testData, numFolds = 0, complexity = FALSE, seed = 1, class = TRUE)
testGolfers_j48
  #############################################################################################