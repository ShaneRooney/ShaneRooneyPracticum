
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
## Check Correlations and Regression in YTD Data Heritage
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