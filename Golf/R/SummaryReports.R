################ Summary Chart of Top 10 Players going into the Masters ##############################
head(FGPlayerFormYTDMastersAvgScoreRnd)
FGPlayerFormYTDMastersAvgScoreRnd <- read.csv(file=file.choose(), header=T)
MastersForm10 <- FGPlayerFormYTDMastersAvgScoreRnd[1:10,]
par(mar = c(9,4,4,2) + 0.1)
barplot(MastersForm10$FGSG_Total, main="", 
        ylab = "Strokes Gained Total", ylim=c(0,6), names.arg = MastersForm10$Player.Name, las=2)
######################################################################################################
################ Summary Chart of Top 10 Players going into the Heritage ##############################
head(FGPlayerFormYTDHeritageAvgScoreRnd)
FGPlayerFormYTDHeritageAvgScoreRnd <- read.csv(file=file.choose(), header=T)
HeritageForm10 <- FGPlayerFormYTDHeritageAvgScoreRnd[1:10,]
par(mar = c(9,4,4,2) + 0.1)
barplot(HeritageForm10$GA_Form, main="", 
        ylab = "GA Form", ylim=c(0,6), names.arg = HeritageForm10$Player.Name, las=2)
######################################################################################################
################ Summary Chart of Top 10 Players going into the Open ##############################
head(FGPlayerFormYTDTheOpenAvgScoreRnd)
FGPlayerFormYTDTheOpenAvgScoreRnd <- read.csv(file=file.choose(), header=T)
TheOpenForm10 <- FGPlayerFormYTDTheOpenAvgScoreRnd[1:10,]
par(mar = c(9,4,4,2) + 0.1)
barplot(TheOpenForm10$FGSG_Total, main="", 
        ylab = "Strokes Gained Total", ylim=c(0,25), names.arg = TheOpenForm10$Player.Name, las=2)
######################################################################################################
######################################################################################################
################ Summary Chart of Top 10 Players going into the Canadian Open ########################
head(FGPlayerFormYTDCanAvgScoreRnd)
FGPlayerFormYTDCanAvgScoreRnd <- read.csv(file=file.choose(), header=T)
CanForm10 <- FGPlayerFormYTDCanAvgScoreRnd[1:10,]
par(mar = c(9,4,4,2) + 0.1)
barplot(CanForm10$GA_Form, main="", 
        ylab = "GA Form", ylim=c(0,25), names.arg = CanForm10$Player.Name, las=2)
######################################################################################################
################ Summary Chart of Top 10 Players going into the PGA Championship #####################
head(FGPlayerFormYTDUSPGAAvgScoreRnd)
FGPlayerFormYTDUSPGAAvgScoreRnd <- read.csv(file=file.choose(), header=T)
USPGAForm10 <- FGPlayerFormYTDUSPGAAvgScoreRnd[1:10,]
par(mar = c(9,4,4,2) + 0.1)
barplot(USPGAForm10$FGSG_Total, main="", 
        ylab = "Strokes Gained Total", ylim=c(0,5), names.arg = USPGAForm10$Player.Name, las=2)
######################################################################################################
################ Summary Chart of Top 10 Players going into the Travelers ############################
head(FGPlayerFormYTDTravAvgScoreRnd)
FGPlayerFormYTDTravAvgScoreRnd <- read.csv(file=file.choose(), header=T)
TravForm10 <- FGPlayerFormYTDTravAvgScoreRnd[1:10,]
par(mar = c(9,4,4,2) + 0.1)
barplot(TravForm10$GA_Form, main="", 
        ylab = "GA Form", ylim=c(0,20), names.arg = TravForm10$Player.Name, las=2)
######################################################################################################
