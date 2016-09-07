## GA Results##
## Read in Random sample weekly scores
FGWeeklyScoresRandom <- read.csv("~/UCD/Business Analytics/Practicum (Dissertation)/Golf/IrishTimes/FGWeeklyScoresRandom.csv", sep=",")
tail(FGWeeklyScoresRandom)
names(FGWeeklyScoresRandom)
boxplot(FGWeeklyScoresRandom$EndOfSeasonScore)
boxplot(FGWeeklyScoresRandom$TheOpenFGScore)
summary(FGWeeklyScoresRandom)
quantile(FGWeeklyScoresRandom$CanadaFGScore, c(.75, .9)) 

## Compare Standard/CrossoverOnly/MutationOnly/Random
## Read results in for 30 runs of each
ANOVA_TheOpen <- read.csv(file=file.choose(), header=T)
tail(ANOVA_TheOpen)
boxplot(ANOVA_TheOpen$Fitness~ANOVA_TheOpen$GA_Params, main="GA Scores For The Different Paramete Settings")
aovTheOpen <- aov(ANOVA_TheOpen$Fitness~ANOVA_TheOpen$GA_Params)
aovTheOpen
summary(aovTheOpen)
pairwise.t.test(ANOVA_TheOpen$Fitness, ANOVA_TheOpen$GA_Params,
                p.adjust = "bonferroni", alternative = c("two.sided"))
TukeyHSD(aovTheOpen)
plot(TukeyHSD(aovTheOpen), las=1)
## Add 30 official weekly scores in terms of points
#RealScores = c(878,923,0,722,227,366,-1,270,468,513,1277,149,291,1083,1021,878,923,0,722,227,366,-1,270,468,513,1277,149,291,1083,1021)
## Add 30 GA scores in terms of points
#GAScores = c(1102,1102,1102,1102,1102,1102,1102,1102,1102,1102,1102,1102,1102,1102,1102,1102,1102,1102,1102,1102,1102,1102,1102,1102,1102,1102,1102,1102,1102,1102)
## do a t-test. We want small p-value so we can reject H0 that there is no difference between the means
t.test(GAScores, RealScores, mu=0, alt="greater", conf=0.95)
t.test(FGWeeklyScoresRandom$TheOpenFGScore)
t.test(FGWeeklyScoresRandom$TheOpenGAScore, FGWeeklyScoresRandom$TheOpenFGScore, mu=0, alt="greater", conf=0.95)
t.test(FGWeeklyScoresRandom$CanadaFGScore)
t.test(FGWeeklyScoresRandom$CanadaGAScore, FGWeeklyScoresRandom$CanadaFGScore, mu=0, alt="greater", conf=0.95)
t.test(FGWeeklyScoresRandom$USPGAFGScore)
t.test(FGWeeklyScoresRandom$USPGAGAScore, FGWeeklyScoresRandom$USPGAFGScore, mu=0, alt="greater", conf=0.95)

## find value of upper quantile given mean and sd of official scores
qnorm(p=0.75, mean=75, sd=5, lower.tail=T)
FG_FinalScores <- read.csv(file=file.choose(), header=T)
boxplot(FG_FinalScores$Pts)
t.test(FG_FinalScores$Pts, mu=15000, alt="less", level=0.95)
### Final Scores All
FinalScoreAll <- read.csv(file=file.choose(), header=T)
boxplot(FinalScoreAll$FinalScores, main="Quartiles of Final Scores in FG")
hist(FinalScoreAll$FinalScores)
