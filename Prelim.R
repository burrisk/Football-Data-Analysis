#Load Data
pbpALL = read.csv("nfl_00-14/csv/PBP.csv")
game = read.csv("nfl_00-14/csv/GAME.csv")

#New Field for Winner of Game
for (i in 1:nrow(game)){
  if (game$ptsv[i] > game$ptsh[i]){
    game$winner[i] = as.character(game$v[i])
  }
  else if (game$ptsv[i] < game$ptsh[i]){
    game$winner[i] = as.character(game$h[i])
  }
  else{
    game$winner[i] = "TIE"
  }
}

#Only Capture Relevant Play by Play Data
pbp = pbpALL[,1:21]

#Merge Game and Play by Play Data
data = merge(pbp,game)

#Convert Factors to Characters
data$h = as.character(data$h)
data$v = as.character(data$v)
data$off = as.character(data$off)
data$def = as.character(data$def)
data$min = as.numeric(as.character(data$min))

#Spread in Favor of Offense
data$spread = data$sprv*(data$h == data$off) - data$sprv*(data$v == data$off)

# 1 = Win, 0 = Loss, 0.5 = Tie
data$offwin = (data$winner == data$off)+ .5*(data$winner =="TIE")

#Seconds Left in Regulation (Negative Values mean Overtime)
data$secLeft = 3600 - 900*data$qtr + 60*data$min + data$sec

#Point Differential of Offense
data$ptDiff = data$ptso - data$ptsd

#Offense is Home Team
data$home = (data$h == data$off)

#Regular Season Games Only
regdata = subset(data,data$wk <= 17)
reggame = subset(game,game$wk <= 17)

### WIN PROBABILITY ANALYSIS ###

#Subset the Data
wpdata = na.omit(regdata[,c("gid", "seas", "home", "dwn","ytg","yfog","spread","offwin","timo","timd","secLeft","ptDiff","off","def","h","v","winner")])
wptraining = subset(wpdata,wpdata$seas %in% c(2000:2003))
gametraining = subset(reggame,game$seas %in% c(2000:2003))
wptest = subset(wpdata,wpdata$seas %in% c(2004:2014))
gametest = subset(reggame,game$seas %in% c(2004:2014))

#Ordinal Logistic Regression Variables
library(MASS)
library(pracma)
library(randomForest)

olrmodel = polr(as.factor(wptraining$offwin) ~ ., data = wptraining[,which(names(wptraining) %in% c("dwn","ytg","yfog","timo","timd","secLeft","ptDiff"))], Hess = TRUE)
#rforest = randomForest(as.factor(wptraining$offwin) ~ ., data = wptraining[,which(names(wptraining) %in% c("dwn","ytg","yfog","timo","timd","secLeft","ptDiff"))], keep.forest = TRUE, ntree = 50)

#summary(olrmodel)
wptest$winprob = predict(olrmodel,newdata = wptest,type = "prob")[,3]# + .5 * predict(olrmodel,wptest,type = "prob")[,2]
wptest$winpredict = (wptest$winprob >= .5)
wptest$hwin = (wptest$h == wptest$winner)
table(wptest$winpredict,wptest$offwin)
sum(wptest$winpredict == wptest$offwin)/nrow(wptest)

firstgame = subset(wptest,gid == 3456)
firstgame$hwinprob = firstgame$winprob*(firstgame$off == firstgame$h) + (1-firstgame$winprob)*(firstgame$off == firstgame$v)
plot(c(3600 - firstgame$secLeft,max(3600-firstgame$secLeft)), c(firstgame$hwinprob,firstgame$hwin[1]), type = 'l', ylim = c(0,1), xlab = "Seconds Into Game", ylab = paste("P(", as.character(firstgame$h[1]),"Wins)"), main = paste(firstgame$seas[1]," ", firstgame$v[1], " @ ",firstgame$h[1] ))
xx1 = c(rev(3600-firstgame$secLeft),3600-firstgame$secLeft)
xx2 = c(3600 - firstgame$secLeft, rev(3600-firstgame$secLeft))
yy1 = c(rep(0,length(firstgame$hwinprob)),firstgame$hwinprob)
yy2 = c(firstgame$hwinprob,rep(1,length(firstgame$hwinprob)))
polygon(xx1,yy1,col = rgb(1,2/5,0))
polygon(xx2,yy2, col = "purple")
text(900,.7,.374)
text(2700,.45,.626)

trapz(firstgame$secLeft,firstgame$hwinprob)/(trapz(firstgame$secLeft,firstgame$hwinprob) + trapz(firstgame$secLeft,1-firstgame$hwinprob))

denbal = 3456
nesea = 3989
  
for (i in 1:nrow(gametest)){
  thisgame = subset(wptest, wptest$gid == gametest$gid[i])
  thisgame$hwinprob = thisgame$winprob*(thisgame$off == thisgame$h) + (1-thisgame$winprob)*(thisgame$off == thisgame$v)
  
  gametest$hprops[i] = trapz(thisgame$secLeft,thisgame$hwinprob)/(trapz(thisgame$secLeft,thisgame$hwinprob) + trapz(thisgame$secLeft,1-thisgame$hwinprob))
  gametest$vprops[i] = 1-gametest$hprops[i]
  gametest$winnerprops[i] = ifelse(gametest$winner[i] == gametest$h[i], gametest$hprops[i], gametest$vprops[i])
}
hist(gametest$winnerprops, breaks = 50, xlab = "Cumulative Win Probabilities for Winners")

wptest = merge(wptest,gametest)

### Create Markov Chain with Transition Probabilities ###
gametest$h = as.character(gametest$h)
gametest$v = as.character(gametest$v)
mgames = subset(gametest, gametest$seas %in% c(2005:2014))


markovRank = function(data,home,visitor,homeStrength,visStrength,prior, priorWeight = 2){
  if(missing(prior)){
    nTeams = length(unique(c(home,visitor)))
    a = 1/(2*nTeams - 2)
    prior = matrix(rep(a,nTeams^2), ncol = nTeams) - diag(a,ncol = nTeams,nrow = nTeams) + diag(.5,ncol = nTeams,nrow = nTeams)
    dimnames(prior) = list(unique(c(home,visitor)), unique(c(home,visitor)))
  }
    nTeams = length(unique(c(home,visitor)))
    nTeamGames = matrix(data = rep(0,nTeams), nrow= nTeams, ncol = 1, dimnames = list(unique(c(home,visitor))))
    diag(prior) = 0
    posterior = priorWeight*prior
    
    for (i in 1:nTeams){
      nTeamGames[i] = sum(home == rownames(nTeamGames)[i]) + sum(visitor == rownames(nTeamGames)[i])
    }
    
    
    for (i in 1:nrow(data)){
      posterior[home[i],visitor[i]] =  posterior[home[i],visitor[i]] + (visStrength[i])
      posterior[visitor[i],home[i]] =  posterior[visitor[i],home[i]] + (homeStrength[i])
    }
    
    for (i in 1:nrow(posterior)){
      posterior[i,] = posterior[i,]/(nTeamGames[i]+priorWeight)
    }
    diag(posterior) = 1-rowSums(posterior)
    posterior
  }

  
"%^%"<-function(A,n){ 
  if(n==1) A else {B<-A; for(i in (2:n)){A<-A%*%B}}; A 
} 

teamRank = function(tMat){
  rankings = as.matrix(1600*(tMat %^% 100)[1,])
  colnames(rankings) = c("pRank")
  library(data.table)
  rankings = data.table(rankings, keep.rownames = TRUE)
  rankings = setorderv(rankings, c("pRank"), order = -1)
  rankings
}

seasSubset = function(data, years = c(2014)){
  subset(data, data$seas %in% years)
}


##Point Differentials
gametest$hptdiff = gametest$ptsh - gametest$ptsv
gametest$vptdiff = -gametest$hptdiff
gametest$hmix = (gametest$ptsh/(gametest$ptsh+gametest$ptsv) + gametest$hprops)/2
gametest$vmix = 1-gametest$hmix
gametest$hptprop = gametest$ptsh/(gametest$ptsh+gametest$ptsv)
gametest$vptprop = 1-gametest$hptprop

mgames2014 = seasSubset(gametest, years = c(2011))
mgames2013 = seasSubset(gametest, years = c(2010))
prior1 = markovRank(mgames2013,mgames2013$h,mgames2013$v,mgames2013$hprops, mgames2013$vprops)
prior2 = markovRank(mgames2013,mgames2013$h,mgames2013$v,mgames2013$ptsh/(mgames2013$ptsh+mgames2013$ptsv), mgames2013$ptsv/(mgames2013$ptsh+mgames2013$ptsv))
prior3 = markovRank(mgames2013,mgames2013$h,mgames2013$v,mgames2013$hmix, mgames2013$vmix)
teamRank(markovRank(mgames2014,mgames2014$h,mgames2014$v,mgames2014$hprops, mgames2014$vprops, prior = prior1, priorWeight = 2))
teamRank(markovRank(mgames2014,mgames2014$h,mgames2014$v,mgames2014$ptsh/(mgames2014$ptsh+mgames2014$ptsv), mgames2014$ptsv/(mgames2014$ptsh+mgames2014$ptsv), prior = prior2))
teamRank(markovRank(mgames2014,mgames2014$h,mgames2014$v,mgames2014$hmix, mgames2014$vmix, prior = prior3))

##SEE WHETHER MY MODEL HAS ANY PREDICTIVE VALUE ##



markovRank(mgames,mgames$h,mgames$v,mgames$ptsh/(mgames$ptsh+mgames$ptsv), mgames$ptsv/(mgames$ptsh+mgames$ptsv))
#Create Data Frame with to predict next year's win loss
cumStrength = data.frame()
for (i in 2004:2013){
  mgames = seasSubset(gametest, years = c(i))
  mgamesPlusOne = seasSubset(gametest, years = c(i+1))
  mrank = as.data.frame(teamRank(markovRank(mgames,mgames$h,mgames$v,mgames$hprops, mgames$vprops)))
  mptrank = as.data.frame(teamRank(markovRank(mgames,mgames$h,mgames$v,mgames$ptsh/(mgames$ptsh+mgames$ptsv), mgames$ptsv/(mgames$ptsh+mgames$ptsv))))
  b = matrix(nrow = 32, ncol = 3, dimnames = list(1:length(unique(c(mgames$h,mgames$v))),c("NextYrWins","PointDiff","ThisYrWins")))
  for (j in 1:length(unique(c(mgames$h,mgames$v)))){
    b[j,1] = sum(mgamesPlusOne$winner == mrank[j,"rn"])
    b[j,2] = sum((mgames$h == mrank[j,"rn"])*mgames$hptdiff) + sum((mgames$v == mrank[j,"rn"])*mgames$vptdiff)
    b[j,3] = sum(mgames$winner == mrank[j,"rn"])
  }
  b= as.data.frame(b)
  a = cbind(mrank, matrix(data = rep(i,length(unique(c(mgames$h,mgames$v)))), dimnames = list(1:length(unique(c(mgames$h,mgames$v))),c("Year"))),b)
  head(a)
  cumStrength = rbind(cumStrength,a)
}

nextYrModel = glm(cumStrength$NextYrWins~scale(cumStrength$pRank) + scale(cumStrength$PointDiff) , data = cumStrength, family = "poisson")
summary(nextYrModel)

## Predict NFL games from rankings
wklyStrength = data.frame()
wklyStrengthGame = data.frame()
for (i in 2005:2014){
  for (j in 1:17){
    games = subset(gametest, gametest$seas == i & gametest$wk == j)
    gamesPrevThisYr = subset(gametest,gametest$seas == i & gametest$wk < j)
    gamesLastYr = subset(gametest,gametest$seas == i-1)
    if (j > 1){
      prior = markovRank(gamesLastYr,gamesLastYr$h,gamesLastYr$v,gamesLastYr$hprops,gamesLastYr$vprops)
      ranks = teamRank(markovRank(gamesPrevThisYr,gamesPrevThisYr$h,gamesPrevThisYr$v,gamesPrevThisYr$hprops,gamesPrevThisYr$vprops,prior))
    }
    else {
      ranks = teamRank(markovRank(gamesLastYr,gamesLastYr$h,gamesLastYr$v,gamesLastYr$hprops,gamesLastYr$vprops))
    }
    yr = rep(i,32)
    wk = rep(j,32)
    setkey(ranks)
    games$hRankGreater = as.numeric(ranks[games$h,pRank] > ranks[games$v,pRank] -5)
    games$hRank = ranks[games$h,pRank]
    games$vRank = ranks[games$v,pRank]
    games$hwin = as.numeric(games$h == games$winner) + .5*(games$winner == "TIE")
    wklyStrength = rbind(wklyStrength,cbind(ranks,yr,wk))
    wklyStrengthGame = rbind(wklyStrengthGame,games)
  }
}


wklyStrengthGame$spreadGreater = as.numeric((wklyStrengthGame$sprv > 0))
table(wklyStrengthGame$hwin,wklyStrengthGame$hRankGreater)

wklyStrengthGame$rankPredict = wklyStrengthGame$hwin == wklyStrengthGame$hRankGreater
mean(wklyStrengthGame$hwin == wklyStrengthGame$hRankGreater)
mean(wklyStrengthGame$spreadGreater == wklyStrengthGame$hRankGreater)

table(wklyStrengthGame$spreadGreater == wklyStrengthGame$hwin,wklyStrengthGame$hRankGreater == wklyStrengthGame$hwin)
mean(wklyStrengthGame$hwin == wklyStrengthGame$spreadGreater)

winsOnly = subset(wklyStrengthGame,wklyStrengthGame$hwin != 0.5)
gamePredictModel1 = glm(as.factor(hwin)~spreadGreater+hRankGreater,winsOnly, family = "binomial")
xtable(summary(gamePredictModel1))

gamePredictModel2 = glm(as.factor(hwin)~scale(sprv)+(scale(hRank-vRank)),winsOnly, family = "binomial")
summary(gamePredictModel2)





