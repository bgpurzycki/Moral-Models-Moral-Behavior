##################################################################################
#### The Cognitive and Cultural Foundations of Moral Behavior ####################
##################################################################################

## Code written by Benjamin Grant Purzycki with Richard McElreath and Anne Pisor
## contact email: benjamin_purzycki@eva.mpg.de

#################
# Setting Up
################

# Install and/or load packages #

install.packages("devtools")
install.packages(c("coda","mvtnorm","devtools","loo"))
library(devtools)
devtools::install_github("rmcelreath/rethinking")
devtools::install_github("rmcelreath/rethinking", ref="Experimental")
install_github('alastair-JL/AnthroTools')

library(AnthroTools)
library(xtable)
library(car)
library(plyr)
library(lme4)
library(psych)
library(lme4)
library(Hmisc)
library(lsr)
library(QuantPsyc)
library(rethinking)
library(stats)

####################
### Moral Models ###
####################

##########################################################################  
## Step 1:  Calculate individual item salience for Good and Bad domains ##
##########################################################################  

# Call up the data # 

setwd("")


FL.raw <- read.csv("CERC_Moral_Model_FL_V1.csv", header = T) 

GOOD.FL <- CalculateSalience(FL.raw, Order = "Order", Subj = "CERCID",
                             CODE = "GOOD_SPEC_BP", GROUPING = "Culture", 
                             Rescale = FALSE, Salience = "GOOD.Sal")

							 
BAD.FL <- CalculateSalience(GOOD.FL, Order = "Order", Subj = "CERCID",
                             CODE = "BAD_SPEC_BP", GROUPING = "Culture", 
                            Rescale = FALSE, Salience = "BAD.Sal")
FL <- BAD.FL

write.csv(FL, "FL.csv") # Saving free-list data with individual item salience calculations

#####################################
## Step 2:  Assessing Moral Models ##
#####################################

#FL <- read.csv("FL.csv", header = T) # call up the free-list data if need be

goodlabs <- c("CERCID", "GOOD_SPEC_BP", "Culture")
good <- FL[goodlabs]
good <- good[complete.cases(good),]
badlabs <- c("CERCID", "BAD_SPEC_BP", "Culture")
bad <- FL[badlabs]
bad <- bad[complete.cases(bad),]

gtab <- FreeListTable(good, CODE = "GOOD_SPEC_BP", Subj = "CERCID", tableType = "FREQUENCY")
gtab$Subject <- NULL
btab <- FreeListTable(bad, CODE = "BAD_SPEC_BP", Subj = "CERCID", tableType = "FREQUENCY")
btab$Subject <- NULL

gtab$SumList <- rowSums(gtab)
btab$SumList <- rowSums(btab)

sum(gtab$SumList) # N listed good 
describe(gtab$SumList) # summary of good
sum(btab$SumList) # N listed bad 
describe(btab$SumList) # summary of bad

##############################################
### Step 2a:  Universal Models of Morality ###
##############################################

# Tables 1 and 2 #

#FL <- read.csv("FL.csv", header = T) # call up the free-list data if need be

GOOD.FL.S <- SalienceByCode(FL, Subj = "CERCID", CODE = "GOOD_SPEC_BP", GROUPING = NA,
                            Salience = "GOOD.Sal", dealWithDoubles = "MAX") 
GOOD.SORT <- GOOD.FL.S[order(-GOOD.FL.S$SmithsS),] 
GOOD.SORT$n <- GOOD.SORT$SumSalience/GOOD.SORT$MeanSalience #item sample size
View(GOOD.SORT)
GOOD.SORT[1,3]/GOOD.SORT[1,4] # get total sample size (Smith's S/sum of item salience)

BAD.FL.S <- SalienceByCode(FL, Subj = "CERCID", CODE = "BAD_SPEC_BP", GROUPING = NA,
                           Salience = "BAD.Sal", dealWithDoubles = "MAX")
BAD.SORT <- BAD.FL.S[order(-BAD.FL.S$SmithsS),] 
BAD.SORT$n <- BAD.SORT$SumSalience/BAD.SORT$MeanSalience
View(BAD.SORT)
BAD.SORT[1,3]/BAD.SORT[1,4] # get total sample size (Smith's S/sum of item salience)

print(xtable(GOOD.SORT), include.rownames=FALSE) # For a LaTeX table
print(xtable(BAD.SORT), include.rownames=FALSE) # For a LaTeX table

##########################################
### Step 2b:  Local Models of Morality ###
##########################################

# Note that this takes longer as it chunks salience calculations by group.
# This code creates tables 3, 4, S1, and S2.

#FL <- read.csv("FL.csv", header = T) # call up the free-list data if need be

##############
## The Good ##
##############

GOOD.FL.S.C <- SalienceByCode(FL, Subj = "CERCID", CODE = "GOOD_SPEC_BP", GROUPING = "Culture",
                            Salience = "GOOD.Sal", dealWithDoubles = "MAX")
#View(GOOD.FL.S.C)
GOODc.SORT <- GOOD.FL.S.C[order(GOOD.FL.S.C$GROUPING, -GOOD.FL.S.C$SmithsS),] 
GcS.sub <- subset(GOODc.SORT, SmithsS >= 0.095, # S of 0.10 is just to make presentation cleaner
                  select=c(GROUPING, CODE, MeanSalience, SumSalience, SmithsS)) 
GcS.sub$n <- GcS.sub$SumSalience/GcS.sub$SmithsS
#View(GcS.sub)
print(xtable(GcS.sub), include.rownames = FALSE) # Table 3 and supps

#############
## The Bad ##
#############

BAD.FL.S.C <- SalienceByCode(FL, Subj = "CERCID", CODE = "BAD_SPEC_BP", GROUPING = "Culture",
                           Salience = "BAD.Sal", dealWithDoubles = "MAX")
#View(BAD.FL.S.C)
BADc.SORT <- BAD.FL.S.C[order(BAD.FL.S.C$GROUPING, -BAD.FL.S.C$SmithsS),] 
BcS.sub <- subset(BADc.SORT, SmithsS >= 0.095,
                  select=c(GROUPING, CODE, MeanSalience, SumSalience, SmithsS)) 
BcS.sub$n <- BcS.sub$SumSalience/BcS.sub$SmithsS
#View(BcS.sub)
print(xtable(BcS.sub), include.rownames = FALSE) # Table 4 and supps

##############################################################
#### Step 3: Merging the Salience Data with the CERC data ####
##############################################################

FL <- read.csv("FL.csv", header = T) # call up the free-list data if need be
cerc <- read.csv("CERC Dataset (Wave 1) Version 5.0.csv", header=T)  
#View(cerc) # Check to see if extra rows are at the bottom. Delete if so.

############################################################
### 3a. Getting only those who list Honest and Dishonest ###
############################################################

H.red <- FL[c("Culture", "CERCID", "Order", "GOOD_SPEC_BP", "GOOD.Sal")] 
HONESTvars <- H.red[ which(FL$GOOD_SPEC_BP=="Honest"), ] # Select only those individuals listing "Honest"
HONESTFL <- ddply(HONESTvars, "CERCID", function(df) return(df[df$Order==min(df$Order),])) # Deletes repeated rows with lower salience. 

write.csv(HONESTFL, "HONESTFL.csv")

D.red <- FL[c("Culture", "CERCID", "Order", "BAD_SPEC_BP", "BAD.Sal")] # Extract important vars
DISHONESTvars <- D.red[ which(FL$BAD_SPEC_BP=="Dishonest"), ] # Select only those individuals listing "Dishonest"
DISHONESTFL <- ddply(DISHONESTvars, "CERCID", function(df) return(df[df$Order==min(df$Order),])) # Deletes repeated rows with lower salience 

write.csv(DISHONESTFL, "DISHONESTFL.csv")

cerc.g <- merge(x = cerc, y = HONESTFL[ , c("CERCID", "GOOD.Sal")], by = "CERCID", all.x = TRUE)
cerc.sal <- merge(x = cerc.g, y = DISHONESTFL[ , c("CERCID", "BAD.Sal")], by = "CERCID", all.x = TRUE)

write.csv(cerc.sal, "cerc_sal.csv")

#######################################################################
### 3b. Getting binary presence/absence data for honesty/dishonesty ###
#######################################################################

GOODBIN <- FreeListTable(FL, CODE = "GOOD_SPEC_BP", Subj = "CERCID", tableType = "PRESENCE")
GOODBIN$CERCID <- rownames(GOODBIN)
GOODBIN_HONEST <- subset(GOODBIN, select = c(CERCID, Honest))

BADBIN <- FreeListTable(FL, CODE = "BAD_SPEC_BP", Subj = "CERCID", tableType = "PRESENCE")
BADBIN$CERCID <- rownames(BADBIN)
BADBIN_DISHONEST <- subset(BADBIN, select = c(CERCID, Dishonest))

DATAwGood <- merge(x = cerc.sal, y = GOODBIN_HONEST, by = "CERCID", all.x = TRUE)
DATAwBad <- merge(x = DATAwGood, y = BADBIN_DISHONEST, by = "CERCID", all.x = TRUE)

## Prep for analyses below #

f <- rename(DATAwBad, c("GOOD.Sal" = "HONSAL", "BAD.Sal" = "DISSAL", 
                 "Honest"="HONBIN", "Dishonest"="DISBIN", "HONEST"="gamecheck")) # Rename the Honest/Dishonest variables for clarity

f$HONDIS <- f$HONBIN + f$DISBIN # create composite for binary (dis)honesty values

set.seed(7)
random.imp <- function(a){ # random imputation for missing hadza
  missing <- is.na(a)
  n.missing <- sum(missing)
  a.obs <- a[!missing]
  imputed <- a
  imputed[missing] <- sample(a.obs, n.missing, replace = TRUE)
  return(imputed)
}

f$gmcheck.imp <- random.imp(f$gamecheck) # randomly impute Hadza data for game check ("HONEST")
f$HADZA <- 0 #create dummy for being Hadza
f$HADZA[f$SITE=="Hadza"] <- 1

f.red <- f[!is.na(f$HONBIN),] # select only those who completed freelist tasks

d2s <- subset(f.red, select = c(CERCID, SITE, COREL.L, INGROUP, COREL.S, SELF,
                          DIEPUN, OMNI.BG, AGE, SEX, CHILDREN, FORMALED,
                          HONSAL, DISSAL, HONBIN, DISBIN, HONDIS,
                          TREATMENT, INGFIRST, gamecheck, gmcheck.imp, HADZA)) # subset data for convenience

write.csv(d2s, "data.to.stack.csv") # save (pull up again later)

############################################################
# Logistic regression of predicting honesty and dishonesty #
############################################################

setwd("")


d <- read.csv("data.stacked.csv")

myvars <- c("HONBIN", "DISBIN")
bind <- d[myvars]
dbin <- bind[complete.cases(bind),] 
y <- dbin$DISBIN
x <- dbin$HONBIN

binpred <- map(
  alist(
    y ~ dbinom(1, p),
    logit(p) <- a + bx*x,
    a ~ dnorm(0,10),
    bx ~ dnorm(0,10)
  ), 
  data = dbin)
precis(binpred, prob = .95)
exp(c(1.96, 1.50, 2.42)) # bx, lower, upper

## Test for relationship between game check and listing "honesty"
### See supplements

myvars2 <- c("HONBIN", "gamecheck") # replace with HONEST.imp if desired
bind2 <- d[myvars2]
dbin2 <- bind2[complete.cases(bind2),] 
y <- dbin2$HONBIN
x <- dbin2$gamecheck

binpred2 <- map(
  alist(
    y ~ dbinom(1, p),
    logit(p) <- a + bx*x,
    a ~ dnorm(0,10),
    bx ~ dnorm(0,10)
  ), 
  data = dbin2)
precis(binpred2, prob = .95)
exp(c(0.62, -0.19, 1.43)) # bx, lower, upper
table(d$HONBIN, d$gamecheck)

logistic(-1.52) # alpha
logistic(-1.52 + 0.62) # alpha + bx

### Table 5 ###
table(d$SITE)

table(d$SITE, d$HONBIN)
table(d$SITE, d$DISBIN)
aggregate(HONSAL~SITE, d, mean)
aggregate(HONSAL~SITE, d, sd)
mean(d$HONSAL, na.rm=T)
sd(d$HONSAL, na.rm=T)

aggregate(DISSAL~SITE, d, mean)
aggregate(DISSAL~SITE, d, sd)
mean(d$DISSAL, na.rm=T)
sd(d$DISSAL, na.rm=T)

######################
### Moral Behavior ###
######################

# Now stack the data

# 1. open data.to.stack externally
# 2. create three new vars:  DISTANT, PROXIM, and SELFGAME
# 3. paste all COREL cup data into DISTANT (one on top of the other), and the SELF and INGROUP
#    cup data into PROXIM
# 4. dummy code which game they're from (SELF game = 1, other game = 0)
# 5. paste all other data on top of it
# 6. ...or just open the save stacked data and hope BLINDED did it properly (see below).

#######################################################
#### 4. Regressing FL data on Game Outcome in STAN ####
#######################################################

# Get comfy. If using a lap/desktop, it might make sense to reopen 
# everything and begin to run from here as Stan models tend to stress computers.

library(rethinking) # version 1.71+

d.s <- read.csv("data.stacked.csv")

d.s$group <- as.numeric(factor(d.s$SITE)) # force culture names to numeric
d.s$cercid <- as.numeric(factor(d.s$CERCID)) # force IDs to numeric
d.s$GMCHECK <- d.s$gamecheck # in case of pulling directly from processed data

y <- d.s$DISTANT # coins in distant cup
h <- d.s$HONDIS  # free-list summations (0:2)
group <- d.s$group # field sites (8)
id <- d.s$cercid # participant ID
pun <- d.s$DIEPUN # god punishment
omni <- d.s$OMNI.BG # god knowledge
kids <- d.s$CHILDREN # number of kids
treat <- d.s$TREATMENT # experimental treatment
order <- d.s$INGFIRST # game order
gmcheck <- d.s$GMCHECK # game check ("what was game about?") with missing Hadza data
game <- d.s$SELFGAME # game dummy (1 = Self Game)
hadza <- d.s$HADZA # Hadza dummy (1 = Hadza)

y_notmiss <- which( !is.na(y) & !is.na(h) )
dat_list <- list(
  y = y[y_notmiss],
  h = h[y_notmiss],
  #h = ifelse( h[y_notmiss]==0 , 0 , 1 ),
  group = group[y_notmiss],
  id = id[y_notmiss],
  pun = pun[y_notmiss],
  omni = omni[y_notmiss],
  kids = kids[y_notmiss] / max(kids[y_notmiss],na.rm=TRUE) ,
  treat = treat[y_notmiss],
  order = order[y_notmiss],
  gmcheck = gmcheck[y_notmiss],
  game = game[y_notmiss],
  hadza = hadza[y_notmiss]
)

dat_list$id <- coerce_index( paste( dat_list$id , "ID" ) ) # need to fix individual IDs, as some were removed by NAs

set.seed(7)
m1 <- map2stan(
  alist(
    ## coin model
    y ~ dbinom(30,p),
    logit(p) <- a + zi[id]*sigma_id + aj[group] + # z standardizes adaptive prior for varying effects on individuals
      (bH+bHj[group])*h + bHavg*inv_logit(Havg[group]) +
      (bpun+bpunj[group])*pun + bpunavg*Pavg[group] + 
      (bomni+bomnij[group])*omni + bomniavg*Oavg[group] +
      bkids*kids + btreat*treat + 
      border*order + bgmcheck*gmcheck + bgame*game,
    ## honesty model
    h ~ dbinom(2,ph),
    logit(ph) <- Havg[group],
    Havg[group] ~ normal(Mu_Havg,sigmaHavg),
    Mu_Havg ~ normal(0,5),
    sigmaHavg ~ exponential(1),
    ## pun model
    pun ~ normal(pun_mu,pun_sd),
    pun_mu <- Pavg[group],
    Pavg[group] ~ normal(Mu_Pavg,sigmaPavg),
    Mu_Pavg ~ normal(0.5,1) & T[0,1],
    sigmaPavg ~ exponential(1),
    ## omni model
    omni ~ normal(omni_mu,omni_sd),
    omni_mu <- Oavg[group],
    Oavg[group] ~ normal(Mu_Oavg,sigmaOavg),
    Mu_Oavg ~ normal(0.5,1) & T[0,1],
    sigmaOavg ~ exponential(1),
    ## priors
    a ~ normal(0,10),
    bH ~ normal(0,1),
    c(bpun, bomni, bkids, btreat, border, bgmcheck, bgame, bpunavg, bomniavg) ~ normal(0,1),
    bHavg ~ normal(0,1),
    ## varying intercepts and slopes for groups and h (individual response) on y
    c( aj , bHj , bpunj , bomnij )[group] ~ dmvnormNC( Sigmaj , Rhoj ),
    Sigmaj ~ dexp(1),
    Rhoj ~ dlkjcorr(4),
    ## individual varying intercepts (residuals for binomial overdispersion)
    zi[id] ~ normal(0,1),
    sigma_id ~ exponential(1),
    ## imputation distributions below
    kids ~ normal(kids_mu,kids_sd), # >=0 constraint imposed later
    order ~ bernoulli(0.5),
    gmcheck ~ bernoulli(phi_gmcheck),
    phi_gmcheck ~ beta(1,1),
    kids_mu ~ normal(1,1) & T[0,],
    kids_sd ~ exponential(10),
    pun_sd ~ exponential(1),
    omni_sd ~ exponential(1)
  ),
  start=list(Havg=rep(0,8)),
  constraints=list(
    sigma_id="lower=0",
    phi_gmcheck="lower=0,upper=1",
    kids_impute="lower=0",
    pun_impute="lower=0,upper=1",
    omni_impute="lower=0,upper=1"
  ),
  data=dat_list , 
  sample=TRUE , control=list(adapt_delta=0.99,max_treedepth=13) , chains=3 , cores=3 , iter=1000 , warmup=500 , WAIC=FALSE , DIC=TRUE )

post <- extract.samples(m1)
str(post)
precis(m1, prob = .95)

set.seed(7)
m2 <- map2stan(
  alist(
    ## coin model
    y ~ dbinom(30,p),
    logit(p) <- a + zi[id]*sigma_id + aj[group] + # z standardizes adaptive prior for varying effects on individuals
      (bpun+bpunj[group])*pun + bpunavg*Pavg[group] + 
      (bomni+bomnij[group])*omni + bomniavg*Oavg[group] +
      bkids*kids + btreat*treat + 
      border*order + bgmcheck*gmcheck + bgame*game,
    ## pun model
    pun ~ normal(pun_mu,pun_sd),
    pun_mu <- Pavg[group],
    Pavg[group] ~ normal(Mu_Pavg,sigmaPavg),
    Mu_Pavg ~ normal(0.5,1) & T[0,1],
    sigmaPavg ~ exponential(1),
    ## omni model
    omni ~ normal(omni_mu,omni_sd),
    omni_mu <- Oavg[group],
    Oavg[group] ~ normal(Mu_Oavg,sigmaOavg),
    Mu_Oavg ~ normal(0.5,1) & T[0,1],
    sigmaOavg ~ exponential(1),
    ## priors
    a ~ normal(0,10),
    c(bpun, bomni, bkids, btreat, border, bgmcheck, bgame, bpunavg, bomniavg) ~ normal(0,1),
    ## varying intercepts and slopes for groups and h (individual response) on y
    c(aj,bpunj,bomnij)[group] ~ dmvnormNC(Sigmaj,Rhoj),
    Sigmaj ~ exponential(1),
    Rhoj ~ dlkjcorr(4),
    ## individual varying intercepts (residuals for binomial overdispersion)
    zi[id] ~ normal(0,1),
    sigma_id ~ exponential(1),
    ## imputation distributions below
    kids ~ normal(kids_mu,kids_sd), # >=0 constraint imposed later
    order ~ bernoulli(0.5),
    gmcheck ~ bernoulli(phi_gmcheck),
    phi_gmcheck ~ beta(1,1),
    kids_mu ~ normal(1,1) & T[0,],
    kids_sd ~ exponential(10),
    pun_sd ~ exponential(1),
    omni_sd ~ exponential(1)
  ),
  start=list(Pavg=rep(0,8)), # Pavg was Havg above
  constraints=list(
    sigma_id="lower=0",
    phi_gmcheck="lower=0,upper=1",
    kids_impute="lower=0",
    pun_impute="lower=0,upper=1",
    omni_impute="lower=0,upper=1"
  ),
  data=dat_list , 
  sample=TRUE , control=list(adapt_delta=0.99,max_treedepth=13) , chains=3 , cores=3 , iter=400 , warmup=200 , WAIC=FALSE , DIC=TRUE )

post2 <- extract.samples(m2)
str(post2)

set.seed(7)
m3 <- map2stan(
  alist(
    ## coin model
    y ~ dbinom(30,p),
    logit(p) <- a + zi[id]*sigma_id + aj[group] + # z standardizes adaptive prior for varying effects on individuals
      (bpun+bpunj[group])*pun + bpunavg*Pavg[group] + 
      (bomni+bomnij[group])*omni + bomniavg*Oavg[group] +
      bkids*kids + btreat*treat + bh*h +
      border*order + bgmcheck*gmcheck + bgame*game,
    ## pun model
    pun ~ normal(pun_mu,pun_sd),
    pun_mu <- Pavg[group],
    Pavg[group] ~ normal(Mu_Pavg,sigmaPavg),
    Mu_Pavg ~ normal(0.5,1) & T[0,1],
    sigmaPavg ~ exponential(1),
    ## omni model
    omni ~ normal(omni_mu,omni_sd),
    omni_mu <- Oavg[group],
    Oavg[group] ~ normal(Mu_Oavg,sigmaOavg),
    Mu_Oavg ~ normal(0.5,1) & T[0,1],
    sigmaOavg ~ exponential(1),
    ## priors
    a ~ normal(0,10),
    c(bpun, bomni, bkids, btreat, border, bgmcheck, bgame, bpunavg, bomniavg, bh) ~ normal(0,1),
    ## varying intercepts and slopes for groups and h (individual response) on y
    c(aj,bpunj,bomnij)[group] ~ dmvnormNC(Sigmaj,Rhoj),
    Sigmaj ~ exponential(1),
    Rhoj ~ dlkjcorr(4),
    ## individual varying intercepts (residuals for binomial overdispersion)
    zi[id] ~ normal(0,1),
    sigma_id ~ exponential(1),
    ## imputation distributions below
    kids ~ normal(kids_mu,kids_sd), # >=0 constraint imposed later
    order ~ bernoulli(0.5),
    gmcheck ~ bernoulli(phi_gmcheck),
    phi_gmcheck ~ beta(1,1),
    kids_mu ~ normal(1,1) & T[0,],
    kids_sd ~ exponential(10),
    pun_sd ~ exponential(1),
    omni_sd ~ exponential(1)
  ),
  start=list(Pavg=rep(0,8)), # Pavg was Havg above
  constraints=list(
    sigma_id="lower=0",
    phi_gmcheck="lower=0,upper=1",
    kids_impute="lower=0",
    pun_impute="lower=0,upper=1",
    omni_impute="lower=0,upper=1"
  ),
  data=dat_list , 
  sample=TRUE , control=list(adapt_delta=0.99,max_treedepth=13) , chains=3 , cores=3 , iter=400 , warmup=200 , WAIC=FALSE , DIC=TRUE )

post3 <- extract.samples(m3)
str(post3)

###############
### Table 7 ###
###############

table1 <- precis(m1, prob = 0.95, pars = c("a", "bH", "bpun", "bomni", "bkids", 
                                         "btreat", "border", "bgmcheck", "bgame",
                                         "bpunavg", "bomniavg", "bHavg"), digits = 2)
#table1 <- precis(m1, prob = 0.95)
lemontwigs <- data.frame(cbind(table1$mean, table1$`2.5%`, table1$`97.5%`))
collabels <- c("OR", "Lower", "Upper")
colnames(lemontwigs) <- collabels
rowlabels <- row.names(table1)
rownames(lemontwigs) <- rowlabels
exp(lemontwigs)

table2 <- precis(m2, prob = 0.95, pars = c("a", "bpun", "bomni", "bkids", 
                                          "btreat", "border", "bgmcheck", "bgame",
                                          "bpunavg", "bomniavg"), digits = 2)
#table2 <- precis(m2, prob = 0.95)
tweakbird <- data.frame(cbind(table2$mean, table2$`2.5%`, 
                               table2$`97.5%`))
collabels2 <- c("OR", "Lower", "Upper")
colnames(tweakbird) <- collabels2
rowlabels2 <- row.names(table2)
rownames(tweakbird) <- rowlabels2
exp(tweakbird)

table3 <- precis(m3, prob = 0.95, pars = c("a", "bh", "bpun", "bomni", "bkids", 
                                            "btreat", "border", "bgmcheck", "bgame",
                                            "bpunavg", "bomniavg"), digits = 2)
#table3 <- precis(m3, prob = 0.95)
bigbiz <- data.frame(cbind(table3$mean, table3$`2.5%`, 
                           table3$`97.5%`))
collabels3 <- c("OR", "Lower", "Upper")
colnames(bigbiz) <- collabels3
rowlabels3 <- row.names(table3)
rownames(bigbiz) <- rowlabels3
exp(bigbiz)

save.image("Latest History.RData")

######################
### Plot: Figure 3 ###
######################

p_pred <- with( post , {
  inv_logit( a + bkids*0 + 
               bH*1 + bHavg*1 +
               bpun*1 + bpunavg*1 + 
               bomni*1 + bomniavg*1 )
})

dens( p_pred , xlim=c(0,1) )

# now make plot with group avg pun on horizontal (prob of allocation to other on vertical)

punavgseq <- seq(from=0,to=1,length.out=10)

p_pred <- sapply( punavgseq , function(x) 
  with( post , {
    inv_logit( a + bkids*0 + 
                 bH*0.5 + bHavg*0.5 +
                 bpun*0.5 + bpunavg*x + 
                 bomni*0.5 + bomniavg*0.5 )
  }) )

p_pun_avg <- apply( p_pred , 2 , mean )
p_pun_PI <- apply( p_pred , 2 , PI, prob = .95 )

p_pred <- sapply( punavgseq , function(x) 
  with( post , {
    inv_logit( a + bkids*0 + 
                 bH*0.5 + bHavg*0.5 +
                 bpun*0.5 + bpunavg*0.5 + 
                 bomni*0.5 + bomniavg*x )
  }) )

p_omni_avg <- apply( p_pred , 2 , mean )
p_omni_PI <- apply( p_pred , 2 , PI, prob = .95 )

p_pred <- sapply( punavgseq , function(x) 
  with( post , {
    inv_logit( a + bkids*0 + 
                 bH*0.5 + bHavg*x +
                 bpun*0.5 + bpunavg*0.5 + 
                 bomni*0.5 + bomniavg*0.5)
  }) )

p_hon_avg <- apply( p_pred , 2 , mean )
p_hon_PI <- apply( p_pred , 2 , PI, prob = .95 )

par(mfrow=c(1,3), mar = (c(4.5,5,4,2)))
plot( punavgseq , p_hon_avg , type="l" , ylim=c(0,1) , 
      ylab = "Prob. of Allocation to Distant", 
      xlab = "Moral Models", col="green", cex.lab = 2 )
shade( p_hon_PI , punavgseq , col=col.alpha("green",0.2) )
abline( h=0.5 , lty=2 , lwd=0.5 )

plot( punavgseq , p_pun_avg , type="l" , ylim=c(0,1) , 
      ylab = "Prob. of Allocation to Distant", 
      xlab = "Gods' Punishment", col="red", cex.lab = 2 )
shade( p_pun_PI , punavgseq , col=col.alpha("red",0.2) )
abline( h=0.5 , lty=2 , lwd=0.5 )

plot( punavgseq , p_omni_avg , type="l" , ylim=c(0,1) ,
      ylab = "Prob. of Allocation to Distant", 
      xlab = "Gods' Knowledge", col="blue", cex.lab = 2 )
shade( p_omni_PI , punavgseq , col=col.alpha("blue",0.2) )
abline( h=0.5 , lty=2 , lwd=0.5 )

##############################
### Supplementary Analyses ###
##############################

# OR and 95% CI reporting function

summods <- function(model){
  se <- sqrt(diag(vcov(model)))
  tab <- cbind(OR = fixef(model), Lower = fixef(model) - 1.96 * se, Upper = fixef(model) + 1.96 * se)
  return(data.frame(exp(tab)))
}

## Stacked data set models ##

d.s <- read.csv("data.stacked.csv")
d.s <- d.s[!(is.na(d.s$HONDIS) | d.s$HONDIS==""),] # remove all rows where HONDIS is "NA"
d.s$HONSAL[is.na(d.s$HONSAL)] <- 0 # replace NA's with 0 for "zero salience"
d.s$DISSAL[is.na(d.s$DISSAL)] <- 0 # replace NA's with 0 for "zero salience"

## NOTE:  Use gamecheck instead of gmcheck.imp to remove Hadza.

## Table S4 ##
### working

mS4.red <- glmer(cbind(DISTANT, PROXIM)~
                       DIEPUN+OMNI.BG+
                       CHILDREN+
                       as.factor(TREATMENT)+as.factor(SELFGAME)+as.factor(gmcheck.imp)+as.factor(INGFIRST)+
                       (DIEPUN + OMNI.BG | SITE) + (1 | CERCID),
                     data=d.s, family=binomial)
summary(mS4.red)
summods(mS4.red)
  
mS4.full <- glmer(cbind(DISTANT, PROXIM)~
                    DIEPUN+OMNI.BG+
                    HONDIS+
                    CHILDREN+
                    as.factor(TREATMENT)+as.factor(SELFGAME)+as.factor(gmcheck.imp)+as.factor(INGFIRST)+
                    (HONDIS + DIEPUN + OMNI.BG | SITE) + (1 | CERCID),
                  data=d.s, family=binomial)
summary(mS4.full)
summods(mS4.full)

mS4.1 <-glmer(cbind(DISTANT, PROXIM)~
               DIEPUN+OMNI.BG+
               HONDIS+
               CHILDREN+
               as.factor(TREATMENT)+as.factor(SELFGAME)+as.factor(gmcheck.imp)+as.factor(INGFIRST)+
               (HONDIS|SITE) + (DIEPUN - 1 | SITE) + (OMNI.BG - 1 | SITE) + (1 | CERCID),
             data=d.s, family=binomial)
summary(mS4.1)
summods(mS4.1)

mS4.2 <-glmer(cbind(DISTANT, PROXIM)~
               DIEPUN+OMNI.BG+
               HONSAL+
               CHILDREN+
               as.factor(TREATMENT)+as.factor(SELFGAME)+as.factor(gmcheck.imp)+as.factor(INGFIRST)+
               (HONSAL|SITE) + (DIEPUN - 1 | SITE) + (OMNI.BG - 1 | SITE) + (1 | CERCID),
             data=d.s, family=binomial)
summary(mS4.2)
summods(mS4.2)

mS4.3 <-glmer(cbind(DISTANT, PROXIM)~
               DIEPUN+OMNI.BG+
               DISSAL+
               CHILDREN+
               as.factor(TREATMENT)+as.factor(SELFGAME)+as.factor(gmcheck.imp)+as.factor(INGFIRST)+
               (DISSAL|SITE) + (DIEPUN - 1 | SITE) + (OMNI.BG - 1 | SITE) + (1 | CERCID),
             data=d.s, family=binomial)
summary(mS4.3)
summods(mS4.3)

## Table S5 ##

mS5.1 <-glmer(cbind(DISTANT, PROXIM)~
               DIEPUN+OMNI.BG+
               CHILDREN+
               as.factor(TREATMENT)+as.factor(INGFIRST)+as.factor(gmcheck.imp)+
               as.factor(SELFGAME)+(1|CERCID),
             data=d.s, family=binomial)
summary(mS5.1)
summods(mS5.1)

mS5.2 <-glmer(cbind(DISTANT, PROXIM)~
               DIEPUN+OMNI.BG+
               CHILDREN+
               as.factor(TREATMENT)+as.factor(INGFIRST)+as.factor(gmcheck.imp)+
               as.factor(SELFGAME)+(1|SITE) + (1|CERCID),
             data=d.s, family=binomial)
summary(mS5.2)
summods(mS5.2)

mS5.3 <-glmer(cbind(DISTANT, PROXIM)~
               HONDIS+
               DIEPUN+OMNI.BG+
               CHILDREN+
               as.factor(TREATMENT)+as.factor(INGFIRST)+as.factor(gmcheck.imp)+
               as.factor(SELFGAME)+(1|SITE) + (1|CERCID),
             data=d.s, family=binomial)
summary(mS5.3)
summods(mS5.3)

mS5.4 <-glmer(cbind(DISTANT, PROXIM)~
               HONDIS+
               DIEPUN+OMNI.BG+
               CHILDREN+
               as.factor(TREATMENT)+as.factor(INGFIRST)+as.factor(gmcheck.imp)+
               as.factor(SELFGAME)+(HONDIS|SITE) + (1|CERCID),
             data=d.s, family=binomial)
summary(mS5.4)
summods(mS5.4)

mS5.5 <-glmer(cbind(DISTANT, PROXIM)~
             DIEPUN+OMNI.BG+
             HONBIN+
             CHILDREN+
             as.factor(TREATMENT)+as.factor(SELFGAME)+as.factor(gmcheck.imp)+
             as.factor(INGFIRST)+(HONBIN|SITE) + (1|CERCID),
           data=d.s, family=binomial)
summary(mS5.5)
summods(mS5.5)

mS5.6 <-glmer(cbind(DISTANT, PROXIM)~
               DIEPUN+OMNI.BG+
               DISBIN+
               CHILDREN+
               as.factor(TREATMENT)+as.factor(SELFGAME)+as.factor(gmcheck.imp)+
               as.factor(INGFIRST)+(DISBIN|SITE) + (1|CERCID),
             data=d.s, family=binomial)
summary(mS5.6)
summods(mS5.6)

mS5.7 <-glmer(cbind(DISTANT, PROXIM)~
               DIEPUN+OMNI.BG+
               HONBIN+DISBIN+
               CHILDREN+
               as.factor(TREATMENT)+as.factor(SELFGAME)+
               as.factor(gmcheck.imp)+as.factor(INGFIRST)+(HONDIS|SITE) + (1|CERCID),
             data=d.s, family=binomial)
summary(mS5.7)
summods(mS5.7)

# Plotting Figure S1 (Model 4 in Table S5, model mS4.1 above). 

mS5.4 <-glmer(cbind(DISTANT, PROXIM)~
                HONDIS+
                DIEPUN+OMNI.BG+
                CHILDREN+
                as.factor(TREATMENT)+as.factor(INGFIRST)+as.factor(gmcheck.imp)+
                as.factor(SELFGAME)+(HONDIS|SITE) + (1|CERCID),
              data=d.s, family=binomial)
summary(mS5.4)
summods(mS5.4)

fixef(mS5.4)
ranef(mS5.4)$SITE
precis(mS5.4)
logistic(-0.42)

# adding per site intercept to simple intercept (A) when HONDIS = 0
logistic(ranef(mS5.4)$SITE[,1] + fixef(mS5.4)[1]) 
# adding A to the sum of simple coefficient of HONDIS and per site varying coefficient of HONDIS (B) when HONDIS = 1
logistic(ranef(mS5.4)$SITE[,1] + fixef(mS5.4)[1] + (fixef(mS5.4)[2] + ranef(mS5.4)$SITE[,2])*1) 
# adding A and B ...when HONDIS = 2
logistic(ranef(mS5.4)$SITE[,1] + fixef(mS5.4)[1] + (fixef(mS5.4)[2] + ranef(mS5.4)$SITE[,2])*2) 

## Plot

add_label <- function(xfrac, yfrac, label, pos = 4, ...) {
  u <- par("usr")
  x <- u[1] + xfrac * (u[2] - u[1])
  y <- u[4] - yfrac * (u[4] - u[3])
  text(x, y, label, pos = pos, ...)
}

labels <- c("Coastal Tanna", "Hadza", "Inland Tanna", "Lovu", "Mauritius", "Marajo", "Tyva Republic", "Yasawa")
shady1 <- aggregate(HONDIS~SITE, data = d.s, mean)
shady1$y <- c(.41, .356, .405, .4225, .4035, .41, .428, .3625) # Pisorized points on regression line
as.character(shady1$SITE) -> shady1$SITE
shady1$SITE[shady1$SITE=="Pesqueiro"] <- "Marajo"

xtick <- seq(0, 2, by = 1)

par(mfrow=c(2,4))
for(i in 1:8){
  
  xs <- c(0,1,2) #x-values (moral model summations)
  pr_allocation <- logistic(fixef(mS5.4)[1] + ranef(mS5.4)$SITE[i,1] + (fixef(mS5.4)[2] + ranef(mS5.4)$SITE[i,2])*xs)
  plot(xs, (pr_allocation), ylim=c(0.3,0.5), type="b", xlab = NA, 
       cex = 1.5, cex.lab = 1.25, cex.axis = 1.25, 
       xaxt= "n", ylab= "Prob. of Allocation to DISTANT")
  axis(side=1, at=xtick, labels=T, cex.axis = 1.25)
  baseline_pr <- logistic(fixef(mS5.4)[1] + fixef(mS5.4)[2]*xs)
  points(xs, baseline_pr, type="l", col="gray")
  labels[i] -> newlabs
  shady1$HONDIS[shady1$SITE == newlabs] -> x
  shady1$y[shady1$SITE == newlabs] -> y
  points(x = x, y = y, pch = 20, col = "black", lwd = 5)
  add_label(0.02, 0.07, labels[i], cex = 1.25)
  rm(newlabs, x, y)
}