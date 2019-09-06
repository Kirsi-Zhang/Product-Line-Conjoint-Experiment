############ Preparation works ############
rm(list=ls())
# set working directory, i.e. where your files and data are located
setwd("C:/Users/siyue/OneDrive/Desktop/Marketing Research/assignment6")

####### Part2: Regression Analysis for getting part-worth estimates #######
# load the data set
filenm = "MKT412R - Final Analysis Case Data"
load(paste(filenm,".Rdata",sep=""))
ls()                # list the objects in R environment and observe them
# sampsize=1000: we have 1000 respondents

atts2 = c("price","size","motion","style")
colnames(desmat) = atts2    # assign names to columns in desmat

#B1. Use regression analysis to develop individual-level part-utility coefficients
# estimate regression for each individual separately
desmatf = cbind(rep(1,nrow(desmat)),desmat) # add column for constant

# construct an empty matrix to store part-worth of each individual
partworths = matrix(nrow=sampsize,ncol=ncol(desmatf))
head(partworths)

# for each individual run the regression
# save the individual coefficients to the corresponding cells in partworth matrix
for(i in 1:sampsize){ 
    partworths[i,]=lm(ratings~desmat,subset=ID==i)$coef
}
colnames(partworths) = c("Intercept","Price","Size","Motion","Style")
head(partworths)
partworths   # we have 1000 sets of coefficients estimates

#B2. Cluster Analysis of Utility Partworths
library(cluster)
library(fpc)
set.seed(123456)   # set random number seed before doing cluster analysis

toclust = partworths    # select partworths to do clustering
pm1 = pamk(toclust,scaling=TRUE)   # determine the optimal number of clusters
# scaling=T: scaling the variables by dividing variables by their root-mean-square
pm1$nc   # the optimal number of clusters ##3

# vitually see the performance with number of clusters
# create a matrix wss to store within-group sum of squares
wss = matrix(NA, nrow=15, ncol=1)
for (i in 1:15) wss[i] <- sum(kmeans(toclust,
                                     centers=i,nstart=2)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")


###3###
km1 = kmeans(toclust,3,iter.max = 20, nstart=2)
# iter.max: maximum number of iterations
# nstart: attempts multiple initial configurations and reports on the best one. 
# nstart=2 generates 2 initial random centroids and choose the best one for the algorithm. 

percsize = paste(1:3," = ",format(km1$size/sum(km1$size)*100,digits=2),"%",sep="")
pie(km1$size,labels=percsize)

clusplot(toclust, km1$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0); #plot clusters against principal components

plotcluster(toclust, km1$cluster)  #plot against discriminant functions ()
km1$centers  # look at how the 3 segments differ in attributes

###2###
km2 = kmeans(toclust,4,iter.max = 20, nstart=2)
# iter.max: maximum number of iterations
# nstart: attempts multiple initial configurations and reports on the best one. 
# nstart=2 generates 2 initial random centroids and choose the best one for the algorithm. 

percsize = paste(1:2," = ",format(km2$size/sum(km2$size)*100,digits=2),"%",sep="")
pie(km2$size,labels=percsize)

clusplot(toclust, km2$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0); #plot clusters against principal components

plotcluster(toclust, km2$cluster)  #plot against discriminant functions ()
km2$centers  # look at how the 2 segments differ in attributes

#C. Use a priori segmentation of the conjoint part-utilities by gender
summary(lm(ratings~desmat*genderD))
# interactions are significant, so can segment based on gender
summary(lm(ratings~desmat,subset=genderD==1)) # Female
summary(lm(ratings~desmat,subset=genderD==0)) # Male

#Use a priori segmentation of the conjoint part-utilities by age
summary(lm(ratings~desmat*ageD))
# interactions are not significant, so cannot segment based on age

#D1. Predict the missing cells (preparing for market simulation)
# repeat individual level partworths for multiplication
partworths.full = matrix(rep(partworths,each=16),ncol=5)
head(partworths.full, 30)

pratings = rowSums(desmatf*partworths.full) # intercept+x1*beta1+x2*beta2+...
finalratings = ifelse(is.na(ratings),pratings,ratings)
# if ratings=NA, fill in the missing rating with predicted rating
# if ratings!=NA, use the existing rating

#D2. Marketing simulation
# Our competitor offers product 7 
# We offer product 13, 5
scen0 = c(7,13,5)

scen1 = c(7,13,5,14)

scen2 = c(7,13,5,16)

scen3 = c(7,13,5,4)

# tranform final ratings into matrix
simDecInput = matrix(finalratings,nrow=nprofiles) # 16 rows as profiles and samplesize as columns

# inputmat is the ratings matrix with rows as profiles and cols as ratings
# scen is the list of products in the market for the scenario (these are rows in inputmat)
simDec = function(inputmat,scen){
    inmkt = inputmat[scen,]
    max = apply(inmkt,2,max)
    firstChoices = (inmkt==rep(max, each=length(scen)))
    shares = firstChoices/rep(colSums(firstChoices),each=length(scen))
    rowMeans(shares)
}

#Market share
simDec0 = simDec(simDecInput,scen0)
simDec0

simDec1 = simDec(simDecInput,scen1)
simDec1

simDec2 = simDec(simDecInput,scen2)
simDec2

simDec3 = simDec(simDecInput,scen3)
simDec3




