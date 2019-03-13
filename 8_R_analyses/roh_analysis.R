
#################
## Format Data ##
#################

#source('script_add_data.R') #Add latlong and bodymass to raw data

roh20 <- read.csv('roh20X_full.csv',header=T)

#Generate response variables (log-transform)
het <- log(roh20$heterozygosity..H_genome.)
numrohs <- log(roh20$number.of.ROHs)
lenrohs <- log(roh20$mean.ROH.length..Kbp.)
proprohs <- log(roh20$total.number.of.SNPs.in.ROHs/roh20$total.number.of.SNPs)

#Test normality
shapiro.test(het)
shapiro.test(numrohs) #Fails
shapiro.test(lenrohs)
shapiro.test(proprohs) #Fails

#Clean up covariates
redlist <- relevel(roh20$Red_list_groups,ref='N')
diet <- relevel(roh20$Class.of.Consumption,ref='O')
lat <- roh20$lat
mass <- roh20$mass

cutoff1 <- 1000
cutoff2 <- 100000
masscat <- rep(NA,length(mass))
masscat[roh20$mass<cutoff1] <- 'S'
masscat[roh20$mass>=cutoff1&roh20$mass<cutoff2] <- 'M'
masscat[roh20$mass>=cutoff2] <- 'L'
masscat <- as.factor(masscat)
masscat <- relevel(masscat,ref='S')

#Build complete dataset
data <- data.frame(het=het,numrohs=numrohs,lenrohs=lenrohs,proprohs=proprohs,
                   redlist=redlist,diet=diet,lat=lat,mass=mass,masscat=masscat)

#############################################################

#Heterozygosity Analysis

data.sub <- data[data$redlist!='D',] #Remove species w/out data
data.sub$redlist <- factor(data.sub$redlist)

test = lm(het~abs(lat)+log(mass)+diet*redlist,data=data.sub)
summary(test)



tiff(filename="fig_heterozygosity.tiff",width=90,height=90,units="mm",res=1000, pointsize=6,
     compression = "lzw",type='cairo')

#Threatened/diet

#Plot interaction between diet and redlist status
mns <- aggregate(x=data.sub$het,by=list(data.sub$diet,data.sub$redlist),FUN=mean)$x
sds <- aggregate(x=data.sub$het,by=list(data.sub$diet,data.sub$redlist),FUN=sd)$x
nsamp <- aggregate(x=data.sub$het,by=list(data.sub$diet,data.sub$redlist),FUN=length)$x
ses <- sds/sqrt(nsamp)

structure <- c(1,2,3,4.5,5.5,6.5)
upper <- mns+ses
lower <- mns-ses

cols <- gray(c(0,0.4,0.7))

par(mar = c(4.5,3.5,1.5,1) + 0.1, oma=c(0,0,0,0),mgp=c(2.5,1,0))
par(fig=c(0,1,0.48,1),new=FALSE)

plot(0,xlim=c(0.5,7),ylim=c(min(lower)*1.02,max(upper)*0.94),xaxt='n',ylab='log(Heterozygosity ± SE)',xlab="",
     main=expression(paste(bold('Red List Status'%*%' Diet Class'))))
axis(1,at=c(2,5.5),labels=c('Non-threatened','Threatened'),tick=F)

wd=0.07
segments(structure,lower,structure,upper)
segments(structure-wd,upper,structure+wd,upper)
segments(structure-wd,lower,structure+wd,lower)
points(structure,mns,pch=21,bg=cols,cex=2.5)
abline(v=3.75)

legend('topright',fill=cols,legend=c('Omnivore','Carnivore','Herbivore'),ncol=3)

beta <- round(test$coefficients[7],3)
text(5.5,upper[5]+0.25,bquote(beta == .(beta)))

pval <- round(summary(test)$coefficients[7,4],3)
text(5.5,upper[5]+0.1,bquote(italic('p = ')*.(pval)))

for (i in 1:6){
  text(structure[i],-7.45,bquote(italic('n=')*.(nsamp[i])))
}

##Body Mass
par(fig=c(0,0.53,0,0.52),new=TRUE)

plot(log(data.sub$mass),data.sub$het,pch=19,ylab="log(Heterozygosity)",main='Body Mass',
     xlab="log(Body Mass [g])")
abline(test$coefficients[1]+mean(abs(data.sub$lat),na.rm=T)*test$coefficients[2],test$coefficients[3])

beta <- round(test$coefficients[3],3)
text(12,-4.6,bquote(beta == .(beta)),adj=c(0,0))

pval <- round(summary(test)$coefficients[3,4],3)
text(12,-4.9,bquote(italic('p = ')*.(pval)),adj=c(0,0))

##Latitude
par(fig=c(0.47,1,0,0.52),new=TRUE)

plot(abs(data$lat),data$het,pch=19,ylab="",main='Latitude',yaxt='n',
     xlab="abs(Degrees Latitude)")
abline(test$coefficients[1]+mean(log(data$mass),na.rm=T)*test$coefficients[3],test$coefficients[2])

beta <- round(test$coefficients[2],3)
text(55,-4.8,bquote(beta == .(beta)),adj=c(0,0))

pval <- round(summary(test)$coefficients[2,4],3)
text(55,-5.1,bquote(italic('p = ')*.(pval)),adj=c(0,0))


dev.off()

###################################################################

tiff(filename="fig_lenrohs.tiff",width=90,height=90,units="mm",res=1000, pointsize=6,
     compression = "lzw",type='cairo')

#Mean ROH Length Analysis
data.sub <- data[data$redlist!='D',] #Remove species w/out data
data.sub$redlist <- factor(data.sub$redlist)

test = lm(lenrohs~abs(lat)+log(mass)+diet*redlist,data=data.sub)
summary(test)


par(mfrow=c(2,2))
par(mar = c(3.5,3.5,1.5,1) + 0.1, oma=c(0,0,0,0),mgp=c(2.5,1,0))

#Redlist status
data.sub <- data[data$redlist!='D',] #Remove species w/out data
data.sub$redlist <- factor(data.sub$redlist)

mns <- aggregate(x=data.sub$lenrohs,by=list(data.sub$redlist),FUN=mean)$x
sds <- aggregate(x=data.sub$lenrohs,by=list(data.sub$redlist),FUN=sd)$x
nsamp <- aggregate(x=data.sub$lenrohs,by=list(data.sub$redlist),FUN=length)$x
ses <- sds/sqrt(nsamp)
upper <- mns+ses
lower <- mns-ses


plot(1:2,mns,pch=19,xlim=c(0.5,2.5),xaxt='n',ylim=c(0.9*min(lower),1.05*max(upper)),
     ylab="log(Mean ROH Length ± SE)",cex=2.5,xlab="",main='Red List Status')
axis(1,at=c(1:2),labels=c('Non-threatened','Threatened'),tick=F)
segments(1:2,lower,1:2,upper)
wd=0.05
segments(c(1:2)-wd,lower,c(1:2)+wd,lower)
segments(c(1:2)-wd,upper,c(1:2)+wd,upper)

beta <- round(test$coefficients[6],3)
text(2,3.6,bquote(beta == .(beta)))

pval <- round(summary(test)$coefficients[6,4],3)
text(2,3.4,bquote(italic('p = ')*.(pval)),cex=1)

for (i in 1:2){
  text(i,2.9,bquote(italic('n=')*.(nsamp[i])))
}

## Diet Class
mns <- aggregate(x=data.sub$lenrohs,by=list(data.sub$diet),FUN=mean)$x
sds <- aggregate(x=data.sub$lenrohs,by=list(data.sub$diet),FUN=sd)$x
nsamp <- aggregate(x=data.sub$lenrohs,by=list(data.sub$diet),FUN=length)$x
ses <- sds/sqrt(nsamp)
upper <- mns+ses
lower <- mns-ses

plot(1:3,mns,pch=19,xlim=c(0.5,3.5),xaxt='n',ylim=c(0.9*min(lower),1.05*max(upper)),
     ylab="log(Mean ROH Length ± SE)",cex=2.5,xlab="",main='Diet Class')
axis(1,at=c(1:3),labels=c('Omnivore','Carnivore','Herbivore'),tick=F)
segments(1:3,lower,1:3,upper)
wd=0.05
segments(c(1:3)-wd,lower,c(1:3)+wd,lower)
segments(c(1:3)-wd,upper,c(1:3)+wd,upper)

beta <- round(test$coefficients[5],3)
text(3,3.5,bquote(beta == .(beta)))


pval <- round(summary(test)$coefficients[5,4],3)
text(3,3.3,bquote(italic('p = ')*.(pval)),cex=1)
for (i in 1:3){
  text(i,2.75,bquote(italic('n=')*.(nsamp[i])))
}

##Body Mass
plot(log(data.sub$mass),data.sub$lenrohs,pch=19,ylab="log(Mean ROH Length)",main='Body Mass',
     xlab="log(Body Mass [g])")
abline(test$coefficients[1]+mean(abs(data.sub$lat),na.rm=T)*test$coefficients[2],test$coefficients[3])

beta <- round(test$coefficients[3],3)
text(3,7.2,bquote(beta == .(beta)),adj=c(0,0))

pval <- round(summary(test)$coefficients[3,4],3)
text(3,6.65,bquote(italic('p = ')*.(pval)),adj=c(0,0))

##Latitude
plot(abs(data.sub$lat),data.sub$lenrohs,pch=19,ylab="log(Mean ROH Length)",main='Latitude',
     xlab="abs(Degrees Latitude)")
abline(test$coefficients[1]+mean(log(data.sub$mass))*test$coefficients[3],test$coefficients[2])

beta <- round(test$coefficients[2],3)
text(15,7.2,bquote(beta == .(beta)),adj=c(0,0))

pval <- round(summary(test)$coefficients[2,4],3)
text(15,6.65,bquote(italic('p = ')*.(pval)),adj=c(0,0))

dev.off()

###################################################################

#Number of ROHS analysis: not normal, so doing non-parametric analysis

tiff(filename="fig_num_rohs.tiff",width=90,height=90,units="mm",res=1000, pointsize=6,
     compression = "lzw",type='cairo')

#Redlist status
data.sub <- data[data$redlist!='D',] #Remove species w/out data
data.sub$redlist <- factor(data.sub$redlist)

nr.redlist <- kruskal.test(data.sub$numrohs,data.sub$redlist)
mns <- aggregate(x=data.sub$numrohs,by=list(data.sub$redlist),FUN=mean)$x
sds <- aggregate(x=data.sub$numrohs,by=list(data.sub$redlist),FUN=sd)$x
nsamp <- aggregate(x=data.sub$numrohs,by=list(data.sub$redlist),FUN=length)$x
ses <- sds/sqrt(nsamp)
upper <- mns+ses
lower <- mns-ses

par(mfrow=c(2,2))
par(mar = c(3.5,3.5,1.5,1) + 0.1, oma=c(0,0,0,0),mgp=c(2.5,1,0))

plot(1:2,mns,pch=19,xlim=c(0.5,2.5),xaxt='n',ylim=c(0.9*min(lower),1.05*max(upper)),
     ylab="log(Number of ROHs ± SE)",cex=2.5,xlab="",main='Red List Status')
axis(1,at=c(1:2),labels=c('Non-threatened','Threatened'),tick=F)
segments(1:2,lower,1:2,upper)
wd=0.05
segments(c(1:2)-wd,lower,c(1:2)+wd,lower)
segments(c(1:2)-wd,upper,c(1:2)+wd,upper)
pval <- round(nr.redlist$p.value,3)
text(0.75,6,bquote(italic('p = ')*.(pval)),cex=1)
for (i in 1:2){
  text(i,3.4,bquote(italic('n=')*.(nsamp[i])))
}

## Diet Class
nr.diet <- kruskal.test(data$numrohs,data$diet)
mns <- aggregate(x=data$numrohs,by=list(data$diet),FUN=mean)$x
sds <- aggregate(x=data$numrohs,by=list(data$diet),FUN=sd)$x
nsamp <- aggregate(x=data$numrohs,by=list(data$diet),FUN=length)$x
ses <- sds/sqrt(nsamp)
upper <- mns+ses
lower <- mns-ses

plot(1:3,mns,pch=19,xlim=c(0.5,3.5),xaxt='n',ylim=c(0.9*min(lower),1.05*max(upper)),
     ylab="log(Number of ROHs ± SE)",cex=2.5,xlab="",main='Diet Class')
axis(1,at=c(1:3),labels=c('Omnivore','Carnivore','Herbivore'),tick=F)
segments(1:3,lower,1:3,upper)
wd=0.05
segments(c(1:3)-wd,lower,c(1:3)+wd,lower)
segments(c(1:3)-wd,upper,c(1:3)+wd,upper)
pval <- round(nr.diet$p.value,3)
text(3,6,bquote(italic('p = ')*.(pval)),cex=1)
for (i in 1:3){
  text(i,3.55,bquote(italic('n=')*.(nsamp[i])))
}

#Mass

nr.mass <- cor.test(data$numrohs,data$mass,method='spearman')
plot(log(data$mass),data$numrohs,pch=19,ylab="log(Number of ROHs)",main='Body Mass',
     xlab="log(Body Mass [g])",ylim=c(0,13))
rho=round(nr.mass$estimate,3)
text(12,12.5,bquote(rho == .(rho)),adj=c(0,0))
pval <- round(nr.mass$p.value,3)
text(12,11.5,bquote(italic('p = ')*.(pval)),adj=c(0,0))

#Latitude

nr.lat <- cor.test(data$numrohs,abs(data$lat),method='spearman')
plot(abs(data$lat),data$numrohs,pch=19,ylab="log(Number of ROHs)",main='Latitude',
     xlab="abs(Degrees Latitude)",ylim=c(0,13))
rho=round(nr.lat$estimate,3)
text(60,12.5,bquote(rho == .(rho)),adj=c(0,0))
pval <- round(nr.lat$p.value,3)
text(60,11.5,bquote(italic('p = ')*.(pval)),adj=c(0,0))

dev.off()

##################################################

#Proportion of ROHs
tiff(filename="fig_prop_rohs.tiff",width=90,height=90,units="mm",res=1000, pointsize=6,
     compression = "lzw",type='cairo')

#Redlist status
data.sub <- data[data$redlist!='D',] #Remove species w/out data
data.sub$redlist <- factor(data.sub$redlist)

pr.redlist <- kruskal.test(data.sub$proprohs,data.sub$redlist)
mns <- aggregate(x=data.sub$proprohs,by=list(data.sub$redlist),FUN=mean)$x
sds <- aggregate(x=data.sub$proprohs,by=list(data.sub$redlist),FUN=sd)$x
nsamp <- aggregate(x=data.sub$proprohs,by=list(data.sub$redlist),FUN=length)$x
ses <- sds/sqrt(nsamp)
upper <- mns+ses
lower <- mns-ses

par(mfrow=c(2,2))
par(mar = c(3.5,3.5,1.5,1) + 0.1, oma=c(0,0,0,0),mgp=c(2.5,1,0))

plot(1:2,mns,pch=19,xlim=c(0.5,2.5),xaxt='n',ylim=c(1.075*min(lower),0.95*max(upper)),
     ylab="log(Proportion of ROHs ± SE)",cex=2.5,xlab="",main='Red List Status')
axis(1,at=c(1:2),labels=c('Non-threatened','Threatened'),tick=F)
segments(1:2,lower,1:2,upper)
wd=0.05
segments(c(1:2)-wd,lower,c(1:2)+wd,lower)
segments(c(1:2)-wd,upper,c(1:2)+wd,upper)
pval <- round(pr.redlist$p.value,3)
text(0.75,-4.75,bquote(italic('p = ')*.(pval)),cex=1)
for (i in 1:2){
  text(i,-7.7,bquote(italic('n=')*.(nsamp[i])))
}

## Diet Class
pr.diet <- kruskal.test(data$proprohs,data$diet)
mns <- aggregate(x=data$proprohs,by=list(data$diet),FUN=mean)$x
sds <- aggregate(x=data$proprohs,by=list(data$diet),FUN=sd)$x
nsamp <- aggregate(x=data$proprohs,by=list(data$diet),FUN=length)$x
ses <- sds/sqrt(nsamp)
upper <- mns+ses
lower <- mns-ses

plot(1:3,mns,pch=19,xlim=c(0.5,3.5),xaxt='n',ylim=c(1.075*min(lower),0.95*max(upper)),
     ylab="log(Proportion of ROHs ± SE)",cex=2.5,xlab="",main='Diet Class')
axis(1,at=c(1:3),labels=c('Omnivore','Carnivore','Herbivore'),tick=F)
segments(1:3,lower,1:3,upper)
wd=0.05
segments(c(1:3)-wd,lower,c(1:3)+wd,lower)
segments(c(1:3)-wd,upper,c(1:3)+wd,upper)
pval <- round(pr.diet$p.value,3)
text(3,-5,bquote(italic('p = ')*.(pval)),cex=1)
for (i in 1:3){
  text(i,-7.6,bquote(italic('n=')*.(nsamp[i])))
}

#Mass

pr.mass <- cor.test(data$proprohs,data$mass,method='spearman')
plot(log(data$mass),data$proprohs,pch=19,ylab="log(Proportion of ROHs)",main='Body Mass',
     xlab="log(Body Mass [g])")
rho=round(pr.mass$estimate,3)
text(12.5,-12,bquote(rho == .(rho)),adj=c(0,0))
pval <- round(pr.mass$p.value,3)
#text(13,-13,bquote(italic('p = ')*.(pval)),adj=c(0,0))
text(12.5,-13,bquote(italic('p <')~0.001),adj=c(0,0))

#Latitude

pr.lat <- cor.test(data$proprohs,abs(data$lat),method='spearman')
plot(abs(data$lat),data$numrohs,pch=19,ylab="log(Proportion of ROHs)",main='Latitude',
     xlab="abs(Degrees Latitude)")
rho=round(pr.lat$estimate,3)
text(60,10.75,bquote(rho == .(rho)),adj=c(0,0))
pval <- round(pr.lat$p.value,3)
text(60,10,bquote(italic('p = ')*.(pval)),adj=c(0,0))

dev.off()
