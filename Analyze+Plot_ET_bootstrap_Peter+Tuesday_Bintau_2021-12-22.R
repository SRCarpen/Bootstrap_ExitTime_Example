# Analyzed bootstrap ET

rm(list = ls())
graphics.off()

# Load data
load(file='ET+Sboot_Tuesday1315.Rdata')
LET0 = ETLR[,1]/12  # convert from 5 min chunks to hours
RET0 = ETLR[,2]/12
rm(ETLR)
#
# Nominal exit times from the data
TmeanETl = 496.6 # hours
TmeanETr = 633 # hours

# Correct bootstrap bias
medLET = median(LET0)
medRET = median(RET0)
#
Lbias = TmeanETl - medLET
Rbias = TmeanETr - medRET
#
LET10 = LET0 + Lbias
LET1 = subset(LET10,subset=(LET10 > 1))
RET1 = RET0 + Rbias

#
probseq = c(0.05,0.1,0.25,0.5,0.75,0.9,0.95)
print('Tuesday Squeal2 exit time',quote=F)
print('Left basin',quote=F)
print('quantiles',quote=F)
print(quantile(LET1,probs=probseq),quote=F)
print('summary',quote=F)
print(summary(LET1),quote=F)
print(c('sd = ',sd(LET1)),quote=F)

print('---------------------------------',quote=F)
print('Right basin',quote=F)
print('quantiles',quote=F)
print(quantile(RET1,probs=probseq),quote=F)
print('summary',quote=F)
print(summary(RET1),quote=F)
print(c('sd = ',sd(RET1)),quote=F)

#
load(file='ET+Sboot_Peter1315.Rdata') #--------------------------------------------------------------
LET02 = ETLR[,1]/12  # convert from 5 min to hours
RET02 = ETLR[,2]/12

# Nominal exit times from the data
RmeanETl = 472.5 # hours
RmeanETr = 465.1 # hours

# Correct bootstrap bias
medLET = median(LET02)
medRET = median(RET02)
#
Lbias = RmeanETl - medLET
Rbias = RmeanETr - medRET
#
LET20 = LET02 + Lbias
LET2 = subset(LET20,subset=(LET20 > 1)) # remove negative ET due to bias correction
RET20 = RET02 + Rbias
RET2 = subset(RET20,subset=(RET20 > 1))

probseq = c(0.05,0.1,0.25,0.5,0.75,0.9,0.95)

print('Peter Squeal 2 Exit Time',quote=F)
print('Left basin',quote=F)
print('quantiles',quote=F)
print(quantile(LET2,probs=probseq),quote=F)
print('summary',quote=F)
print(summary(LET2),quote=F)
print(c('sd = ',sd(LET2)),quote=F)

print('---------------------------------',quote=F)
print('Right basin',quote=F)
print('quantiles',quote=F)
print(quantile(RET2,probs=probseq),quote=F)
print('summary',quote=F)
print(summary(RET2),quote=F)
print(c('sd = ',sd(RET2)),quote=F)

windows()
par(mfrow=c(2,2),mar=c(4, 4.3, 4, 2) + 0.1, cex.axis=1.6,cex.lab=1.6)
hist(LET1,col='skyblue')
hist(RET1,col='lightgreen')
hist(LET2,col='skyblue')
hist(RET2,col='lightgreen')

# clean up for bar chart
rangeall = range(LET1,LET2,RET1,RET2)
xlist = seq(rangeall[1],rangeall[2],by=10)
# lines used to clean up margins, if needed
cl1 = LET1 #ifelse(LET1 <= 200,LET1,200)
cl2 = LET2 #ifelse(LET2 <= 200,LET2,200)
cr1 = RET1  #ifelse(RET1 <= 200,RET1,200)
cr2 = RET2  #ifelse(RET2 <= 200,RET2,200)

# Histograms
windows()
par(mfrow=c(2,2),mar=c(4, 4.3, 4, 2) + 0.1, cex.axis=1.6,cex.lab=1.6)
hist(cl1,col='skyblue',breaks=50,xlim=c(0,1000),xlab='Exit time, h',ylab='Frequency',
     main='Tuesday, Left')
abline(v=TmeanETl,col='darkblue',lty=2,lwd=3)
hist(cr1,col='lightgreen',breaks=50,xlim=c(0,1000),xlab='Exit time, h',ylab='Frequency',
     main='Tuesday, Right')
abline(v=TmeanETr,col='darkblue',lty=2,lwd=2)
hist(cl2,col='skyblue',breaks=50,xlim=c(0,1000),xlab='Exit time, h',ylab='Frequency',
     main='Peter, Left')
abline(v=RmeanETl,col='darkblue',lty=2,lwd=2)
hist(cr2,col='lightgreen',breaks=50,xlim=c(0,1000),xlab='Exit time, h',ylab='Frequency',
     main='Peter, right')
abline(v=RmeanETr,col='darkblue',lty=2,lwd=2)

# Density plots
densl1 = density(cl1,bw='SJ',kernel='epanechnikov',n=256)
densl2 = density(cl2,bw='SJ',kernel='epanechnikov',n=256)
windows()
par(mfrow=c(1,1),mar=c(4, 4.3, 4, 2) + 0.1, cex.axis=1.6,cex.lab=1.4)
yrange = range(densl1$y,densl2$y)
xrange = range(densl1$x,densl2$x)
xrange[1] = 0
plot(densl1$x,densl1$y,type='l',lwd=2,col='sienna',xlim=xrange,ylim=yrange,
     xlab='Exit Time, h',ylab='density',main='Left Exit Time')
points(densl2$x,densl2$y,type='l',lwd=2,col='blue')
abline(v=RmeanETl,col='blue',lty=2,lwd=2)
abline(v=TmeanETl,col='sienna',lty=2,lwd=2)
legend('topright',legend=c('Tuesday','Peter'),lwd=c(2,2),
       col=c('sienna','blue'),cex=1.5,bty='n')
#
# Try a filled version of left exit time
# 
# Function to tune transparency
# Alpha is transparency on 0-100 scale
makeTransparent<-function(someColor, alpha)
{
        newColor<-col2rgb(someColor)
        apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                                    blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}
#
transcoral = makeTransparent('lightcoral',50)
transblue = makeTransparent('lightskyblue',75)
#
windows(width=12,height=6)
par(mfrow=c(1,2),mar=c(4, 4.3, 4, 2) + 0.1, cex.axis=1.6,cex.lab=1.6)
yrange = range(densl1$y,densl2$y)
xrange = range(densl1$x,densl2$x)
xrange[1] = 0
plot(densl1$x,densl1$y,type='l',lwd=1,col='sienna',xlim=xrange,ylim=yrange,
     xlab='Mean Exit Time, h',ylab='Density',
     main='A. Mean Exit Time from Low-Pigment State')
polygon(densl1$x,densl1$y,col=transcoral,border=NA)
polygon(densl2$x,densl2$y,col=transblue,border=NA)
points(densl1$x,densl1$y,type='l',lwd=2,col='sienna')
points(densl2$x,densl2$y,type='l',lwd=2,col='blue')
#abline(v=RmeanETl,col='blue',lty=2,lwd=2)
#abline(v=TmeanETl,col='sienna',lty=2,lwd=2)
legend('topright',legend=c('Tuesday Lake','Peter Lake'),lwd=c(2,2),
       col=c('sienna','blue'),cex=1.5,bty='n')
#
densr1 = density(cr1,bw='SJ',kernel='epanechnikov',n=128)
densr2 = density(cr2,bw='SJ',kernel='epanechnikov',n=128)

#windows()
#par(mfrow=c(1,1),mar=c(4, 4.3, 4, 2) + 0.1, cex.axis=1.6,cex.lab=1.6)
yrange = range(densr1$y,densr2$y)
xrange = range(densr1$x,densr2$x)
xrange[1] = 0
plot(densr1$x,densr1$y,type='l',lwd=1,col='sienna',xlim=xrange,ylim=yrange,
     xlab='Mean Exit Time, h',ylab='Density',
     main='B. Mean Exit Time from High-Pigment State')
polygon(densr1$x,densr1$y,col=transcoral,border=NA)
polygon(densr2$x,densr2$y,col=transblue,border=NA)
points(densr1$x,densr1$y,type='l',lwd=2,col='sienna')
points(densr2$x,densr2$y,type='l',lwd=2,col='blue')
#abline(v=RmeanETl,col='blue',lty=2,lwd=2)
#abline(v=TmeanETl,col='sienna',lty=2,lwd=2)
#legend('topright',legend=c('Tuesday Lake','Peter Lake'),lwd=c(2,2),
#       col=c('sienna','blue'),cex=1.5,bty='n')


windows()
par(mfrow=c(1,1),mar=c(4, 4.3, 4, 2) + 0.1, cex.axis=1.6,cex.lab=1.4)
yrange = range(densr1$y,densr2$y)
xrange = range(densr1$x,densr2$x)
plot(densr1$x,densr1$y,type='l',lwd=2,col='sienna',xlim=xrange,ylim=yrange,
     xlab='Exit Time, h',ylab='density',main='Right Exit Time')
points(densr2$x,densr2$y,type='l',lwd=2,col='blue')
legend('topleft',legend=c('Tuesday','Peter'),lwd=c(2,2),
       col=c('sienna','blue'),cex=1.5,bty='n')
#
windows(height=5,width=8)
par(mfrow=c(1,2),mar=c(4, 4.3, 4, 2) + 0.1, cex.axis=1.6,cex.lab=1.4)
yrange = range(densl1$y,densl2$y)
xrange = range(densl1$x,densl2$x)
plot(densl1$x,densl1$y,type='l',lwd=2,col='sienna',xlim=xrange,ylim=yrange,
     xlab='Exit Time, h',ylab='density',main='Left Exit Time')
points(densl2$x,densl2$y,type='l',lwd=2,col='blue')
#legend('topleft',legend=c('Tuesday','Peter'),lwd=c(2,2),
#       col=c('sienna','blue'),cex=1,bty='n')
yrange = range(densr1$y,densr2$y)
xrange = range(densr1$x,densr2$x)
plot(densr1$x,densr1$y,type='l',lwd=2,col='sienna',xlim=xrange,ylim=yrange,
     xlab='Exit Time, h',ylab='density',main='Right Exit Time')
points(densr2$x,densr2$y,type='l',lwd=2,col='blue')
legend('topright',legend=c('Tuesday','Peter'),lwd=c(2,2),
       col=c('sienna','blue'),cex=1,bty='n')
#
# Rank probability plots
p.cl1 = (rank(cl1))/length(cl1)
p.cl2 = (rank(cl2))/length(cl2)
p.cr1 = (rank(cr1))/length(cr1)
p.cr2 = (rank(cr2))/length(cr2)

windows(height=5,width=8)
par(mfrow=c(1,2),mar=c(4, 4.3, 4, 2) + 0.1, cex.axis=1.6,cex.lab=1.4)
xrange=range(cl1,cl2)
yrange=c(0,1)
plot(cl1,p.cl1,type='p',pch=19,cex=0.7,col='sienna',xlim=xrange,ylim=yrange,
     xlab='Left Exit Time, h',ylab='Probability')
points(cl2,p.cl2,type='p',pch=19,cex=0.7,col='blue')
legend('topleft',legend=c('Tuesday','Peter'),pch=c(19,19),pt.cex=1,cex=1.6,
       col=c('sienna','blue'),bty='n')
xrange=range(cr1,cr2)
yrange=c(0,1)
plot(cr1,p.cr1,type='p',pch=19,cex=0.7,col='sienna',xlim=xrange,ylim=yrange,
     xlab='Right Exit Time, h',ylab='Probability')
points(cr2,p.cr2,type='p',pch=19,cex=0.7,col='blue')