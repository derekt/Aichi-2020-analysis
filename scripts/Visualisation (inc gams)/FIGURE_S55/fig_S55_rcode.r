library(scales)
library(Hmisc)
library(gdata)
library(gamm4)
library(plyr)
library(MuMIn)
library(mgcv)
library(fields)
library(akima)
library(colorRamps)
library(plotrix)
library(gplots)



script.dir <- dirname(sys.frame(1)$ofile)
setwd(script.dir)

cov<-read.csv('../Figure_S53/coverage_for_r_august6.csv',header=TRUE)
cov$mvar<-seq(1,dim(cov)[1])


#READS DATA FILES IN
setwd('../Figure_S53/AICHI_DATA_FILES')

f<-function(d){
nm0<-(d$Filename)
print(nm0)
nm<-paste(nm0,'.csv');nm<-gsub(' ','',nm)
dat<-try(read.csv(nm,header=TRUE))
if((regexpr('Error',dat)>0)[1]==TRUE){nm<-paste(nm0,'.xlsx');nm<-gsub(' ','',nm)
dat<-try(read.xls(nm,sheet=1))}
print(names(dat))
if(names(dat)[1] %in% c('year','Year')){dum<-data.frame(year=dat[,1])
} else dum<-data.frame(year=rep(NA,1))
dum$y<-dat[,d$Col_for_response]
print(d$Col_for_response)
dum$transform<-d$Transform
dum$family<-d$Family
dum$family2<-d$Family2
dum$link2<-d$Link2
dum$indicator<-d$Indicator_name
dum$element<-d$Element
dum$nelements<-d$N.elements
dum$target<-d$Target
dum$goal<-d$Goal
dum$alignment<-d$Alignment
dum$Target_label<-d$Target_label
dum$category<-d$Category
dum$filename.ss<-d$Filename.ss
return(dum)
}
data<-subset(ddply(subset(cov,is.na(Col_for_response)==FALSE ),.(mvar),.fun=f),is.na(year)==FALSE)
data<-data[,-1]
data<-subset(data,is.na(y)==FALSE)




#FUNCTION WHICH DOES ALL THE WORK: FITS GAMS, EXTRACTS 'BEST' MODEL, ESTIMATES MULTI-MODEL ESTIMATES, PLOTS FITS AND MODEL DIAGNOSTICS, AND OUTPUTS ESTIMATES OF CHANGE FROM 2010 TO 2020. USES LOG LINK ON SEVERAL IN ORDER TO MAKE COMPARABLE WITH SS
mm.fun.mm.gamm<-function(dat,faml=gaussian('identity'),bas='cr',gam=1.4,diag.plots=TRUE,ylm){
print(unique(dat$indicator))
names(dat)[1:2]<-c('year','y')
dat$indicator<-as.character(dat$indicator)
if(unique(dat$indicator) %in% c('Funds for environmental research','Funds toward institutional capacity building in fishing','Funds towards nature reserves','Global funds towards environmental policy and laws','Funds for environmental education and training','Funds for environmental impact assessments','WTO green box spending','Funds towards species protection','GEF funding','FSC and PEFC certified forest')){
    dat$y<-dat$y/1000000
} else NULL

dat<-na.omit(dat)

if(dim(dat)[1]>=10){

if(unique(dat$family2) %in% c('quasibinomial','quasibinomial_transform')){faml=quasibinomial('logit')
} else if(unique(dat$family2) == 'gaussian' & unique(dat$link2)=='log'){faml=gaussian('log')
} else if(unique(dat$family2) == 'gaussian' & unique(dat$link2)=='identity'){faml=gaussian('identity')
} else if(unique(dat$family2) == 'gamma' & unique(dat$link2)=='inverse'){faml=Gamma('inverse')
} else if(unique(dat$family2) == 'gamma' & unique(dat$link2)=='log'){faml=Gamma('log')
} else if(unique(dat$family2) == 'poisson'){faml=poisson('log')
} else {faml=gaussian('identity')}

#TRANSFORMS DATA WHERE NECESSARY
if(unique(dat$family) %in% c('quasibinomial_transform')){dat$y<-dat$y/100
if(max(dat$y)==1){dat$y<-dat$y-.001}
}

if(unique(dat$family2) == 'gamma' & min(dat$y)==0){dat$y<-dat$y+.0001}

l<-gsub(' ', '',paste('(',unique(dat$target)[1],')'))
ttl<-paste(l,unique(dat$indicator),unique(dat$family2),unique(dat$link2))


#FITS MODEL FOR ALL REASONABLY POSSIBLE BASIS DIMENSIONS
dat<-dat[order(dat$year),]#ORDERS DATA BY YEAR FOR AUTOCORRELATION COVARIANCE MATRIX
kupr<-floor(3*(length(unique(dat$year))^(2/9)))#TAKES CONSERVATIVE APPROACH TO BASIS DIMENTION ESTIMATION
kupr<-ifelse(kupr<=4,5,kupr)
kk<-seq(1,kupr,1)#SETS UPPER BOUNDS ON K BASED LOOSLY ON KIM AND GU (2004) SIMULATION STUDY
rm(l)
l<-list()
options(warn=-1)
for(i in 1:length(kk)){
l[[i]]<-try(gamm(y~s(year,k=kk[i],bs=bas),family=faml,data=dat,gamma=gam,select=TRUE,correlation=corAR1(form=~1),method='ML',verbosePQL=FALSE,gamma=1.4))}
options(warn=0)

#EXTRACTS MODEL FIT PARAMETERS FROM CANDIDATE MODEL SET
f2gamm4<-function(d){
dd<-d$gam
s<-summary(dd)
if(((regexpr('Error',d)>0)[1]==T)==TRUE){return(data.frame(family=NA,link=NA,aic=NA,method=NA,bic=NA,r2=NA,pval=NA))
}else{return(data.frame(family=dd$family[1],link=as.character(dd$family[2]),method=dd$method,aic=AICc(d),bic=BIC(d),r2=s$r.sq,pval=s$s.pv[1]))}}

out<-data.frame(do.call('rbind',llply(l,.fun=f2gamm4,.parallel=FALSE)))
out$modelID<-seq(1,dim(out)[1],1)#ADDS IDENTIFIER FOR MODELS
out<-subset(out,is.na(aic)==F & modelID>=4)

#BIC WEIGHTS, SUM TO 1
lkbic<-exp(-.5*(out$bic-min(out$bic)))#LIKELIHOOD OF MODEL, COND ON BBIC
out$wbic<-lkbic/(sum(lkbic))#BIC WEIGHT, SUMS TO 1
out<-out[rev(order(out$wbic)),]#ORDERS DATA BASED ON BIC
k<-out$modelID[1]

#SUM OF STANDARDIZED BIC WEIGHTS
ll<-seq(1,length(out$wbic),1)
s<-matrix(0,length(ll),1)
for(i in 1:length(ll)){d<-out[1:ll[i],];s[i,]<-sum(d$wbic)}
out$swbic<-s

#NEW MODEL LIST, ORDERED BY BIC
newlist<-list()
mds<-out$modelID
for(i in 1:length(mds)){newlist[[i]]<-l[[mds[i]]]}
mod<-newlist[[1]]#IDENTIFIES 'BEST' MODEL


#MULTIMODEL AVERAGE AND MULTIMODEL PREDICTION - DOES NOT WORK FOR MIXED MODEL APPROACH
year2<-seq(min(dat$year),2020,.25)
pred.bst<-predict(mod$gam,newdata=list(year=year2),se.fit=T,type='response')

#PREDICTION BASED ON TOP-RANKED MODEL IN ENSEMBLE
pred<-data.frame(year=year2,pred.bst=pred.bst$fit,se.bst=pred.bst$se.fit,upr.bst=pred.bst$fit+(1.96*pred.bst$se.fit),lwr.bst=pred.bst$fit-(1.96*pred.bst$se.fit),upr.bst99=pred.bst$fit+(2.58*pred.bst$se.fit),lwr.bst99=pred.bst$fit-(2.58*pred.bst$se.fit))

#PLOTS ALL ENSEMBLE MODEL PRDICTIONS; COLOURS REPRESENT THE AIC WEIGHTS
ylm<-c(min(c(dat$y,pred$pred.bst)),max(c(dat$y,pred$upr.bst)))
if((regexpr('Funds',ttl)>0)==TRUE | (regexpr('Funding',ttl)>0)==TRUE | (regexpr('funds',ttl)>0)==TRUE){ylm<-c(0,ylm[2])}else NULL
xlm<-c(min(dat$year),2020)
par(mfrow=c(3,2),mar=c(3,3,1,1),oma=c(1,10,1,10))
plot(dat$year,dat$y,xlim=xlm,ylim=ylm,las=1,xlab='Year',ylab='',pch=16,cex=.5,main=ttl,cex.main=.75)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray75")
mtext('Ensemble predictions',side=3,line=-2,outer=FALSE,cex=.75)

#CALCULATES MULTIMODEL PREDICTION
cl<-(heat.colors(n=dim(out)[1]+1))
m<-list()
for(i in 1:length(newlist)){
year2<-seq(min(dat$year),2020,.25)
p<-predict(newlist[[i]]$gam,newdata=list(year=year2),se.fit=T,type='response')
lines(year2,p$fit,lwd=out[i,9]*3,col=cl[i])
m[[i]]<-data.frame(year=year2,p=p$fit*out[i,9])}
m<-data.frame(do.call('rbind',m))
sig_mean<-data.frame(year=sort(unique(m$year)),sig_mean=tapply(m$p,m$year,sum))

#CALCULATES MULTIMODEL VARIANCE
m<-list()
for(i in 1:length(newlist)){
year2<-seq(min(dat$year),2020,.25)
p<-predict(newlist[[i]]$gam,newdata=list(year=year2),se.fit=T,type='response')
df<-data.frame(year=year2,sig_i=p$fit,sig_mean=sig_mean$sig_mean,sig_var_i=p$se.fit^2,model=i)
df$sig_var_mean<-out[i,9]*(df$sig_var_i+((df$sig_i-df$sig_mean)^2))
m[[i]]<-df}
m<-data.frame(do.call('rbind',m))
dum<-data.frame(year=sort(unique(m$year)),p=sig_mean$sig_mean,se=sqrt(tapply(m$sig_var_mean,m$year,sum)))
dum$upr<-dum$p+(1.96*dum$se);dum$lwr<-dum$p-(1.96*dum$se);dum$upr99<-dum$p+(2.58*dum$se);dum$lwr99<-dum$p-(2.58*dum$se)

#combines all predicted data
predicted<-cbind(dum,pred)

a2010<-subset(predicted,year==2010)
a2020<-subset(predicted,year==2020)

#CALCULATED % DIFFERENCE RELATIVE TO 2010
prd<-data.frame(p.mm=(a2020$p-a2010$p)/a2010$p,
                           upr.mm=(a2020$upr-a2010$p)/a2010$p,
                           lwr.mm=(a2020$lwr-a2010$p)/a2010$p,
                           p.bst=(a2020$pred.bst-a2010$pred.bst)/a2010$pred.bst,
                           upr.bst=(a2020$upr.bst-a2010$pred.bst)/a2010$pred.bst,
                           lwr.bst=(a2020$lwr.bst-a2010$pred.bst)/a2010$pred.bst,
                           goal=unique(dat$goal),
                           target.num=unique(dat$target),
                           target.lab=unique(dat$Target_label),
                           indicator=unique(dat$indicator),
                           alignment=unique(dat$alignment),
                           category=unique(dat$category),
                           filename.ss=gsub('.csv','',unique(dat$filename.ss)),
                           n=length(unique(dat$year)))

prd[,1:6]<-prd[,1:6]*100#CONVERTS TO %

#PLOTS TOP-RANKED MODEL
if(unique(dat$family) %in% c('quasibinomial','quasibinomial_transform','gamma_log','gamma_inverse','poisson') & min(predicted$lwr.bst)<0){
    ylm<-c(0,max(c(dat$y,predicted$pred)))
} else if((regexpr('Funds',ttl)>0)==TRUE | (regexpr('Funding',ttl)>0)==TRUE | (regexpr('funds',ttl)>0)==TRUE) {ylm<-c(0,max(c(dat$y,predicted$pred)))
} else { ylm<-c(min(c(dat$y,predicted$lwr.bst)),max(c(dat$y,predicted$pred)))}

xlm<-c(min(dat$year),2020)
plot(dat$year,dat$y,xlim=xlm,ylim=ylm,las=1,xlab='Year',ylab='',pch=16,cex=.5,main=ttl,cex.main=.75)
polygon(c(predicted$year,predicted$year[length(predicted$year):1]),c(predicted$upr.bst99,predicted$lwr.bst99[length(predicted$lwr.bst99):1]),col='cornflowerblue',border=NA)
polygon(c(predicted$year,predicted$year[length(predicted$year):1]),c(predicted$upr.bst,predicted$lwr.bst[length(predicted$lwr.bst):1]),col='lightskyblue',border=NA)
lines(predicted$year,predicted$pred.bst,lwd=.1)
points(dat$year,dat$y,xlim=c(min(dat$year),2020),pch=15,cex=.7,col='gray20')
text(xlm[1],ylm[2],paste('Basis dim. = ',out$modelID[1]),adj=0)
mtext('Best-fitting model prediction',side=3,line=-2,outer=FALSE,cex=.75)
box()

#PLOTS MULTI MODEL
plot(dat$year,dat$y,xlim=xlm,ylim=ylm,las=1,xlab='Year',ylab='',pch=16,cex=.5,main=ttl,cex.main=.75)
polygon(c(predicted$year,predicted$year[length(predicted$year):1]),c(predicted$upr99,predicted$lwr99[length(predicted$lwr99):1]),col='cornflowerblue',border=NA,xlim=c(1890,2010))
polygon(c(predicted$year,predicted$year[length(predicted$year):1]),c(predicted$upr,predicted$lwr[length(predicted$lwr):1]),col='lightskyblue',border=NA,xlim=c(1890,2010))
lines(predicted$year,predicted$p,lwd=.1)
points(dat$year,dat$y,xlim=c(min(dat$year),2020),pch=15,cex=.7,col='gray20')
mtext('Multi-model predictions',side=3,line=-2,outer=FALSE,cex=.75)
box()

#PLOTS MODEL DIAGNOSTICS
if(diag.plots){
plot(predict(mod$gam,type='link'),residuals(mod$gam,type='deviance'),pch=16,xlab='Year',ylab='Residuals',las=1);abline(h=0,lty=3)
a<-acf(residuals(mod$gam),type='partial',plot=F)
b<-acf(residuals(mod$gam,type='deviance'),plot=F)
plot(seq(0,length(a$acf)-1),a$acf,ylim=c(-1,1),las=1,type='h')#gamm
points((seq(0,(length(b$acf)-1)))+.1,b$acf,col='red',type='h')#gam
abline(h=0,lty=3)
legend('topright',legend=c('GAM residuals','GAMM residuals'),col=c('red','black'),lwd=c(2,2),bty='n')
hist(residuals(mod$gam),main='',las=1,breaks=10)
}
return(prd)
} else {print('Limited data')
l<-gsub(' ', '',paste('(',unique(dat$target)[1],')'))
ttl<-paste(l,unique(dat$indicator),unique(dat$family))
par(mfrow=c(3,2),mar=c(3,3,1,1),oma=c(1,10,1,10))
plot(0,0,main=ttl)
}
}

##BELOW RUNS USING 'CR' AND 'CS' SPLINE METHODS: 'CS' SEEMS TO GIVE SLIGHTLY BETTER RESULTS, BUT BOTH ARE SIMILAR

postscript('GBO4_GAMM_extrapolations_SS_compare_cs_august6.ps')
o<-ddply(data,bas='cs',.(indicator),.fun=mm.fun.mm.gamm)
dev.off()


out.gam<-o



###IMPORTS SS ESTIMATES AND CALCULATES % CHANGE FROM 2010 TO 2020 SAME AS FOR GAM
setwd('../../results/untransformed')
l<-gsub('./','',list.files(full = TRUE))

out<-list()
for(i in 1:length(l)){
print(l[i])
dat<-data.frame(t(read.csv(paste(l[i]),header=TRUE)))
names(dat)<-c('Year','ext','p','lwr','upr','se','y')
dat<-dat[-1,]
#CALCULATES % DIFFERENCE RELATIVE TO 2010 VALUE
a2010<-subset(dat,Year==2010)
a2020<-subset(dat,Year==2020)
prd<-data.frame(p.ss=((a2020$p-a2010$p)/a2010$p)*100,upr.ss=((a2020$upr-a2010$p)/a2010$p)*100,lwr.ss=((a2020$lwr-a2010$p)/a2010$p)*100)
prd$filename.ss<-gsub('.csv','',paste(l[i]))
prd$target<-unique(na.omit(as.numeric(unlist(strsplit(unlist(l[i]), "[^0-9]+")))))[1]
out[[i]]<-prd
}
out.ss<-data.frame(do.call('rbind.fill',out))


out.gam$filename.ss<-as.character(out.gam$filename.ss)
out.ss$filename.ss<-as.character(out.ss$filename.ss)
out<-merge(out.gam,out.ss,by=c('filename.ss'),all=FALSE)
ax1<-min(c(out$p.mm,out$p.ss))
ax2<-max(c(out$p.mm,out$p.ss))

#CORRELATION BETWEEN STATE-SPACE AND GAM
cor(out$p.ss,out$p.mm)
plot(out$p.ss,out$p.mm)

#CALCULATES SOME VARIABLES FOR PLOTS
out$delta<-out$p.ss-out$p.mm
out<-out[with(out, order(p.ss)), ]#ORDERS BY state-space rate
out$mv<-seq(1,dim(out)[1],1)#VALUES FOR X

z<-ifelse((out$p.ss>0 & out$p.mm<0) |(out$p.ss<0 & out$p.mm>0),'wrong','right')
out$clasf<-ifelse((out$p.ss>0 & out$p.mm>0 & out$lwr.mm>0 & out$lwr.ss<0) | (out$p.ss>0 & out$p.mm>0 & out$lwr.mm<0 & out$lwr.ss>0) | (out$p.ss<0 & out$p.mm<0 & out$lwr.mm>0 & out$lwr.ss<0) | (out$p.ss<0 & out$p.mm<0 & out$lwr.mm<0 & out$lwr.ss>0),'wrong','right')

#FIGURE SHOWING GAM V SS COMPARISONS
setwd('../../images/')
pdf('ss_gam_compare_indicator_by_indicator2_august6_cs.pdf',width=7,height=11)
cl1<-'firebrick'
cl2<-'royalblue4'
par(mar=c(4,17,.1,.1),oma=c(0,0,0,0),mgp=c(1.4,.5,0))
plot(out$p.ss,out$mv,axes=FALSE,ylab='',xlab='Percent change (2010 to 2020)',cex=1,lwd=.5,pch=18,xlim=c(-110,1100),col=alpha(cl2,.75))
f<-function(d){
lines(c(d$lwr.ss,d$upr.ss),c(d$mv,d$mv),col=alpha(cl2,.75),lwd=.5)
lines(c(d$lwr.mm,d$upr.mm),c(d$mv+.15,d$mv+.1),col=alpha(cl1,.75),lwd=.5)}
z<-dlply(out,.(mv),.fun=f)
axis(1,seq(-100,1100,50),cex.axis=.7,line=-.75,tck=-.01,labels=FALSE,tick=TRUE)
axis(1,seq(-100,1100,200),cex.axis=.7,line=-.75,tck=-.01,labels=TRUE)
axis(2,out$mv,labels=out$indicator,las=1,cex.axis=.7,tck=-.01)
points(out$p.mm,out$mv+.15,pch=18,col=alpha(cl1,.75),cex=1,lwd=.5)
segments(x0=0,y0=0.5,x1=0,y1=max(out$mv)+.5,lty=2,lwd=.75)
legend(200,2,legend=c('State-space/multi-model estimates','GAM multi-model estimates'),pch=18,col=c(cl2,cl1),bty='n',pt.cex=1.25,cex=.75)
text(-128,out$mv,gsub(' ','',paste('(',out$n,')')),cex=.4)
points(rep(-90,dim(out)[1]),out$mv,pch=ifelse(out$clasf=='right','','*'),cex=1.25,lwd=.01)
dev.off()





