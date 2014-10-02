#CODE TO PRODUCE FIGURE S53. REQUIRES ALL DATA FILES TO BE IN SAME DIRECTORY, AND EXCEL SPREADSHEET 'COVERAGE_FOR_R_AUGUST6.CSV' MUST BE AVAILABLE. 
library(mgcv)
library(fields)
library(akima)
library(colorRamps)
library(plotrix)
library(gplots)
library(gdata)
library(plyr)

script.dir <- dirname(sys.frame(1)$ofile)
setwd(script.dir)

#LOCATION OF SPREADSHEET 'COVERAGE_FOR_R_AUGUST6.CSV'
cov<-read.csv('coverage_for_r_august6.csv',header=TRUE)
dum2<-subset(cov,select=c('Target','Target_label'))
cov2<-cov
cov$mvar<-seq(1,dim(cov)[1])
cov$Target2<-abs(cov$Target-21)

#LOCATION OF ALL AICHI DATA FILES IN .CSV FORMAT. NAME OF FILES MUST MATCH 'FILENAME' VARIABLE IN SPREADSHEET
setwd('/AICHI_DATA_FILES')
cov<-subset(cov,Filename!='')

#READS ALL AICHI SPREADSHEETS IN AND COMBINES TO SINGLE FILE
f<-function(d){
nm0<-(d$Filename)
nm<-paste(nm0,'.csv');nm<-gsub(' ','',nm)
dat<-try(read.csv(nm,header=TRUE))
if((regexpr('Error',dat)>0)[1]==TRUE){nm<-paste(nm0,'.xlsx');nm<-gsub(' ','',nm)
dat<-try(read.xls(nm,sheet=1))}
print(nm0)
print(names(dat))
if(names(dat)[1] %in% c('year','Year')){dum<-data.frame(year=dat[,1])
} else dum<-data.frame(year=rep(NA,1))
dum$indicator<-d$Indicator_name
dum$element<-d$Element
dum$nelements<-d$N.elements
dum$target<-d$Target
dum$goal<-d$Goal
dum$alignment<-d$Alignment
dum$Target_label<-d$Target_label
dum$Category<-d$Category
return(dum)
}
z<-subset(ddply(cov,.(mvar),.fun=f),is.na(year)==FALSE)


#CALCULATES START AND END YEAR FOR EACH INDICATOR
f2<-function(d){
return(data.frame(goal=unique(d$goal),target=unique(d$target),target_label=unique(d$Target_label),indicator=unique(d$indicator),year1=min(d$year),year2=max(d$year),alignment=unique(d$alignment),category=unique(d$Category)))
}
zz<-ddply(z,.(indicator),.fun=f2)
zz$pre_year1<-ifelse(zz$year1<1900,'yes','no')
zz$year1<-ifelse(zz$year1<1900,1900,zz$year1)

zz<-zz[with(zz,order(target,year1)),]
zz$indicator.n<-seq(dim(zz)[1],1,-1)

#ADDS COLOUR SCHEME FOR ALIGNMENT
rdgn <- colorRampPalette(c('darkred','red','lightpink','olivedrab1','green3','darkgreen'))
gry <- colorRampPalette(c('gray100','gray80','gray60','gray40','gray20','black'))
rc<-data.frame(alignment=seq(1,3,1),cl=rdgn(3),cl2=gry(3),cl3=c('red3','royalblue','green3'),cl4=rich.colors(3),cl5=c('gold2','orangered','red4'))
zz<-merge(zz,rc,by=c('alignment'),all=FALSE)

zz$clr<-ifelse(zz$target %in% c(1,2,3,4),'dodgerblue4','orange')
zz$clr<-ifelse(zz$target %in% c(5,6,7,8,9,10),'red3',zz$clr)
zz$clr<-ifelse(zz$target %in% c(11,12,13),'forestgreen',zz$clr)
zz$clr<-ifelse(zz$target %in% c(14,15,16),'purple',zz$clr)

#ADDS LABELS AND PLOTTING INFO FOR AICHI TARGETS
axs1<-data.frame(y=tapply(zz$indicator.n,zz$target,max),cl=c(rep('dodgerblue4',4),rep('red3',6),rep('forestgreen',3),rep('purple',1),rep('orange',2)),
target_label=c(
"(1) Biodiversity awareness\n increased",
"(2) Biodiversity values integrated\n into development strategies",
"(3) Reform incentives",
"(4) Sustainable\n production,\n consumption, and\n resource use",
"(5) Natural habitat loss halved,\n degradation reduced",
"(6) Marine living resources\n managed sustainably",
"(7) Agriculture, aquaculture,\n and forestry managed sustainably",
"(8) Pollution reduced",
"(9) Invasive alien\n species controlled\n and eradicated",
"(10) Pressure on coral reefs\n and vulnerable ecosystems\n reduced",
"(11) Protected area extent and\n biodiversity coverage increased",
"(12) Extinctions\n prevented\n and threatened\n species status\n improved",
"(13) Genetic diveristy of farmed\n plants and animals maintained",
"(14) Ecosystems providing essential\n services are safeguarded",
"(19) knowledge relating to\n biodiversity improved, applied",
"(20) Increase financial resources\n from all sources"))


 #LABELS FOR STRATEGIC GOALS
 axs2<-data.frame(y=tapply(zz$indicator.n,zz$goal,max),goal_label=sort(unique(zz$goal)))
 axs2<-arrange(axs2,desc(y))
 axs2$cl<-c('dodgerblue4','red3','forestgreen','purple','orange')

 axs3<-data.frame(y=tapply(zz$indicator.n,zz$target,mean),coverage=rescale(tapply(zz$alignment,zz$target,sum),newrange=c(.5,3)))

#REPLACES 'PROTECTED AREA' WITH 'PA' IN ORDER TO MAKE TITLES SHORTER
zz$indicator<-gsub('Protected area','PA',zz$indicator)
zz$indicator<-gsub('protected area','PA',zz$indicator)

#CHANGES COVERAGE SYMBOLS TO COLOURED ALL AXIS TEXT BLACK
axs1$cl<-'black'
axs2$cl<-'black'


##############################################################
#PRODUCES THE FIGURE - NEEDS TO BE MODIFIED IN ILLUSTRATOR AFTERWARD TO ADJUST TEXT LABELS AND ADD COLOUR BAR

postscript('fig1_prac_scheme29.ps',height=10,width=7.4,font='Helvetica',pointsize=9,horizontal=FALSE)
par(mar=c(4,10,.5,0),mgp=c(13,.35,0),tcl=-.25,cex.axis=.65)
plot(0,0,xlim=c(1870,2020),ylim=c(min(zz$indicator.n),max(zz$indicator.n)),las=1,yaxt='n',xlab='Year',ylab='',lwd=.1,axes=FALSE)
axis(side=1,at=seq(1900,2020,20),las=1,lwd=.25,line=-1)
text(rep(1849.5,dim(axs1)[1]),axs1$y+.25,axs1$target_label,adj=c(0,1),col=as.character(axs1$cl),xpd=TRUE,cex=.65)
text(1950,-2,'Year',xpd=TRUE,cex=.75)
segments(x0=2010,x1=2010,y0=1,y1=55,lwd=.25,xpd=TRUE,col='black',lty=2)#blue

lb<-1783.75#location of lower rectangle limits
lt<-1849#location of vert lines for targets
ln<-3.9#location of goals text
lh<-1849#location of horizontal lines

#ADDS RECTANGLES AROUND GOALS
amt<-.5
segments(x0=ln,x1=2014,y0=axs2$y[2]+amt,y1=axs2$y[2]+amt,lwd=.5,xpd=TRUE,col='black')#blue
segments(x0=ln,x1=2014,y0=axs2$y[3]+amt,y1=axs2$y[3]+amt,lwd=.5,xpd=TRUE,col='black')#blue
segments(x0=ln,x1=2014,y0=axs2$y[4]+amt,y1=axs2$y[4]+amt,lwd=.5,xpd=TRUE,col='black')#blue
segments(x0=ln,x1=2014,y0=axs2$y[5]+amt,y1=axs2$y[5]+amt,lwd=.5,xpd=TRUE,col='black')#blue
segments(x0=ln,x1=2014,y0=0+amt,y1=0+amt,lwd=.5,xpd=TRUE,col='black')#blue

ind<-c(axs1$y,0);ind<-ind[-1];ind<-ind+.5

#PLOTS COVERAGE SYMBOLS
axs3$coverage2<-(axs3$coverage/max(axs3$coverage))*100
axs3$coverage3<-ifelse(axs3$coverage2<=33.333,1,2)
axs3$coverage3<-ifelse(axs3$coverage2>=66.66,3,axs3$coverage3)
axs3$cl<-ifelse(axs3$coverage3==1,'gold2','orangered')
axs3$cl<-ifelse(axs3$coverage3==3,'red4',axs3$cl)
points(rep(1890,16),axs3$y,pch=16,cex=1.9,xpd=TRUE,col=axs3$cl)
points(rep(1890,16),axs3$y,pch=1,cex=1.9,lwd=.2,xpd=TRUE,col='black')

#HORIZONTAL LINES
for(i in 1:length(ind)){
l1<-1860.5
x1<-lh
x2<-2014
y<-ind[i]
segments(x0=x1,x1=x2,y0=y,y1=y,lwd=.5,xpd=TRUE,col='black')#blue
}

#VERTICAL LINES FOR TARGETS
ind<-c(axs1$y,0)
for(i in 1:length(ind)){
l1<-lt
y0<-ind[i]
y1<-ind[i+1]+.5
segments(x0=l1,x1=l1,y0=y0,y1=y1,lwd=1,xpd=TRUE,col=as.character(axs1$cl[i]))#blue
}

#ADDS LABELS FOR STRATEGIC GOALS
ls<-1;cx<-.65;ad<-1;
att<-57.5
mtext(expression(bold('Strategic goal')),side=2,line=4,cex=.75,at=att,las=ls,padj=ad)
mtext(expression(bold('Target')),side=2,line=-1,cex=.75,at=att,las=ls,padj=ad)
mtext(expression(bold('Coverage of target')),side=2,line=-9,cex=.75,at=att,las=ls,padj=ad)
mtext(expression(bold('Availability and alignment of indicator')),side=2,line=-30,cex=.75,at=att,las=ls,padj=ad)

mtext('(A) Address the\n underlying causes\n of biodiversity loss\n by mainstreaming\n biodiversity across\n government and\n society',side=2,line=ln,cex=cx,at=axs2$y[1],las=ls,padj=ad,col=axs2$cl[1])
mtext('(B) Reduce the\n direct pressures\n on biodiversity\n and promote\n sustainable use',side=2,line=ln,cex=cx,at=axs2$y[2],las=ls,padj=ad,col=axs2$cl[2])
mtext('(C) To improve\n the status of\n biodiversity by\n safeguarding\n ecosystems,\n species and\n genetic diversity',side=2,line=ln,cex=cx,at=axs2$y[3],las=ls,padj=ad,col=axs2$cl[3])
mtext('(D) Enhance the benefits\n to all from biodiversity\n and ecosystem services',side=2,line=ln,cex=cx,at=axs2$y[4],las=ls,padj=ad,col=axs2$cl[4])
mtext('(E) Enhance\n implementation\n through participatory\n planning,\n knowledge\n management\n and capacity\n building',side=2,line=ln,cex=cx,at=axs2$y[5],las=ls,padj=ad,col=axs2$cl[5])

ff<-function(d){
y1<-rep(d$indicator.n-.2,100)
y2<-rep(d$indicator.n+.2,100)
x<-seq(d$year1,d$year2,length.out=100)
polygon(c(x,x[length(x):1]),c(y1,y2[length(y2):1]),col=as.character(d$cl5),border='black',lwd=.1)#media
fnc<-function(ot){
dm<-ifelse(ot=='state','S','R')
dm<-ifelse(ot=='pressure','P',dm)
return(dm)
}
if(d$pre_year1=='yes'){
          points(d$year1,d$indicator.n,pch=18,col=as.character(d$cl5))
          text(d$year1-.5,d$indicator.n,paste(as.character(d$indicator),gsub(' ','',paste('(',fnc(d$category),')'))),cex=.65,adj=1)
} else {  text(d$year1-.5,d$indicator.n,paste(as.character(d$indicator),gsub(' ','',paste('(',fnc(d$category),')'))),cex=.65,adj=1)}
}
l<-dlply(zz,.(indicator),.fun=ff)
dev.off()


#PRODUCES THE COLOUR BAR WHICH CAN BE ADDED IN ILLUSTRATOR. EASIER THIS WAY THEN ADDING TO FIGURE DIRECTLY.
gry <- colorRampPalette(c('gold2','orangered','red4'))
setwd('../../images/')
postscript('fig1_prac_scheme29_colbar.ps',height=3,width=3,font='Helvetica',pointsize=9,horizontal=FALSE)
par(mar=c(8,6,6,6),lwd=.01)
plot(0,0,xlab='')
image.plot(legend.only=TRUE,zlim=c(0,3),col=gry(3),legend.lab="Element coverage",horizontal=TRUE,smallplot=c(.14,.95,.06,.08),lwd=.001)
dev.off()






