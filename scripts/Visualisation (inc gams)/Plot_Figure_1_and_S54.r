#############################################################################################################
## Code for Figure 1 of Tittensor et al
## Greg Britten
#############################################################################################################


script.dir <- dirname(sys.frame(1)$ofile)
setwd(script.dir)

NAMES <- read.csv(file="NAMES.csv", header=TRUE, stringsAsFactors=FALSE)

folders <- c("STATES", "PRESSURES", "RESPONSES", "BENEFITS")

#############################################################################################################
#############################################################################################################

for(j in 1:4)
{	
	folder <- folders[j]
	setwd(paste("../../results/untransformed", folder, sep="\\"))
	files <- list.files()
	
	n <- numeric(length(file))
	
	for(i in 1:length(files))
	{
		tmp_file <- files[i]
		tmp_dat <- read.csv(file=tmp_file)[,-1] 
		n[i] <- max(as.numeric(tmp_dat[1,])) - min(as.numeric(tmp_dat[1,]))
	}
	
	index_n <- data.frame(index = seq(1,length(files)), n = n)
	response_n <- index_n[order(index_n$n),]
	if(j==1){states_index=response_n$index}
	if(j==2){pressures_index=response_n$index}
	if(j==3){responses_index=response_n$index}
	if(j==4){benefits_index=response_n$index}
}	
		
###########################################################################################################	
###########################################################################################################	

j=4
folder <- folders[j]

if(j==1){colour <- "orange"; index <- states_index}
if(j==2){colour <- "red"; index <- pressures_index}
if(j==3){colour <- "light green"; index <- responses_index}
if(j==4){colour <- "light blue"; index <- benefits_index}

setwd(paste("../../results/untransformed", folder, sep="\\"))
files <- list.files()	#set folder to the specific group

pdf(file=paste("../../images/", folder, ".pdf", sep=""), height=length(files)*0.9, width=3.5)	#make file name according to group


par(mfrow=c(length(files),1), mar=c(0,0,0,0), oma=c(6,7,6,7), cex.axis=0.6, las=1)

n <- numeric(length(files))

for(i in 1:length(files))
{
	tmp_file <- files[index[i]]	#get file name
	nme <- NAMES$TARGET[NAMES$FILE==tmp_file] #look up info from csv
	unt <- NAMES$UNIT[NAMES$FILE==tmp_file]
	tmp_dat <- read.csv(file=tmp_file)[,-1] #read file
	years <- as.numeric(tmp_dat[1,])
	mod <- as.numeric(tmp_dat[3,])
	n[i] <- max(as.numeric(tmp_dat[1,])) - min(as.numeric(tmp_dat[1,]))
	CI_u <- as.numeric(tmp_dat[5,])
	CI_l <- as.numeric(tmp_dat[4,])
	obs <- as.numeric(tmp_dat[7,])
	u_2020 <- tmp_dat[3,ncol(tmp_dat)]	#2020 is the last column
	CI_2020_u <- tmp_dat[5, ncol(tmp_dat)]
	CI_2020_l <- tmp_dat[4, ncol(tmp_dat)]
	u_2010 <- mod[years==2010]	#extract value at 2010
	
	if((CI_2020_u - u_2010) >= 0 & (CI_2020_l - u_2010) <=0){alpha_col = 0.2; line_col="dark grey"}else{alpha_col=0.6; line_col="black"}
	
	y_axis <- c(mod, obs, CI_2020_l, CI_2020_u)
	y_axis <- y_axis[!is.na(y_axis)]
	#Round the value to a prettier values (this isn't the best way to do this - Figure out a better one!)
	#y_axis <- y_axis[!is.na(y_axis)]
	
	#if(max(y_axis) - min(y_axis) < 1){
	#
	#plot(-999, xlim=c(1950, 2020), 
	#	ylim=c(0.5, 1), 
	#	ylab="", xlab="", xaxt="n", yaxt="n", bty="n")
	
	#if(i%%2==0){axis(2, at=c(0.5,1)); mtext(unt,side=2, cex=0.3, line=1, adj=1)}
	#if(i%%2==1){axis(4, at=c(0.5,1)); mtext(unt,side=4, cex=0.3, line=1, adj=0)}
	
	#}else{
	
	if(j==3 & index[i] %in% c(6,13,17,24,25,26)){
	plot(999, xlim=c(1950, 2020), 
		ylim=range(y_axis), 
		ylab="", xlab="", xaxt="n", yaxt="n", bty="n", log="y")
	}else{			
	plot(-999, xlim=c(1950, 2020), 
		ylim=range(y_axis), 
		ylab="", xlab="", xaxt="n", yaxt="n", bty="n")
	}
	
	if(i%%2==0){axis(2, at=range(y_axis),cex.lab=0.6); mtext(unt,side=2, cex=0.45, line=1, adj=1)}	#put axis on alternating sides
	if(i%%2==1){axis(4, at=range(y_axis), cex.lab=1.2); mtext(unt,side=4, cex=0.45, line=1, adj=0)}
	
	#}
	polygon(c(years, rev(years)), c(CI_u, rev(CI_l)), col=adjustcolor(colour,alpha.f=alpha_col), border=FALSE)	#Confidence interval
	
	
	if(length(mod) < 70){lines(years[1:(length(mod)-10)], mod[1:(length(mod)-10)], lwd=1, col=line_col)
						lines(years[(length(mod)-10):length(mod)], mod[(length(mod)-10):length(mod)], lwd=1, col=line_col, lty=3)}else{
						lines(1950:2010, mod[(length(mod)-70):(length(mod)-10)], lwd=1, col=line_col)
						lines(2010:2020, mod[(length(mod)-10):length(mod)], lwd=1, lty=3,col=line_col)}	#if series are bigger than the axis, only plot the ones that fit on (so the line doesn't end in an ugly way
	points(years,obs, pch=19, cex=0.2)
	

	segments(x0=2020, x1=2020, y0=CI_2020_l, y1=CI_2020_u, lwd=1.1)	#2020 projection confidence interval 
	points(2020, u_2020, pch=19, cex=0.5, col=colour)
	points(2020, u_2020, pch=1, cex=0.5, lwd=0.5)
	abline(h=u_2010, lty=2, lwd=0.5)

	
	words <- unlist(strsplit(nme, split=" "))	#separate string into words
	
	if(length(words) > 5){#Breaks long names into two lines		
		line1 <- paste(words[1:5], collapse=" ")
		line2 <- paste(words[6:length(words)], collapse=" ")
		mtext(c(paste(line1, "\n", line2, "\n")) , cex=0.4, line=-1.5, adj=0.1)	#\n tells R to break line
	}else{mtext(nme, cex=0.4, line=-0.5, adj=0.1)}
	
}

axis(1, at=c(1950, 1985, 2020), cex=3)


dev.off()		
# }



