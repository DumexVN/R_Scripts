######## WEIGHT DISTRIBUTION ###########
footballDis <- "C:/Users/Dumex/Desktop/Social_Final/kmxt_w/edgeW_dis/football.txt"
flrSmallDis <- "C:/Users/Dumex/Desktop/Social_Final/kmxt_w/edgeW_dis/lfr_n_10k_k15_maxk_50_size_10_1000.txt"
flrBigDis <- "C:/Users/Dumex/Desktop/Social_Final/kmxt_w/edgeW_dis/lfr_n_10k_k50_maxk_100_size_10_1000.txt"
geometric <- "C:/Users/Dumex/Desktop/Social_Final/kmxt_w/edgeW_dis/geometric.txt"

####
footballMat <- read.table(footballDis , sep="\t", header=F)
flrSmallMat <- read.table(flrSmallDis , sep="\t", header=F)
flrBigMat <- read.table(flrBigDis , sep="\t", header=F)
geometricMat <- read.table(geometric , sep="\t", header=F)

#extract column
football <- footballMat[,2]
flrSmall <- flrSmallMat[,2]
flrBig <- flrBigMat[,2] 
geo <- geometricMat [,2]
###DENSITY PLOT
histo <- hist(col,breaks=2000,probability=FALSE)

###
cols <- list("Football"=football ,
		"LFR1"=flrSmall , 
		"LFR2"=flrBig ,
		"Geo"=geo )
dat <- as.data.frame(lapply(cols, `length<-`, max(sapply(cols, length)))) 

df <- melt(dat)
cdat <- ddply(df, "variable", summarise, value.mean=mean(value,na.rm=TRUE))

ggplot(df , aes(x=value, colour=variable, fill=variable)) + geom_density(alpha=.3) +
	geom_vline(data=cdat, aes(xintercept=value.mean,  colour=variable),
               linetype="dashed", size=1) +
	theme(legend.text.align = 0) +
	scale_colour_discrete(name="Graph",
                         breaks=c("Football", "LFR1", "LFR2", "Geo"),
                         labels=c("Football", 
						expression(paste("LFR1: ", mu, "=0.1, ", "maxD=15", ", size",
    						"=[10,1000]")), 
						expression(paste("LFR2: ", mu, "=0.1, ", "maxD=50", ", size",
    						"=[10,1000]")),
						"Geo-tagged"	)) 	+
	scale_fill_discrete(name="Graph",
                         breaks=c("Football", "LFR1", "LFR2", "Geo"),
                         labels=c("Football", 
						expression(paste("LFR1: ", mu, "=0.1, ", "maxD=15", ", size",
    						"=[10,1000]")), 
						expression(paste("LFR2: ", mu, "=0.1, ", "maxD=50", ", size",
    						"=[10,1000]")),
						"Geo-tagged"	)) 	+ 
	facet_wrap(~ variable, ncol=2,scales="free")