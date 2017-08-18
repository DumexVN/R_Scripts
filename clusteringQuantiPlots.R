##################### GRAPH PLOTS #####################
input <- "C:/Users/Dumex/Desktop/Social_Final/Plots/CNM_ARI.txt"
matrix <- read.table(input,sep='\t',header=T, encoding = "Latin-1")
matrix[,1] <- c("0.1","0.2","0.3","0.4","0.5","0.6","0.7")
colnames(matrix) <- c("mu","[10,50]", "[50,100]", "[100,150]",
		"[150,200]", "[200,250]", "[250,300]", "[300,350]")
matrix

library(ggplot2)
library(reshape2)

d <- melt(matrix, id.vars="mu")
colnames(d) <- c("mu", "Sizes", "value")
ggplot(d, aes(d$mu,value, col=Sizes,group=Sizes)) + 
  coord_cartesian(ylim=c(0,1))+
  theme_light()+
  geom_line(size=1.05) +
  geom_point(size=2.5) +
  xlab(expression(~ mu)) + 
  ylab("ARI") +
  ggtitle("CNM ARI")