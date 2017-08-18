library(ggplot2)
library(reshape2)
################### K-MXT(W) PLOT ###################
input <- "C:/Users/Dumex/Desktop/Social_Final/kmxt_w/k_2_w_varies/combine.txt"
geoFile <- "C:/Users/Dumex/Desktop/Social_Final/kmxt_w/k_2_w_varies/Geo.txt"
#input <- "C:/Users/Dumex/Desktop/Social_Final/kmxt_w/k_varies_w_mean/combine.txt"
#geoFile <- "C:/Users/Dumex/Desktop/Social_Final/kmxt_w/k_varies_w_mean/Geo.txt"

matrix <- read.table(input,sep='\t',header=T,check.names=FALSE)
geoMatrix <- read.table(geoFile,sep='\t',header=T,check.names=FALSE)

d <- melt(matrix, id=c("Graph","Type","Index"))
geo <- melt(geoMatrix, id=c("Graph","Type","Index"))

plot1 <- ggplot(d, aes(x=variable,y=value,group=Index, colour=Index)) +
  theme_light()+ 
  geom_line(size=1.03)+
  geom_point(aes(shape=Index), size=3) +
  facet_wrap(Graph ~ Type, scales="free", ncol=4,
			labeller = label_wrap_gen(multi_line=FALSE)) +
  xlab("Minimum Weight w") + 
  ylab("Value") +
  ggtitle(expression(paste("Scores for 2-MXT(w) as a function of ","w" ,"; results on various input graphs")))

plot2 <- ggplot(geo, aes(x=variable,y=value,group=Index, colour=Index)) + 
  theme_light()+
  geom_line(size=1.03)+
  geom_point(aes(shape=Index), size=3) +
  theme(legend.position="none", axis.title.y=element_blank(), axis.title.x=element_blank())+
  facet_wrap(Graph ~ Type, scales="free", ncol=4,
			labeller = label_wrap_gen(multi_line=FALSE))


g1 <- ggplotGrob(plot1)
g2 <- ggplotGrob(plot2)

g1 <- gtable::gtable_add_grob(g1, g2, t = 10, l=10,b=13.5,r=19)
grid.newpage()
grid.draw(g1)