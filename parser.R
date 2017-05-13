#### PARSER SCRIPT ####
#TODO: PARSE GEO FILE AND FILTER
#given n points, only points within the rectangle is kept
#the bounding rect is specified by upleft and downright
upleft <- c(51.51817, -0.14110)
downright <- c(51.49081,-0.09535)

#inputfile

#READ
data <- read.table(inputFile, sep=";", header=TRUE)
final <- matrix(nrow=nrow(data),ncol=2)
#parsefile
valid = 1
for(i in 1:nrow(data))
{
	point <- data[i,]
	lat <- point[,"lat"]
	lon <- point[,"long"]
	if (lat <= upleft[1] && lat >= downright[1] &&
		lon <= downright[2] && lon >= upleft[2])
	{
		final[valid,] <- c(lat,lon)
		valid <- valid+1
	}	
}
#delete empty entries
final <- final[rowSums(is.na(final)) != ncol(final),]
#output
final <- cbind(final, seq(0, nrow(final)-1 , 1))
colnames(final) <- c("lat","long", "id")
write.table(final, file=outputFile, sep=";", 
			row.names=FALSE, col.names=TRUE,
			append=FALSE, quote=FALSE)
