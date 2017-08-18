#GEOGRAPHICAL POLYGON AREA
polygonFile <- "C:/Users/Dumex/Desktop/GeoEXp/Log/kMTw/London_kmxtw_w80_18_06_2017/ConvexHull_2/poly.txt"
clusterSizeFile <- "C:/Users/Dumex/Desktop/GeoEXp/Log/kMTw/London_kmxtw_w80_18_06_2017/ConvexHull_2/clusterSize_d0.025_k2.txt"
outPath <-  "C:/Users/Dumex/Desktop/GeoEXp/Log/kMTw/London_kmxtw_w80_18_06_2017/ConvexHull_2/clusterDistribution.txt"

#LOAD THE MATRIX
#THE INPUT IS IN THE FORM
#lat;long;id;comm_id
data <- read.table(polygonFile, sep=";", header=TRUE)
sizeMatrix <- read.table(clusterSizeFile, sep=";", header=TRUE)
if (file.exists(outPath)) file.remove(outPath)
#CALCULATE THE AREA OF POLYGON
calArea<-function(v_id)
{
	#transform to numerical array (might be redundant)
	y <- as.numeric(unlist(v_id))
	#input matrix
	input = matrix(nrow=length(v_id),ncol=2)
	#get coordinates
	for(i in 1:length(y))
	{
		#get each vertex from the global var i.e. data
		v_index <- y[i]
		lat<-as.double(data[v_index,][["lat"]])
		long<-as.double(data[v_index,][["long"]])
		input[i,]<-c(long,lat)
	}
	area<-areaPolygon(input)
	return(area)
}

#PREPARING DATA
num_community <- data[nrow(data),"com"]
output<-matrix(nrow=num_community,ncol=4)
colnames(output) <- c("com","Area","NumDots","Density")
#Put each v into its comm
current_comm <- 1 #0 COM IS NOISE I.E SKIPPED
current_comm_members <- list()
for(i in 1:nrow(data))
{
	point <- data[i,]
	if(point[["com"]] == 0)	next
	if(point[["com"]]==current_comm)
	{
		#current_comm_members[[length(current_comm_members)+1]] <- point[["id"]]
		current_comm_members[[length(current_comm_members)+1]] <- i
	}
	else
	{	#CARE IF WHEN AREA IS EMPTY?
		area = calArea(current_comm_members) 
		num_dots = sizeMatrix[(current_comm),2] #+1 For DBSCAN SINCE 0 is the noise
		density = (num_dots/area)
		density <- round(density, digits = 3)
		area = round(area, digits = 3)
		output[current_comm,]<-c(current_comm,area,num_dots,density)
		current_comm_members <- list(i)
		current_comm <- point[["com"]]
	}
	if (i == nrow(data)) #why did i put this here?
	{	
		area = calArea(current_comm_members) 
		num_dots = sizeMatrix[(current_comm),2]
		density = (num_dots/area)
		density <- round(density, digits = 3)
		area = round(area, digits = 3)
		output[current_comm,]<-c(current_comm,area,num_dots,density)
	}
}
#output<-output[order(output[,3], decreasing = TRUE),]
write.table(output, file=outPath, sep=";", 
			row.names=FALSE, col.names=TRUE,
			append=FALSE, quote=FALSE)
