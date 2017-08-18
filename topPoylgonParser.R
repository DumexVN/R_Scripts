######## DENSE CLUSTER PARSER ###########
### TODO: PICK A TOP n DENSEST CLUSTER ####
### READ THE POLYGON FILE AND REMOVE/RELABEL THE POLYGON TO DRAW ###
densityFile <- "C:/Users/Dumex/Desktop/Small_Set/DBSCAN/innerLondon/25m/0.00025_80_dbscan_clusterDensity.txt"
polygonFile <- "C:/Users/Dumex/Desktop/Small_Set/DBSCAN/innerLondon/25m/0.00025_80_dbscan_poly.txt"
###### CREATE DATA MATRIX #######
densityMatrix <- read.table(densityFile, sep=";", header=TRUE)
polygonMatrix <- read.table(polygonFile, sep=";", header=TRUE)
### GET TOP 10 DENSEST POLYGON ###
minArea <- 100000
## REMOVE INVALID ENTRIES ##
#REMOVE NA
densityMatrix <- densityMatrix[complete.cases(densityMatrix),]
#REMOVE INF
densityMatrix <- densityMatrix[densityMatrix$Density != Inf, ]
#REMOVE SMALL AREA
parsedPolygon <- densityMatrix[densityMatrix$Area > minArea,]
#RANK POLYGON ACCORDING TO ITS DENSITY
rankedPolygon <- parsedPolygon[order(parsedPolygon$Density,decreasing=TRUE),]
rankedPolygon <- rankedPolygon[(1:10),]
topCom <- rankedPolygon$com
#### NOW PRINT THE TOP POLYGON ####
polygonMatrix <- polygonMatrix [(polygonMatrix$com %in% topCom),]
######### SAVE OUTPUT #############
## FILEPATH ##
vertexPath <- "C:/Users/Dumex/Desktop/Tokyo_EXP/Log/kMXT_50m_w20/ConvexHull_4/top10_vertex.txt"
edgePath <- "C:/Users/Dumex/Desktop/Tokyo_EXP/Log/kMXT_50m_w20/ConvexHull_4/top10_edge.txt"
## FIRST PRINT VERTEX ##
# RE-INDEX THE VERTEX ID #
polygonMatrix$id <- seq(0,nrow(polygonMatrix)-1,1)
write.table(polygonMatrix, file=vertexPath, sep=";", 
			row.names=FALSE, col.names=TRUE,
			append=FALSE, quote=FALSE)
# PRINT EDGE #
edgeMatrix <- matrix(nrow=nrow(polygonMatrix),ncol=2)
# EDGE CONTINOUSELY i.e edge[1]: 0,1
# CAUTION AT THE LAST EDGE POINTING BACK TO THE BEGINNING
edgeMatrix[,1] <- seq(0,nrow(polygonMatrix)-1,1)
edgeMatrix[,2] <- seq(1,nrow(polygonMatrix),1)
currentCom <- polygonMatrix[1,]$com
startingVertex <- polygonMatrix[1,]$id
for (i in 1:nrow(polygonMatrix))
{
	thisCom <- polygonMatrix[i,]$com
	if (currentCom != thisCom)
	{
		#point the last edge back to the starting vertex
		edgeMatrix[(i-1),2] <- startingVertex #the entry is i-1 since we past the entry
		#reset the pointer
		currentCom <- thisCom
		startingVertex <- polygonMatrix[i,]$id
	}
	# ADJUST LAST ENTRY
	if (i == nrow(polygonMatrix)){edgeMatrix[i,2] <- startingVertex}
}
write.table(edgeMatrix, file=edgePath, sep=",", 
			row.names=FALSE, col.names=FALSE,
			append=FALSE, quote=FALSE)
#CLEANING UP
densityMatrix <- list()
polygonMatrix  <- list()
