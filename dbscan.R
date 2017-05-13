#DBSCAN SCRIPT
inputFile <- ""
outputDir <- ""

data <- read.table(inputFile, sep=";", header=TRUE)
x <- matrix(unlist(data), ncol=2 , nrow=nrow(data))
x <- x[,-3]

#SWAP TWO COLUMNS SINCE DIST HAVERSINE USE THE REVERSE ...
x[ , c(1,2)] <- x[ , c(2,1)]

#CONVERT TO DIST OBJECT
#TO USE HAVERSINE FORMULA
#distM <- distm(x, fun=distHaversine)

## RUN DBSCAN
#PARAMS: eps = distance, minPts = minimum of points inside circle
eps <- 0.00025
minPts <- 6
res <- dbscan(x, eps, minPts)

##PARSING RESULTS
#GET NUMBER OF CLUSTER
num <- max(res$cluster)

##PRINT IN THE FOLLOWING FORMAT
#"lat;long;id;comm"
#RE-REVERSE DATA :DDDDD

#BINDING THE CLUSTER ORDER
x <- cbind(x, res$cluster)
x <- x[order(x[,3], decreasing = FALSE),]

#READJUST THE MATRIX
x <- cbind(x, seq(0, nrow(data)-1 , 1))
x[ , c(1,2,3,4)] <- x[ , c(2,1,4,3)]
colnames(x) <- c("lat","long","id","com")
#WRITE TO FILE
param <- paste(c(eps, minPts, "dbscan_ashape_in.txt"), collapse = "_")
outPath <- paste(outputDir,param, sep="")
print(outPath)
write.table(x, file=outPath, sep=";", 
			row.names=FALSE, col.names=TRUE,
			append=FALSE, quote=FALSE)

