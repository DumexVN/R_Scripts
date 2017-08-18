# READ FILE - MODIFY THIS
#inputFile <- "C:/Users/Dumex/Desktop/test.txt"
#outputFile <- "C:/Users/Dumex/Desktop/test_out.txt"
# ------------------------------------
inputFile <- "C:/Users/Dumex/Desktop/GeoEXp/Log/kMTw/London_kmxt(w)_10m/ConvexHull_3/ashape_in.txt"
outputFile <- "C:/Users/Dumex/Desktop/GeoEXp/Log/kMTw/London_kmxt(w)_10m/ConvexHull_3/ashape_out.txt"
polygonFile <- "C:/Users/Dumex/Desktop/GeoEXp/Log/kMTw/London_kmxt(w)_10m/ConvexHull_3/poly.txt"
#------------- BEGIN MAIN SCRIPT -----------------------
#-------------------------------------------------------
if (file.exists(outputFile)) file.remove(outputFile)
if (file.exists(polygonFile))
{ 
	file.remove(polygonFile)
	header <- "lat;long;id;com"
	write(header,polygonFile)
}
else
{
	header <- "lat;long;id;com"
	write(header,polygonFile)
}
data <- read.table(inputFile, sep=";", header=TRUE)
final <- matrix(nrow=nrow(data),ncol=2)
count = 1
alpha = 1.5

#Functions
#GET A PROPER POYLGON
sortPolygon<-function(edge_list)
{
	totalPoint = nrow(edge_list)
	pointsUsed = 0
	prev_size = 0
	polyOutput <- list()
	while(pointsUsed < totalPoint)
	{
		poly <- properPolygon(edge_list)
		poly <- poly[!is.na(poly)]
		points <- (length(poly)-1)
		if (points > prev_size)
		{
			polyOutput <- poly
			prev_size <- points
		}
		pointsUsed = pointsUsed + points 
		edge_list <- edge_list[!((edge_list[,1] %in% poly) & (edge_list[,2] %in% poly)),]
	}
	#print(polyOutput)
	return(polyOutput)
}
#The following function parses an edge list and returns a convexhull
properPolygon<-function(edge_list)
{
	new_edge <- matrix(nrow=nrow(edge_list),ncol=2)
	new_edge[1,] <- edge_list[1,]
	#edge list is 2d array [from - to]
	edge_remain = nrow(edge_list)-1
	points <- list(edge_list[1,])
	prev_id = 1
	starting_v <- edge_list[1,1]
	current_from = edge_list[1,2]
	#if done in middle of array (sometimes the alphashape includes an annoyhing hole)
	done = FALSE
	for(e in 2:nrow(edge_list))
	{
		for(i in 1:nrow(edge_list))
		{
			#print(i)
			if (i == prev_id) next
			edge = edge_list[i,]
			if(current_from %in% edge)
			{
				#FOUND
				to = -1
				#print(edge)
				if(current_from == edge[1]){to = edge[2]}
				else{to = edge[1]}
				#ADJUST
				if (to == starting_v){done = TRUE}
				new_edge[e,]<- c(current_from,to)
				prev_id = i
				current_from <- to
				edge_remain = edge_remain-1
				#print(new_edge)
				#print(edge_remain)
				#print(current_from)
				break
			}
		}
		if (done) break
	}
	polygon <- c(new_edge[1,])
	for(i in 2:nrow(new_edge))
	{
		polygon[length(polygon)+1] <- new_edge[i,2]
	}
	return(polygon)
}

#FUZZY ADJUSTMENT
fuzzy<-function(arr)
{
	new_arr <- matrix(nrow=nrow(arr),ncol=ncol(arr))
	for(i in 1:nrow(arr))
	{
		fuzz_x <- arr[i,1]+ runif(1, -0.00015, 0.00015)
		fuzz_y <- arr[i,2]+ runif(1, -0.00015, 0.00015)
		new_arr[i,] <- c(fuzz_x,fuzz_y)
	}
	return(new_arr) 
}

calAlpha<-function(input)
{
	out <- tryCatch(
        {
            # Just to highlight: if you want to use more than one 
            # R expression in the "try" part then you'll have to 
            # use curly brackets.
            # 'tryCatch()' will return the last evaluated expression 
            # in case the "try" part was completed successfully
		print(paste("AlphaShape: Looking Good! ..."))
            alpha.obj <- ashape(input, alpha = alpha)
	#print(alpha.obj)
 		alpha.obj$edges
            # The return value of `readLines()` is the actual value 
            # that will be returned in case there is no condition 
            # (e.g. warning or error). 
            # You don't need to state the return value via `return()` as code 
            # in the "try" part is not wrapped insided a function (unlike that
            # for the condition handlers for warnings and error below)
        },
        error=function(e) {
            print(paste("Collinear, retrying ..."))
		new_arr = fuzzy(input)
		calAlpha(new_arr)
        }
    )    
    return(out)
}

commFunc<-function(arr)
{
	#transform to numerical array (might be redundant)
	y <- as.numeric(unlist(arr))
	#input matrix
	input = matrix(nrow=length(arr),ncol=2)
	#get coordinates
	for(i in 1:length(y))
	{
		#get each vertex from the global var i.e. data
		v_index <- y[i]
		lat <- data[v_index,][["lat"]]
		long <- data[v_index,][["long"]] 
		lat <- lat
		long <- long
		input[i,] <- c(lat,long)
	}
	#calling ashape
	edges = calAlpha(input)
	#print(edges)
	output <- matrix(nrow=nrow(edges),ncol=2)
	#CREATE PROPER POLYGON FOR CALCULATING AREA
	rawpolygon <- matrix(nrow=nrow(edges),ncol=2)
	rawpolygon[,1] <- edges[,1]
	rawpolygon[,2] <- edges[,2]
	sortedPolygon <- sortPolygon(rawpolygon)
	polygonOut <- matrix(nrow=length(sortedPolygon),ncol=4)

	for(i in 1:length(sortedPolygon))
	{
		id <- y[sortedPolygon[i]]
		lat <- data[id,][["lat"]]
		long <- data[id,][["long"]]
		polygonOut[i,] <- c(lat,long,i-1,current_comm)
	}
	write.table(polygonOut, file=polygonFile, sep=";", 
			row.names=FALSE, col.names=FALSE,
			append=TRUE)
	#EDGE OUTPUT FOR QGIS DRAWING
	for(i in 1:nrow(edges))
	{	
		local_from <- y[edges[i,][["ind1"]]]
		local_to <- y[edges[i,][["ind2"]]]
		v_from <- data[local_from,][["id"]]
		v_to <- data[local_to,][["id"]]
		output[i,] <- c(v_from,v_to)
		#GET LAT/LONG FOR POLYGONS
	}	
	#APPEND TO FILE
	#APPEND QGIS FILE
	write.table(output, file=outputFile, sep=",", 
			row.names=FALSE, col.names=FALSE,
			append=TRUE)
}

#GET THE NUMBER OF COMMUNITY
num_community <- data[nrow(data),"com"]
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
	{
		commFunc(current_comm_members) 
		current_comm_members <- list(i)
		current_comm <- point[["com"]]
	}
	if (i == nrow(data))	commFunc(current_comm_members) #last
}
