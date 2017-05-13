# READ FILE - MODIFY THIS
#inputFile <- ""
#outputFile <- ""
# ------------------------------------
inputFile <- ""
outputFile <- ""
#------------- BEGIN MAIN SCRIPT -----------------------
#-------------------------------------------------------
if (file.exists(outputFile)) file.remove(outputFile)
data <- read.table(inputFile, sep=";", header=TRUE)
final <- matrix(nrow=nrow(data),ncol=2)
count = 1
alpha = 0.005

#Functions
fuzzy<-function(arr)
{
	new_arr <- matrix(nrow=nrow(arr),ncol=ncol(arr))
	for(i in 1:nrow(arr))
	{
		fuzz_x <- arr[i,1]+ runif(1, -0.00001, 0.00001)
		fuzz_y <- arr[i,2]+ runif(1, -0.00001, 0.00001)
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
	#plot(alpha.obj)
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
		input[i,] <- c(lat,long)
	}
	#calling ashape
	edges = calAlpha(input)
	output <- matrix(nrow=nrow(edges),ncol=2)
	for(i in 1:nrow(edges))
	{
		local_from <- y[edges[i,][["ind1"]]]
		local_to <- y[edges[i,][["ind2"]]]
		v_from <- data[local_from,][["id"]]
		v_to <- data[local_to,][["id"]]
		output[i,] <- c(v_from,v_to)
	}	
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