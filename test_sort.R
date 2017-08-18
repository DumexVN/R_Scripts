properPolygon<-function(edge_list)
{
	print(edge_list)
	new_edge <- matrix(nrow=nrow(edge_list),ncol=2)
	new_edge[1,] <- edge_list[1,]
	#edge list is 2d array [from - to]
	edge_remain = nrow(edge_list)-1
	points <- list(edge_list[1,])
	prev_id = 1
	current_from = edge_list[1,2]
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
				if (to == -1)
				{
					print("ERROR OCCURED WHILE PARSING POLYGON")
					break
				}
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
	}
	polygon <- c(new_edge[1,])
	for(i in 2:nrow(new_edge))
	{
		polygon[length(polygon)+1] <- new_edge[i,2]
	}
	print(polygon)
}


p <- rbind(c(1,2),c(3,2),c(4,5),c(3,5),c(1,4))
properPolygon(p)