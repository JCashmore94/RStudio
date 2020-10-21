getData = function(){
  Coord<- read.csv("r101.csv")
  return (Coord)
}
node_point <- sample(1:100)
node_point
Coord<- read.csv("r101.csv")
calc_dist = function(v1,v2){
  
  dist(rbind(v1,v2), method = "euclidean")
  #For some reason, the code below slows things down. 
  #sqrt((v1[1]-v2[1])*(v1[1]-v2[1]) + (v1[2]-v2[2])*(v1[2]-v2[2]))
}

failed_vehicle = list()
# left capacity, total time, coordinates

MAX_VEHICLES <<- 25    #Global Constant

lappend <- function (lst, ...){
  lst <- c(lst, list(...))
  return(lst)
}


DistanceFunction <- function(node_point){
  start_position = Coord[1,c(2,3)]
  time = 0
  capacity = 200
  total_distance = 0
  numVehicles = 1
  
  prev_coord = start_position
  prev_node = 0
  
  i = 1
  limit = length(node_point)
  
  while(i <= limit && numVehicles < MAX_VEHICLES) {
    node_id = (node_point[i])  + 1
    
    next_distance = as.numeric(calc_dist( prev_coord,  Coord[node_id, c(2,3)] ))
    time_to_next_node = time + next_distance   #but time = 0; so no effect
    wait_time = 0
    
    next_demand = Coord$DEMAND[node_id]
    
    willExceedCapacity = (next_demand > capacity)
    willExceedDueTime = time_to_next_node > Coord$DUE.DATE[node_id]
    
    if (willExceedDueTime || willExceedCapacity) {
      
      coord = unlist(prev_coord, use.names = FALSE)
      item = c(coord, capacity, time)
      failed_vehicle = lappend(failed_vehicle, item)
      
      
      ind = 0
      updated_list = list()
      parked_chacked = FALSE
      
      # test code
      min = 150
      
      for (some_point in failed_vehicle){
        
        willFitCapacity = (next_demand <= some_point[3])
        
        parked_coord = list(some_point[1], some_point[2])
        next_distance_parked = as.numeric(calc_dist( parked_coord,  Coord[node_id, c(2,3)] ))
        
        fit_time_to_next_node = as.numeric(some_point[4]) + next_distance_parked   #but time = 0; so no effect
        willFitTime = fit_time_to_next_node <= Coord$DUE.DATE[node_id]
        
        if (willFitCapacity && willFitTime){
          if (next_distance_parked<min){
            min = next_distance_parked
          }
        }
        
       
       
       
      }
      # -end of  test code
      
      
      for (some_point in failed_vehicle){
        willFitCapacity = (next_demand <= some_point[3])
        
        parked_coord = list(some_point[1], some_point[2])
        next_distance_parked = as.numeric(calc_dist( parked_coord,  Coord[node_id, c(2,3)] ))
       
        fit_time_to_next_node = as.numeric(some_point[4]) + next_distance_parked   #but time = 0; so no effect
        willFitTime = fit_time_to_next_node <= Coord$DUE.DATE[node_id]
        
        new_item = some_point
        
        if (willFitCapacity && willFitTime && (next_distance_parked == min)  ) {
          #update time & distance for parked vehicle & total
          #
          fit_wait = 0
          if (time_to_next_node < Coord$READY.TIME[node_id]){
            fit_wait = Coord$READY.TIME[node_id] - fit_time_to_next_node
          }
          
          new_capacity = as.numeric(some_point[3]) - next_demand
          new_time = as.numeric(some_point[4]) + next_distance_parked
          new_item = c(Coord[node_id, c(2,3)], new_capacity, new_time + fit_wait)
          
         
          
          updated_list = lappend(updated_list, new_item)
          total_distance = total_distance + next_distance_parked
          parked_chacked = TRUE
          break;
          
        } else {
          updated_list = lappend(updated_list, new_item)
        }
       
        
       
        #some_point[1] = 100500
        
      }
      if (parked_chacked == FALSE){
        failed_vehicle = updated_list
        time = 0
        next_distance = calc_dist( start_position,  Coord[node_id, c(2,3)] )
        total_distance = total_distance + next_distance + calc_dist(prev_coord, Coord[1, c(2,3)])
        wait_time = Coord$READY.TIME[node_id] - next_distance
        capacity = 200
        numVehicles = numVehicles + 1
      }
     
      
      
    } else if (time_to_next_node < Coord$READY.TIME[node_id]){
      
      wait_time = Coord$READY.TIME[node_id] - time_to_next_node
    } 
    
    capacity = capacity - next_demand
    total_distance = total_distance + next_distance
    time = time + next_distance + 10 + wait_time
    
    prev_coord = Coord[node_id, c(2,3)]
    i = i +1   # looping variable must increment
    #print(c('vehicles_', numVehicles))
    #print(i)
  }
  #print(failed_vehicle)
  return(c(total_distance, numVehicles, i))
  #print(solution)
}

my_function_final <- function(node_point){
  result = DistanceFunction(node_point)
  num_Vehicle = result[2]
  i = result[3]
  dist = result[1]
  fitness = 1/(1+dist)
  
  if (num_Vehicle > MAX_VEHICLES){
    fitness = 0
  }
  else if (i < 100){
    fitness = 0
  }
  return(c(fitness, num_Vehicle, i))
}

x <-  my_function_final(node_point)
x
#distance <- 1/x[1]
#distance
#node_point = sample(1:100)
#print(node_point)