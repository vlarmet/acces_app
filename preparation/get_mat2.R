get_distance_matrix2<-function(Graph,from,to,algorithm="phast",allcores=FALSE, reverse=FALSE){

  from<-as.character(from)
  to<-as.character(to)
  
  from_id<-Graph$dict$id[match(from,Graph$dict$ref)]
  to_id<-Graph$dict$id[match(to,Graph$dict$ref)]
  


      
      invrank<-(Graph$nbnode)-Graph$rank
      

  
        if (length(to)< length(from))  res<-Phast3_min(invrank[to_id+1],invrank[from_id+1],invrank[Graph$data$to+1],invrank[Graph$data$from+1],Graph$data[,3],Graph$nbnode, reverse)
        else res<-Phast3_min(invrank[from_id+1],invrank[to_id+1],invrank[Graph$data$from+1],invrank[Graph$data$to+1],Graph$data[,3],Graph$nbnode, reverse)

  return(res)
}