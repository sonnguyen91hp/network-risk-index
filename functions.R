library(data.table)
library(igraph)
library(Cairo)

CairoWin()

distill.structure <- function(nodename) {
#This function distill a vertice's ego network, then return that
#network as well as the diameter of the ego network. ego.size is
#a interger vector of the vertices in the ego network, calculating
#the number of vertices in their own ego network
  ego.graph.in <- make_ego_graph(
    g,
    order = 99,
    nodes = nodename,
    mode = "in",
    mindist = 0
  )
  ego.graph.in <- ego.graph.in[[1]]
  ego.graph.out <- make_ego_graph(
    g,
    order = 99,
    nodes = nodename,
    mode = "out",
    mindist = 0
  )
  ego.graph.out <- ego.graph.out[[1]]
  #extracting "in" and "out" ego networks
  
  attrs <- rbind(as_data_frame(ego.graph.in, "vertices"),
                 as_data_frame(ego.graph.out, "vertices")) %>% unique()
  #uniques of vertices are put into attrs(properties)
  
  el <- rbind(as_data_frame(ego.graph.in),
              as_data_frame(ego.graph.out))
  #el is the combination of edges and all properties of two networks
  
  ego.graph <- graph_from_data_frame(el, directed = T, vertices = attrs)
  #having also attrs because there are properties from the nodes also
  
  diameter <- diameter(ego.graph)
  
  ego.size <- ego_size(ego.graph,order = 99,nodes = V(ego.graph),
                       mode = c("in"),mindist = 0)+
              ego_size(ego.graph,order = 99,nodes = V(ego.graph),
                         mode = c("out"),mindist = 0) - 1
# -1 since each ego calculation count also the node itself
  
  plot(ego.graph,  vertex.color=grp_cls[V(ego.graph)$group],
       vertex.size=ego.size/2,
       vertex.frame.color="blue",
       vertex.label.color="black",
       vertex.label.cex=1.7,
       vertex.label.dist=1.6,
       vertex.label.family="Roboto Condensed",
       edge.arrow.size=2.0,
       edge.curved=0.0,
       edge.width=E(ego.graph)$weight^2*25,
       edge.color=grp_cls[E(ego.graph)$trigger],
       vertex.label.degree=grp_label[V(ego.graph)$group],
       rescale=F,
       layout=cbind(V(ego.graph)$x,V(ego.graph)$y)/4
  )
  return(list("Diameter"=diameter,
              "Egosize"=ego.size))
}

#-----------------------------------------------------------------------------

path.untangle <- function(path.res,g) {
#this function untangle the results return by all_shortest_paths
#and all_simple_paths. The function returns a set of unique edge IDs
#that lie in the paths so that they can be colored in drawing graph.
    a.path <- c()
  for (i in 1:length(path.res)) {
   unlist.unit <- unlist(path.res[i])
   #taking each unit (path) in the list out
   a.path <- c(a.path,as.numeric(E(g,path = unlist.unit)))
  }
  a.path <- unique(a.path)
  return(a.path)
}

#-----------------------------------------------------------------------------

path.probs <- function(path.res,g) {
#This function receive the same inputs, but returns the "jumps" of the
#pathway, the production (combinative probability) of the whole path,
#and the destination where the paths end
  probs <- c()
  jumps <- c()
  des <- c()
  entropies <- c()
  acc.entropy <- c()
  std.dev <- c()
  
for (i in 1:length(path.res)) {
  unlist.unit <- unlist(path.res[i])
  
  vec <- as.numeric(E(g,path = unlist.unit))
  #getting the edge ids from pathway units
  
  jumps <- c(jumps,length(vec))
  #number of jumps
  
  probs <- c(probs,prod(E(g)$weight[vec]))
  #combinative probability calculation
  
  entropies <- c(entropies,sum(E(g)$entropy[vec]))
  #sum the entropies of all edges
  
  des <- c(des,ends(g,E(g)[vec[length(vec)]])[,2])
  #taking the destination of the pathway
  
  acc.prob <- prod(E(g)$weight[vec])
  #to calculate the accumulated probability, cannot use probs
  acc.entropy <- c(acc.entropy,-acc.prob*log2(acc.prob)-(1-acc.prob)*log2(1-acc.prob))
  #calculating entropy based on accumulated probability, which is
  #different from sum of entropies
  
  std.dev <- c(std.dev,sqrt(sum( (E(g)$std[vec])^2 )))
  #calculating accumulated standard deviation
  
}
  return(data.frame(jumps,probs,entropies,std.dev,acc.entropy,des))
}

#-----------------------------------------------------------------------------

reach.ratio <- function(g,mode=c("in","out"))
#This function counts all simple paths in the network and then divide
#it by the number of edges, reflecting the complexity of the network.
{ 
 count <- 0 #to count the number of scenarios
 count.indiv <- c() #to count the number of scenarios for each node
 name <- c() #to record the names of the nodes
 
  for (i in 1:length(V(g)))
  {
    all.path <- length(all_simple_paths(g,from = V(g)[i],mode = mode))
    #how many paths?
    
    count.indiv <- c(count.indiv,all.path)
    #count
    
    name <- c(name,V(g)$name[i])
  }
  count <- sum (count.indiv)
  #total number to calculate Qratio
  
  return(list("Qratio"=count/length(E(g)),
              "individual"=data.frame(name,count.indiv)))
  #one number and one data frame
}

#-----------------------------------------------------------------------------

find.short <- function(probs) 
#This function is to find whether a scenario is the scenario with 
#highest probability. This is to form analyses with scenarios
#that are more certain. It is used in building the library.
#It takes the input as the result of the path.probs function
{
  short <- rep(F,length(probs$probs))
#as the probs in the library is calculated to each node,
#the finding of the most likely pathway can be done in each turn
#of all_simple_path
  uniques <- unique(probs$des)
  #the unique destination of paths
  
  for (i in 1:length(uniques))
  {
    maxima <- max(probs$probs[probs$des==uniques[i]])
    #max for each destination node
    
    unit <- probs$probs==maxima
    #who equal to maxima
    
    short <- short | unit
    #True or False is equal to True, this is done multiple times
  }
  return(short)
}

#-----------------------------------------------------------------------------

library.build <- function(g) 
{
#This function build a library of all paths in the network and
#calculate the probability of all of them. Result is a list of a
#data frame and a list of paths
  From <- c()
  To <- c()
  Probs <- c()
  Jumps <- c()
  Shortest <- c()
  Entropy <- c()
  All.path <- list()
  Acc.entropy <- c()
  Std.dev <- c()
  
  for (i in 1:length(V(g))) 
  {
    if (degree(g, V(g)[i] , mode = "out")!=0)
    {
      all.path <- all_simple_paths(g, from = V(g)[i], mode = "out")
      All.path <- c(All.path,all.path)
      #listing paths
      From <- c(From,rep(V(g)$name[i],length(all.path)))
      #The origin of the path
      probs.list <- path.probs(all.path,g)
      #using path probs to create a data frame of path details
      To <- c(To,probs.list$des)
      #The destination getting from probs
      Probs <- c(Probs,probs.list$probs)
      #The probs
      Entropy <- c(Entropy,probs.list$entropies)
      #The entropy
      Jumps <- c(Jumps,probs.list$jumps)
      #The "steps"
      shortest <- find.short(probs.list)
      Shortest <- c(Shortest,shortest)
      #The finding of shortest path, the highest probability path
      Acc.entropy <- c(Acc.entropy,probs.list$acc.entropy)
      #The entropy based on accumulated probability, which is
      #different from sum of entropies
      Std.dev <- c(Std.dev,probs.list$std.dev)
      #The accumulated standard deviation
    }  
  }
  return(list("Record"=data.frame(From,To,Jumps,Probs,Entropy,Acc.entropy,Std.dev,Shortest),
              "Path.list"=All.path))
}

#-----------------------------------------------------------------------------

examine.bet <- function(g,Paths,id,mode=c("vertex","edge"))
#Examine betweenness: this function examines if a vertex is in numeric
#paths or not input is a list of numeric paths and a numeric vertext
#id output is a boolean vector with the same length of the numeric path
#list
{
 len <- length(Paths)
 bet <- rep(F,len)
 #between or not is default at False
 if (mode=="vertex")  
 {
  for (i in 1:len) 
  {
     unit <- Paths[[i]]
     if ((length(unit)>2) & (id %in% unit[2:(length(unit)-1)])) 
     #if there are only two nodes in a path, then no nodes in between
       {bet[i] <- T}
  }
 }
 else if (mode=="edge")
 {
  for (i in 1:len) 
  {
    unit <- E(g,path = Paths[[i]])
    #Getting edge ids from a path----- "path=" is strictly needed
    if (id %in% unit) 
    {bet[i] <- T}
  }
}  
  return(bet) 
}

#-----------------------------------------------------------------------------


trigger.centrality <- function(g,mode=c("vertex","edge"))
#This function calculate the betweenness centrality, but by using
#the new index of probability. Outputs are two centralities one
#is based on all, and one is based on shortest (highest probs)
{
  Library <- library.build(g)
  Paths <- Library$Path.list
  Stats <- Library$Record
  Central.no.weight <- c()
  Central.no.weight.short <- c()
  Centrality <- c()
  Central.short <- c()
  #.short is to record the index calculated by include only highest
  #probability paths
  if (mode=="vertex") 
  {
    for (i in 1:length(V(g))) 
    {
      if ((degree(g, V(g)[i] , mode = "out")!=0) &
          (degree(g, V(g)[i] , mode = "in")!=0))
      #Only for risks in Group 2
      {
        include <- examine.bet(g,Paths,i,mode="vertex")
        #Include in the path or not
        centrality <- sum(Stats$Probs[include])
        Centrality <- c(Centrality,centrality)
        #gather probs of including paths
        add.condi <- ((include) & (Stats$Shortest))
        #condition added for highest probs paths
        central.short <- sum(Stats$Probs[add.condi])
        Central.short <- c(Central.short,central.short)
        #gather probs of shortest paths
        central.no.weight <- sum(include)
        Central.no.weight <- c(Central.no.weight,central.no.weight)
        #gather counts of including paths
        central.no.weight.short <- sum(add.condi)
        Central.no.weight.short <- c(Central.no.weight.short,central.no.weight.short)
        #gather counts of shortest paths
      }
      else
      {
        Centrality <- c(Centrality,0)
        Central.short <- c(Central.short,0)
        Central.no.weight <- c(Central.no.weight,0)
        Central.no.weight.short <- c(Central.no.weight.short,0)
        #If not, just put 0 for network drawing
      }  
      
    }
    
    
  } 
  else if (mode=="edge") 
  {
  for (i in 1:length(E(g))) 
    {
      include <- examine.bet(g,Paths,i,mode="edge")
      centrality <- sum(Stats$Probs[include])
      Centrality <- c(Centrality,centrality)
      add.condi <- ((include) & (Stats$Shortest))
      central.short <- sum(Stats$Probs[add.condi])
      Central.short <- c(Central.short,central.short)
      
      central.no.weight <- sum(include)
      Central.no.weight <- c(Central.no.weight,central.no.weight)
      #gather counts of including paths
      central.no.weight.short <- sum(add.condi)
      Central.no.weight.short <- c(Central.no.weight.short,central.no.weight.short)
      #gather counts of shortest paths
    }
  }
  else
  {print("Input mode not supported")}
  
  return(data.frame(Centrality,Central.short,Central.no.weight,Central.no.weight.short))
}

#-----------------------------------------------------------------------------

trigger.index <- function(g)
#This index is similar to Closeness centrality. It is calculated for
#begin or end of edges and paths
{
 Library <- library.build(g)
 Stats <- Library$Record

#For calculating nodes as the beginning of paths-----------------------
 Close.trigg.out <- c()
 Close.short.out <- c()
 CNWO <- c()
 #Close.no.weight.out
 CNWSO <- c()
 #Close.no.weight.short.out
 
  for (i in 1:length(V(g))) 
  {
    if (degree(g, V(g)[i] , mode = "out")!=0)
    {
     include <- Stats$From==V(g)$name[i]
     close.trigg <- sum(Stats$Probs[include])
     Close.trigg.out <- c(Close.trigg.out,close.trigg)
     cNWO <- sum(include)
     CNWO <- c(CNWO,cNWO)
     
     add.condi <- ((include) & (Stats$Shortest))
     close.short <- sum(Stats$Probs[add.condi])
     Close.short.out <- c(Close.short.out,close.short)
     cNWSO <- sum(add.condi)
     CNWSO <- c(CNWSO,cNWSO)
     
    }
    else
    {
     Close.trigg.out <- c(Close.trigg.out,0)
     Close.short.out <- c(Close.short.out,0)
     CNWO <- c(CNWO,0)
     CNWSO <- c(CNWSO,0)
    }
  }

#For calculating nodes as the ends of paths------------------
 Close.trigg.in <- c()
 Close.short.in <- c()
 CNWI <- c()
 #Close.no.weight.out
 CNWSI <- c()
 #Close.no.weight.short.out

      
  for (i in 1:length(V(g))) 
  {
    if (degree(g, V(g)[i] , mode = "in")!=0)
    {
      include <- Stats$To==V(g)$name[i]
      close.trigg <- sum(Stats$Probs[include])
      Close.trigg.in <- c(Close.trigg.in,close.trigg)
      cNWI <- sum(include)
      CNWI <- c(CNWI,cNWI)
      
      add.condi <- ((include) & (Stats$Shortest))
      close.short <- sum(Stats$Probs[add.condi])
      Close.short.in <- c(Close.short.in,close.short)
      cNWSI <- sum(add.condi)
      CNWSI <- c(CNWSI,cNWSI)
    }
    else
    {
      Close.trigg.in <- c(Close.trigg.in,0)
      Close.short.in <- c(Close.short.in,0)
      CNWI <- c(CNWI,0)
      CNWSI <- c(CNWSI,0)
    }
  }
return(data.frame(Close.trigg.out,Close.short.out,CNWO,
                  CNWSO,
                  Close.trigg.in,Close.short.in,CNWI,
                  CNWSI))
}

#-----------------------------------------------------------------------------

prevent.diff <- function(g)
#Prevention difficulty:
#A function to calculate the minimum edges need to be removed to
#stop a connection from a risk to another (edge_connectivity)
#then sum up the results of each node 
#This function shoulld be examined if there are parts of the net
#work that do not connect
{
  Connectivity <- c()
  From <- c()
  To <- c()
  
  for (i in 1:length(V(g)))
  {
    for (j in 1:length(V(g)))
    {
      From.check <- (degree(g, V(g)[i] , mode = "out") != 0)
      To.check <- (degree(g, V(g)[j] , mode = "in") != 0)
      Same.check <- (i!=j)
      if (From.check & To.check & Same.check)
      {
        connectivity <- edge_connectivity(g,source=V(g)$name[i],
                                          target=V(g)$name[j],
                                          checks=T)
        Connectivity <- c(Connectivity,connectivity)
        From <- c(From,V(g)$name[i])
        To <- c(To,V(g)$name[j])
      }
    }
  }
  
  sum.from <- c()
  #sum the connectivity from a node
  for (i in 1:length(V(g)))
  {
    sum.from <- c(sum.from,sum(Connectivity[From==V(g)$name[i]]))
  }
                
  sum.to <- c()
  #sum the connectivity to a node
  for (i in 1:length(V(g)))
  {
    sum.to <- c(sum.to,sum(Connectivity[To==V(g)$name[i]]))
  }
  
  Individual <- data.frame(From,To,Connectivity)
  Summary.from <- data.frame(V(g)$name,sum.from)
  Summary.to <- data.frame(V(g)$name,sum.to)
  
  return(list("Individual"=Individual,
              "Summary.from"=Summary.from,
              "Summary.to"=Summary.to))
}

#-----------------------------------------------------------------------------

entropy.element <- function(g)
#entropy of each edge will be calculated and stored in E(g)$entropy
#entropy of each node will be calculated and stored in V(g)$entropy
{
 #--------Edge-------------
 Entropy.E <- c()
 
 for (i in 1:length(E(g)))
 {
  ele <-  E(g)$weight[i]
  element <- -ele*log2(ele)-(1-ele)*log2(1-ele)
  #Entropy is the amount of information if such an event reveal, so
  #if the prob is 0.5, the information is maximum (1bit), this 
  #is the quantified amount of uncertainty, too
  Entropy.E <- c(Entropy.E,element)
 } 
 
 #E(g)$entropy <- Element
 
 #--------Vertice-------------
 Entropy.V <- c()

#add ends of the edges to E(g)$from and E(g)$to
#E(g)$from <- ends(g, E(g),names=F)[,1]
#E(g)$to <- ends(g, E(g),names=F)[,2]
 
 for (i in 1:length(V(g)))
 {
   include <- (E(g)$from==i) | (E(g)$to==i)
   element <- sum(E(g)$entropy[include])
   Entropy.V <- c(Entropy.V,element)
 } 
 
 #V(g)$entropy <- Element
 return(list(Edge_entropy=Entropy.E,Vertices_entropy=Entropy.V))  
}

#-----------------------------------------------------------------------------

entropy.calculate <- function(g)
#This function calculate the Ivd index for the whole network 
{
  g_sample <- as.undirected(g, mode = "collapse")
  #convert to undirected network
  degree <- centr_degree(g_sample)$res
  #using centr_degree to calculate the connected edges of vertices
  Sum <- sum(degree*log2(degree))
  #Calculate the total Ivd of the network
  return(Sum)
  
}

#-----------------------------------------------------------------------------

entropy.structure <- function(g)
#This function calculate   
{
  Ivd.original <- entropy.calculate(g)
  #The Ivd of the original network
  En_Str <- c()
  
  for (i in 1:length(E(g)))
  {
    g_instance <- delete_edges(g,i)
    Comp <- components(g_instance)
    #This calculation is to check if the network is disconnected 
    #after the deletion of an edge
    if (Comp$no>1)
    {
      g_instance <- delete_vertices(g_instance,which(Comp$membership>1))
      #The "which" expression is to return the id of the nodes that
      #were separated
    }
    
    Ivd.new <- entropy.calculate(g_instance)
    Ivd.contribute <- Ivd.original - Ivd.new
    #Ivd of the original network minus the Ivd of the network in which
    #the edge has been removed
    En_Str <- c(En_Str,Ivd.contribute)
  }
  
  return(En_Str)
}

#-------------------------------------------------------------#

size.mapping <- function(Vec,map.min,map.max)
#This function map points in a vector linearly into the range of
#map.max and map.min. Using for mapping color or size of plots
{
  size.mapping <- (Vec-min(Vec))/(max(Vec)-min(Vec))*(map.max-map.min)
  size.mapping <- size.mapping+map.min
  return(size.mapping)
}  
