library(data.table)
library(igraph)
library(Cairo)
library(centiserve)
library(expm)
library(linkcomm)
library(DescTools)
library(ggplot2)
library(extrafont)
library(ggpubr)
library(openxlsx)
library(corrplot)

Edges <- Survey1[c(1,2),]
#two first rows are for "from" and "to" of the edges

Edgesok <- transpose(as.data.frame(Edges))
Edgesok <- Edgesok[c(-1,-2,-3,-35),]
#deleting uneccesary rows

rownames(Edgesok) <- 1:nrow(Edgesok)
#assigning new names, as the row ids remained after deletion (optional)

colnames(Edgesok) <- c("from","to")

Nodes <- as.data.frame(unique(c(Edgesok$from,Edgesok$to)))
#extracting the name of the nodes

colnames(Nodes) <- "name"

g <- graph_from_data_frame(d=Edgesok,directed=T,vertices=Nodes)
#create g

plot(g, edge.arrow.size=0.1, vertex.color="green", vertex.size=12,
     vertex.frame.color="blue", vertex.label.color="purple",
     vertex.label.cex=1, vertex.label.dist=2, edge.curved=0)

E(g)
V(g)
g

V(g)$group <- c(rep(1,7),rep(2,10),rep(3,8))
#groups of risk based on their positions in the network

E(g)$exp <- c(rep(1,31),rep(2,23))
#groups of edges based on expertise mobilized

Edges.ends <- ends(g,seq(1,length(E(g)),1))
E(g)$name <- paste(Edges.ends[,1],Edges.ends[,2])
#name for edges

list.edge.num <- ends(g,es=E(g),names=F)
#A numeric list nodes

edge.num <- as.numeric(list.edge.num[,1]<8) + 3*(list.edge.num[,2]>17)
edge.num[edge.num==0] <- 2
#marking edges based on their from and to, Group 1 of edges from
#Group 1 nodes, Group 3 of edges to Group 3 nodes, rest is Group 2

E(g)$trigger <- edge.num

ProbIT <- Survey1[Survey1$X2=="A1",seq(from=4,to=34,by=1)]
#selecting the the rows and collumns for assessment from IT exp
ProbIT <- ProbIT[rowSums(is.na(ProbIT)) != ncol(ProbIT),]
#deleting rows that are NA
ProbIT <- lapply(ProbIT, as.numeric)
#covering to numeric

ProbCOpt <- Survey1[Survey1$X2=="A2",seq(from=36,to=58,by=1)]
ProbCOpt <- ProbCOpt[rowSums(is.na(ProbCOpt)) != ncol(ProbCOpt),]
ProbCOpt <- lapply(ProbCOpt, as.numeric)

WE.IT <- lapply(ProbIT, mean)
WE.COpt <- lapply(ProbCOpt, mean)
std.IT <- lapply(ProbIT, sd)
std.COpt <- lapply(ProbCOpt, sd)
WE <- unname(unlist(c(WE.IT,WE.COpt)))
E(g)$weight <- WE
std <- unname(unlist(c(std.IT,std.COpt)))
E(g)$std <- std
#weight and std are available as data of edges

grp_cls <- c("seagreen4","tomato","maroon")
#colors of the groups of edges and nodes
grp_label <- c(-pi/2,0,pi/2)
#the degree of labels of nodes

fr <- layout_with_fr(g,niter=10000)
kk <- layout_with_kk(g)
lgl <- layout_with_lgl(g)
ncl <- layout_nicely(g)
grd <- layout_on_grid(g)
dh <- layout_with_dh(g)
gem <- layout_with_gem(g)
tr <- layout_as_tree(g)
#multiple types of layouts of network

hist(E(g.cut.edge)$weight)
CairoWin()
#initializing cairo for drawing

cut.off <- median(E(g)$weight)
g.cut.edge <- delete_edges(g, E(g)[weight<cut.off])
#deleting edges that have weights smaller than cut off threshold


plot(g.cut.edge,  vertex.color=grp_cls[V(g)$group], vertex.size=8,
     vertex.frame.color="blue", vertex.label.color="black",
     vertex.label.cex=1.7, vertex.label.dist=1.6,vertex.label.family="Roboto Condensed",
     edge.arrow.size=2.0,edge.curved=0.0,
     edge.width=E(g)$weight^2*15,edge.color=grp_cls[E(g)$trigger],
     vertex.label.degree=grp_label[V(g)$group],rescale=T,
     )

cut.off <- median(E(g)$std)
g.cut.edge <- delete_edges(g, E(g)[std<cut.off])
g.cut.edge <- delete_edges(g.cut.edge, E(g.cut.edge)[std>cut.off])

plot(g.cut.edge,  vertex.color=grp_cls[V(g.cut.edge)$group], vertex.size=degree(g,mode="all")/2+3,
     vertex.frame.color="black", vertex.label.color=grp_cls[V(g.cut.edge)$group],
     vertex.label.cex=1.7, vertex.label.dist=1.6,vertex.label.family="Roboto Condensed",
     edge.arrow.size=2.5,edge.curved=0.0,
     edge.width=E(g.cut.edge)$std^2.5*300,edge.color=grp_cls[E(g.cut.edge)$trigger],
     vertex.label.degree=grp_label[V(g.cut.edge)$group],rescale=F,
     layout=cbind(V(g)$x,V(g)$y)/4)

plot(g.cut.edge,  vertex.color=grp_cls[V(g.cut.edge)$group], vertex.size=degree(g,mode="all")/2+3,
     vertex.frame.color="black", vertex.label.color="black",
     vertex.label.cex=1.7, vertex.label.dist=1.6,vertex.label.family="Roboto Condensed",
     edge.arrow.size=2.5,edge.curved=0.0,
     edge.width=E(g.cut.edge)$weight^2*15,edge.color=grp_cls[E(g.cut.edge)$trigger],
     vertex.label.degree=grp_label[V(g.cut.edge)$group],rescale=F,
     layout=cbind(V(g)$x,V(g)$y)/4)

cut.off <- 0.35
g.cut.edge <- delete_edges(g, E(g)[weight<cut.off])

Survey <- Survey1[is.na(Survey1$X1)==F,]
dim(Survey[(Survey$X2=="A1" & Survey$X1=="P"),])
dim(Survey[(Survey$X2=="A1" & Survey$X1=="S"),])
dim(Survey[(Survey$X2=="A1" & Survey$X1=="F"),])
dim(Survey[(Survey$X2=="A2" & Survey$X1=="P"),])
dim(Survey[(Survey$X2=="A2" & Survey$X1=="S"),])
dim(Survey[(Survey$X2=="A2" & Survey$X1=="F"),])
#controlling how many data received in each sector

g.induced <- induced_subgraph(g,c("CyBr","StID","PfLw","ExIB","DySC","DySP","CyRs"))
plot(g.induced,  vertex.color=grp_cls[V(g.induced)$group], vertex.size=8,
     vertex.frame.color="black", vertex.label.color=grp_cls[V(g.induced)$group],
     vertex.label.cex=1.7, vertex.label.dist=1.6,vertex.label.family="Roboto Condensed",
     edge.arrow.size=2.5,edge.curved=0.0,
     edge.width=E(g.induced)$std^2.5*300,edge.color=grp_cls[E(g.induced)$trigger],
     vertex.label.degree=grp_label[V(g.induced)$group],rescale=F,
     layout=cbind(V(g.induced)$x,V(g.induced)$y)/4)
#graph that only include the nodes as specified

?degree

degree(g,mode="all")
degree(g,mode="in")
degree(g,mode="out")

plot(degree_distribution(g, cumulative = T))
plot(degree_distribution(g, cumulative = F))

cut.off <- mean(degree(g,mode="all"))

g.induced <- induced_subgraph(g,V(g)[degree(g,mode="all")>cut.off])

plot(g.induced,  vertex.color=grp_cls[V(g.induced)$group],
     vertex.size=degree(g.induced,mode="all")/2+3,
     vertex.frame.color="blue",
     vertex.label.color="black",
     vertex.label.cex=1.7,
     vertex.label.dist=1.6,
     vertex.label.family="Roboto Condensed",
     edge.arrow.size=2.0,
     edge.curved=0.0,
     edge.width=E(g.induced)$weight^2.5*35,
     edge.color=grp_cls[E(g.induced)$trigger],
     vertex.label.degree=grp_label[V(g.induced)$group]
     )

?neighborhood
#ego functions
?incident
incident(g,"CyBr",mode="in")
incident(g,"CyBr",mode="all")
#the immediate connections to neighbors 

ego.size <- ego_size(g,order = 4,nodes = V(g),
                     mode = c("in"),mindist = 0)+ego_size(
                             g,order = 4,nodes = V(g),
                        mode = c("out"),mindist = 0)
#size of the ego network, "all" is different from "in" +"out"

ego.size <- cbind(V(g)$name,ego.size)
#for better views


plot(g,  vertex.color=grp_cls[V(g)$group],
     vertex.size=9,
     vertex.frame.color="blue",
     vertex.label.color="black",
     vertex.label.cex=1.7,
     vertex.label.dist=1.6,
     vertex.label.family="Roboto Condensed",
     edge.arrow.size=2.0,
     edge.curved=0.0,
     edge.width=E(g)$weight^2*25,
     edge.color=grp_cls[E(g)$trigger],
     vertex.label.degree=grp_label[V(g)$group]
)

ego.graph <- make_ego_graph(
        g,
        order = 4,
        nodes = "DySP",
        mode = "in",
        mindist = 0
)
#making ego graph for each node
ego.graph <- ego.graph[[1]]
#make_ego_grapp returns a list
ego.graph <- make_ego_graph(
        g,
        order = 4,
        nodes = "DaCp",
        mode = "out",
        mindist = 0
)
ego.graph <- ego.graph[[1]]

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

cut.off <- median(E(g)$weight)
g.cut.edge <- delete_edges(g, E(g)[weight<cut.off])

ego.graph <- make_ego_graph(
        g,
        order = 2,
        nodes = "PfLw",
        mode = "all",
        mindist = 0
)
ego.graph <- ego.graph[[1]]

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

ego.graph.in <- make_ego_graph(
        g,
        order = 4,
        nodes = "PfLw",
        mode = "in",
        mindist = 0
)
ego.graph.in <- ego.graph.in[[1]]
ego.graph.out <- make_ego_graph(
        g,
        order = 4,
        nodes = "PfLw",
        mode = "out",
        mindist = 0
)
ego.graph.out <- ego.graph.out[[1]]

attrs <- rbind(as_data_frame(ego.graph.in, "vertices"),
               as_data_frame(ego.graph.out, "vertices")) %>% unique()
el <- rbind(as_data_frame(ego.graph.in),
            as_data_frame(ego.graph.out))
#combining to receive a bowtie diagram for each node
ego.graph <- graph_from_data_frame(el, directed = T,
                                   vertices = attrs)

is_dag(g)
topo_sort(g)
transitivity(g)
#also called clustering coefficient, 
transitivity(g,vids = "CyBr",type="local")
transitivity(g,vids = "CyBr",type="global")
transitivity(g,vids = V(g),type="weighted")
#the probability that the connected neighbors also form connections
#"triples" "triangles". Calculating by the number of the triangles
#divided by the potential triangles
diameter(g)
#the length of the longest geodesic (the longest shortest path)
#in case of weighted network, the weights will be calculated
diameter(g, weights=NA)
get_diameter(ego.graph,weights=NA)

all_simple_paths(g,from="DySP",mode = "in")
#return all paths from a vertex or between two vertices
#"from" should always be filled, then "mode" can be used to indicate

cluster_walktrap
cluster_edge_betweenness
cluster_fast_greedy
cluster_spinglass
#algorithms to test for modularity

wtc <- cluster_walktrap(g)
modularity(wtc)
modularity(g, membership(wtc))
#modularity is normally used for undirected graph only


?assortativity
#assortativity measure the tendency of the network that similar
#vertices based on a quality tend to connect to each other
#it seems to be a type of quantifying the homophily of a network

Pagerank <- page_rank(g, algo = c("prpack"),
          vids = V(g), directed = TRUE, damping = 0.85,
          personalized = NULL, weights = NULL, options = NULL)
#to rank nodes based on its connectivity and random surfing activities
V(g)$PageRank <- Pagerank$vector

plot(g,  vertex.color=grp_cls[V(g)$group],
     vertex.size=V(g)$PageRank*100,
     vertex.frame.color="blue",
     vertex.label.color="black",
     vertex.label.cex=1.7,
     vertex.label.dist=1.6,
     vertex.label.family="Roboto Condensed",
     edge.arrow.size=2.0,
     edge.curved=0.0,
     edge.width=E(g)$weight^2*25,
     edge.color=grp_cls[E(g)$trigger],
     vertex.label.degree=grp_label[V(g)$group]
     #rescale=F,
     #layout=cbind(V(g)$x,V(g)$y)/4
)

?page_rank
?diameter
?transitivity
?topo_sort
?union
#union cannot combine graphs with the same attributes
?intersection
?difference
?as_data_frame
#delete nodes and edges then compare the overall risk situation

averagedis(g, vids = "CyBr",mode = "all")
bottleneck(g,mode = "in")
bottleneck(g,mode = "out")
centroid(g, mode = "all")
closeness.currentflow(g)
closeness_latora<- closeness.latora(g, mode = "in",
                 normalized = F) + closeness.latora(g, mode = "out",
                                                 normalized = F)
closeness.residual(g,mode="out")+closeness.residual(g,mode="in")
clusterrank(g, directed = T)     
communitycent(g, vids = V(g), type = "commweight",normalise = F)
cluster <- cluster_edge_betweenness(g, directed = T, edge.betweenness = T,
                                    merges = T,bridges = T, membership = T)
plot(cluster,g)
edge.betweenness(g,directed=T,weights = NULL)

betweenness(g,v = V(g),directed = T,weights = NULL,normalized = F)

cluster <- cluster_fast_greedy(g1, merges = T,modularity = T,membership = T)
plot(cluster,g1)
#fast greedy does not apply for directed network

cluster <- cluster_infomap(g,nb.trials = 1000, modularity = F)
g1 <- as.undirected(g,mode="collapse")

cluster <- cluster_label_prop(g1)
plot(cluster,g1)

cluster <- cluster_optimal(g1)
plot(cluster,g1)
#using modularity, this is not suitable for directed graph

cluster <- cluster_leading_eigen(g1,
                                 steps = -1,
                                 weights = NULL,
                                 start = NULL,
                                 options = arpack_defaults,
                                 callback = NULL,
                                 extra = NULL,
                                 env = parent.frame())
plot(cluster,g1)

cluster <- cluster_louvain(g1)
plot(cluster,g1)

?cluster_spinglass()
cluster <- cluster_spinglass(
        g,
        weights = NULL,
        vertex = NULL,
        spins = 20,
        parupdate = FALSE,
        start.temp = 1,
        stop.temp = 0.01,
        cool.fact = 0.99,
        update.rule = "config",
        gamma = 1,
        implementation = "orig",
        gamma.minus = 1
)
plot(cluster,g)

cluster <- cluster_walktrap(
        g,
        weights = E(g)$weight,
        steps = 8,
        merges = TRUE,
        modularity = TRUE,
        membership = TRUE
)
plot(cluster,g)

?clusters()

cluster_infomap()


# Plot
layout <-layout.fruchterman.reingold(g)
plot(cluster, g, layout=layout, vertex.label=NA, vertex.size=5,  edge.arrow.size=.2)

# Change colors of nodes, polygons, and polygon borders
new_cols <- c("white", "red", "black","blue")[membership(cluster)]
plot(cluster, g, col=new_cols, mark.border="black", mark.col=c("tan", "pink", "lightgray","darkseagreen1"), 
     vertex.label=NA, vertex.size=5, edge.arrow.size=.2)

diffusion.degree(g, vids = V(g), mode = "in",
                 lambda = 1)+
        diffusion.degree(g, vids = V(g), mode = "out",
                                              lambda = 1)

entropy(g, mode="all")+entropy(g, mode="out")

epc(g, vids = V(g), threshold = 0.5)

geokpath(g, vids = V(g), mode = "in",
         weights = NULL, k = 3)+
        geokpath(g, vids = V(g), mode = "out",
                                        weights = NULL, k = 3)
hubbell(g, vids = V(g), weights = NULL, weightfactor = 0.5)

katzcent(g, vids = V(g), alpha = 0.1)
laplacian(g, vids = V(g), mode = "out")+laplacian(g, vids = V(g), mode = "in")

leaderrank(g, vids = V(g))
lincent(g, vids = V(g), mode ="all")
lobby(g, vids = V(g), mode = "in")+lobby(g, vids = V(g), mode = "out")
markovcent(g1, vids = V(g1))
pairwisedis(g, vids = V(g))
radiality(g, vids = V(g), mode = "all")
salsa(g, vids = V(g), score = "authority")+salsa(g, vids = V(g), score = "hub")
semilocal(g, vids = V(g), mode = "out")
semilocal(g, vids = V(g), mode = "in")
semilocal(g, vids = V(g), mode = "all")

plot(g,  vertex.color=grp_cls[V(g)$group],
     vertex.size=V(g)$PageRank*100,
     vertex.frame.color="blue",
     vertex.label.color="black",
     vertex.label.cex=1.7,
     vertex.label.dist=1.6,
     vertex.label.family="Roboto Condensed",
     edge.arrow.size=2.0,
     edge.curved=0.0,
     edge.width=E(g)$weight^2*25,
     edge.color=grp_cls[E(g)$trigger],
     vertex.label.degree=grp_label[V(g)$group]
     #rescale=F,
     #layout=cbind(V(g)$x,V(g)$y)/4
)
ExFvals <- sapply(1:vcount(g1),ExFW, g1)
ExFvals <- sapply(1:vcount(g),ExFW, g)
#function provided by Glenn Lawyer, to measure the influence from group1
#the potential of causing multiple-event scenarios, called "spreading power"
#https://github.com/glennlawyer/ExpectedForce/blob/master/ExFWeightedImplementation.R



#Decorating experiments

grp_cls <- c("#659B43","#DEA900","#C95B11")
#colors of the groups of edges and nodes
ColToHex(c("navy","darkgoldenrod","darkred"))
grp_cls_add <- c("#00008060","#B8860B60","#8B000060")

fr <- layout_with_fr(g,niter=10000)
kk <- layout_with_kk(g)
lgl <- layout_with_lgl(g)
ncl <- layout_nicely(g)
grd <- layout_on_grid(g)
dh <- layout_with_dh(g)
gem <- layout_with_gem(g)
tr <- layout_as_tree(g)


plot(g,  vertex.color=grp_cls[V(g)$group],
     vertex.size=14,
     #vertex.size2=10,
     vertex.frame.color="blue",
     vertex.label.color="white",
     vertex.label.cex=1.7,
     vertex.label.dist=0,
     vertex.label.family="Roboto Condensed",
     vertex.label.degree=grp_label[V(g)$group],
     vertex.shape="circle",
     edge.arrow.size=2.0,
     edge.curved=0.0,
     edge.width=E(g)$weight^2*20,
     edge.color=grp_cls_add[E(g)$trigger],
     #rescale=F,
     layout=gem
)

vertex.shapes (shape = NULL)
V(g)$shape <- "rectangle" #"raster""rectangle""sphere""square""vrectangle"
#"circle""crectangle""csquare""none""pie""raster"
#pie chart can be vertices as well, interesting.
#raster can be used to put images as vertices

V(g)$ego.size <- ego.size

plot(g,  vertex.color=grp_cls[V(g)$group],
     vertex.size=20,
     vertex.size2=13,
     vertex.frame.color="blue",
     vertex.label.color="white",
     vertex.label.cex=1.7,
     vertex.label.dist=0,
     vertex.label.family="Roboto Condensed",
     vertex.label.degree=grp_label[V(g)$group],
     vertex.shapes="vrectangle",
     edge.arrow.size=0.0,
     edge.curved=0.0,
     edge.width=E(g)$weight^2*25,
     edge.color=grp_cls_add[E(g)$trigger],
     rescale=F,
     layout=cbind(V(g)$x/3.7,V(g)$y/3)
)



closeness_latora<- closeness.latora(g, mode = "in",normalized = F)+
        closeness.latora(g, mode = "out",normalized = F)
color.palette.v <- colorRampPalette(c("gray70","darkorange","orangered3"))
color.space.v <- color.palette.v(1001)
#Value.color.v <- closeness_latora
Try.v <- trigger.centrality(g,"vertex")
Value.color.v <- Try.v$Centrality
minc <- min(Value.color.v)
maxc <- max(Value.color.v)
color.pos.v <- trunc((Value.color.v-minc)/(maxc-minc)*1000)+1

#color.palette.e <- colorRampPalette(c("gray80","gray20"))
color.palette.e <- colorRampPalette(c("#CCCCCC30","#33333399"),alpha=T)
color.space.e <- color.palette.e(1001)
#Value.color.e <- E(g)$weight
Try.e <- trigger.centrality(g,"edge")
Value.color.e <- Try.e$Centrality
minc <- min(Value.color.e)
maxc <- max(Value.color.e)
color.pos.e <- trunc((Value.color.e-minc)/(maxc-minc)*1000)+1

plot(g,
     vertex.color=color.space.v[color.pos.v],
     vertex.size=V(g)$ego.size+4,
     #vertex.size2=10,
     vertex.frame.color="blue",
     vertex.label.color="white",
     vertex.label.cex=(V(g)$ego.size+5)/12,
     vertex.label.dist=0,
     vertex.label.family="Roboto Condensed",
     vertex.label.degree=grp_label[V(g)$group],
     vertex.shape="circle",
     edge.arrow.size=1.5,
     edge.curved=0.0,
     edge.width=5,
     edge.color=color.space.e[color.pos.e],
     rescale=T,
     layout=gem#fr#kk#ncl#lgl#grd#dh#gem#cbind(V(g)$x/4.1,V(g)$y/3)
)

#highlight a path or a set of paths-------------------------

ego.graph.in <- make_ego_graph(
        g,
        order = 99,
        nodes = "PfLw",
        mode = "in",
        mindist = 0
)
ego.graph.in <- ego.graph.in[[1]]
ego.graph.out <- make_ego_graph(
        g,
        order = 99,
        nodes = "PfLw",
        mode = "out",
        mindist = 0
)
ego.graph.out <- ego.graph.out[[1]]

attrs <- rbind(as_data_frame(ego.graph.in, "vertices"),
               as_data_frame(ego.graph.out, "vertices")) %>% unique()
el <- rbind(as_data_frame(ego.graph.in),
            as_data_frame(ego.graph.out))
#combining to receive a bowtie diagram for each node
ego.graph <- graph_from_data_frame(el, directed = T,
                                   vertices = attrs)

closeness_latora<- closeness.latora(ego.graph, mode = "in",normalized = F)+
        closeness.latora(ego.graph, mode = "out",normalized = F)

color.palette.v <- colorRampPalette(c("gray70","darkorange","orangered3"))
color.space.v <- color.palette.v(1001)
Value.color.v <- closeness_latora
minc <- min(Value.color.v)
maxc <- max(Value.color.v)
color.pos.v <- trunc((Value.color.v-minc)/(maxc-minc)*1000)+1

#color.palette.e <- colorRampPalette(c("gray80","gray20"))
color.palette.e <- colorRampPalette(c("#CCCCCC40","#33333340"),alpha=T)
color.space.e <- color.palette.e(1001)
Value.color.e <- E(ego.graph)$weight
minc <- min(Value.color.e)
maxc <- max(Value.color.e)
color.pos.e <- trunc((Value.color.e-minc)/(maxc-minc)*1000)+1

ego.size <- ego_size(g,order = 4,nodes = V(g),
                     mode = c("in"),mindist = 0)+
            ego_size(g,order = 4,nodes = V(g),
                     mode = c("out"),mindist = 0)-1

fr <- layout_with_fr(ego.graph,niter=10000)
kk <- layout_with_kk(ego.graph)
lgl <- layout_with_lgl(ego.graph)
ncl <- layout_nicely(ego.graph)
grd <- layout_on_grid(ego.graph)
dh <- layout_with_dh(ego.graph)
gem <- layout_with_gem(ego.graph)
tr <- layout_as_tree(ego.graph)

plot(ego.graph,
     vertex.color=color.space.v[color.pos.v],
     vertex.size=V(ego.graph)$ego.size+4,
     #vertex.size2=10,
     vertex.frame.color="blue",
     vertex.label.color="white",
     vertex.label.cex=(V(ego.graph)$ego.size+5)/11,
     vertex.label.dist=0,
     vertex.label.family="Roboto Condensed",
     vertex.label.degree=grp_label[V(ego.graph)$group],
     vertex.shape="circle",
     edge.arrow.size=1.5,
     edge.curved=0.0,
     edge.width=5,
     edge.color=color.space.e[color.pos.e],
     rescale=T,
     layout=lgl#fr#kk#ncl#lgl#grd#dh#gem#cbind(V(g)$x/4.1,V(g)$y/3)
)

short.path <- all_shortest_paths(g,"CyBr","CyRs",mode="all",weights=NA)
#color.pos.e[as.numeric(E(g,path = unlist(short.path$res[1])))] <- 1001
path.color <- color.space.e[color.pos.e]
path.color[as.numeric(E(g,path = unlist(short.path$res[1])))] <- "blue"

short.path <- all_shortest_paths(g,"CyBr",mode="out",weights=NA)
all.path <- all_simple_paths(g,"CyBr","CyRs",mode = "out")

path.color[path.untangle(short.path$res,g)] <- "black"

path.color[path.untangle(all.path,g)] <- "black"

fr <- layout_with_fr(g,niter=10000)
kk <- layout_with_kk(g)
lgl <- layout_with_lgl(g)
ncl <- layout_nicely(g)
grd <- layout_on_grid(g)
dh <- layout_with_dh(g)
gem <- layout_with_gem(g)
tr <- layout_as_tree(g)

plot(g,
     vertex.color=color.space.v[color.pos.v],
     vertex.size=V(g)$ego.size+5,
     #vertex.size2=10,
     vertex.frame.color="white",
     vertex.label.color="white",
     vertex.label.cex=(V(g)$ego.size+5)/10,
     vertex.label.dist=0,
     vertex.label.family="Roboto Condensed",
     vertex.label.degree=grp_label[V(g)$group],
     vertex.shape="circle",
     edge.arrow.size=1.5,
     edge.curved=0.0,
     edge.width=5,
     edge.color=path.color,
     rescale=T,
     layout=ncl#fr#kk#ncl#lgl#grd#dh#gem#cbind(V(g)$x/4.1,V(g)$y/3)
)

probs <- path.probs(all.path,g)
scatter.smooth(probs[[1]],probs[[2]])
centr_degree(g,mode = "all")$centralization
centr_clo(g,mode = "all")$centralization
centr_eigen(g,directed=T,scale=F,normalized=T)$centralization
#eigen centralities of DAG is zero

#create a test graph
gtest <- graph_from_literal(A-+E,A-+D-+E,B-+D,c-+D,D-+G,D-+H)
E(gtest)$weight <- c(0.2,0.1,0.1,0.2,0.2,0.3,0.4)

gFang <- graph_from_literal(R49-+R5-+R46-+R10-+R2-+R43,
                            R49-+R29-+R52-+R2,
                            R27-+R37-+R2-+R55-+R43,
                            R27-+R12-+R2,
                            R27-+R51-+R52-+R55,
                            R27-+R38-+R25-+R56-+R2,
                            R24-+R37-+R41-+R55,
                            R46-+R43,
                            R38-+R31-+R51-+R2,
                            R31-+R52,
                            R25-+R52,
                            R54-+R52,
                            R54-+R22-+R43,
                            R29-+R22,
                            R22-+R55,
                            R28-+R43,
                            R26-+R55,
                            R26-+R43,
                            R36-+R55,
                            R36-+R43,
                            R53-+R43,
                            R46-+R13-+R39-+R52,
                            R22-+R43,
                            R37-+R42-+R55,
                            R42-+R43,
                            R41-+R43,
                            R39-+R12,
                            R10-+R13-+R12,
                            R6-+R39,
                            R6-+R30-+R12,
                            R6-+R35-+R12,
                            R44-+R43,
                            R35-+R52,
                            R21-+R53-+R2,
                            R53-+R55,
                            R12-+R55,
                            R47-+R12,
                            R47-+R10-+R12,
                            R45-+R10-+R44,
                            R7-+R10,
                            R7-+R12,
                            R6-+R7-+R2,
                            R35-+R2,
                            R1-+R2,
                            R44-+R2,
                            R6-+R50-+R2,
                            R33-+R17-+R50,
                            R17-+R9-+R14-+R2,
                            R9-+R2,
                            R19-+R18-+R51,
                            R18-+R53,
                            R18-+R32-+R12,
                            R32-+R2,
                            R16-+R18-+R2,
                            R18-+R48-+R3-+R43,
                            R48-+R2,
                            R20-+R40-+R2,
                            R16-+R2,
                            R4-+R2,
                            R4-+R43
                           )

#http://dx.doi.org/10.1016/j.ress.2012.04.005
reach.ratio(gFang,mode = "out")$Qratio
entropy.calculate(gFang)

save.image("D:/1drive/Onedrive/Documents/PhD/Data/Phase2data/RSur1.RData")
