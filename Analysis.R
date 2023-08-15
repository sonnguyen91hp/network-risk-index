

Reach.out <- reach.ratio(g,mode = "out")
Reach.in <- reach.ratio(g,mode = "in")

sum(Reach.out$individual[,2])
sum(Reach.in$individual[,2])
#the fact that these two sums are equal proves that there are 367 scenarios
#from this network

#------------------------------------------------------#
#Result for nodes: This part records the calculation of analysis
#result for nodes, which is important for Methodology and Result 
#analysis sections

#1. Degree centrality: In, out, and total centrality degree

V(g)$centr.degree.in <- centr_degree(g,mode = "in")$res
V(g)$centr.degree.out <- centr_degree(g,mode = "out")$res
V(g)$centr.degree <- centr_degree(g,mode = "all")$res

#2. Reachability centrality: In, out, and total Reachability
#degree

Reach.ratio <- reach.ratio(g,mode = "out")$Qratio
V(g)$reach.in <- reach.ratio(g,mode = "in")$individual$count.indiv
V(g)$reach.out <- reach.ratio(g,mode = "out")$individual$count.indiv
V(g)$reach <- V(g)$reach.in + V(g)$reach.out

#3. Betweenness centrality: For vertices. This has been named "Trigger
#centrality" fuction. This is for risks in Group 2
#This has been named "Betweenness centrality" in the paper

V(g)$bet <- trigger.centrality(g,mode="vertex")$Centrality
Data.bet.likely <- betweenness.centrality(g,mode="vertex")
V(g)$bet.likely <- Data.bet.likely$Centrality

#4. Closeness centrality: For vertices. The function is named "Trigger
#index". This is for (1) prove that Group 2 is primarily the intermediate
#(2) prove that Group 1 and Group 3 should be analyzed using a different
#measurement
#This has been named "Reaching centrality" in the paper


V(g)$reaching <- trigger.index(g)$Close.trigg.out+trigger.index(g)$Close.trigg.in

Data.reaching.likely <- reaching.centrality(g)
V(g)$reaching.likely <- Data.reaching.likely$Reach.out + 
  Data.reaching.likely$Reach.in


#5. Prevention difficulty: The function is named prevent.diff
#

V(g)$prevention <- prevent.diff(g)$Summary.to$sum.to+
  prevent.diff(g)$Summary.from$sum.from

#6. Average likelihood for reaching centrality: divide the reaching and betweeness centrality
#to the number of scenarios included
V(g)$reaching.avg <- V(g)$reaching/V(g)$reach
V(g)$reaching.avg.likely <- V(g)$reaching.likely/Data.reaching.likely$Likely

#7. Average likelihood for betweeness centrality:
V(g)$bet.avg <- V(g)$bet/trigger.centrality(g,mode="vertex")$Central.no.weight
V(g)$bet.avg.likely <- V(g)$bet.likely/Data.bet.likely$Likely
#------------------------------------------------------#
#Result for edges: This part records the calculation of analysis
#result for edges, which is important for Methodology and Result 
#analysis sections

#1. Betweeness centrality of edges: Using trigger.centrality for edges

E(g)$bet <- trigger.centrality(g,"edge")$Centrality
E(g)$bet.avg <- E(g)$bet/trigger.centrality(g,"edge")$Central.no.weight
Data.bet.likely.e <- betweenness.centrality(g,mode="edge")
E(g)$bet.likely <- Data.bet.likely.e$Centrality
E(g)$bet.avg.likely <- E(g)$bet.likely/Data.bet.likely.e$Likely

#2. Magnitude of edges: stored in E(g)$weight
#3. Uncertainty of edges: stored in E(g)$std. This index can be used to
#reflect the uncertainty of the network

#4. Entropy of edges: stored in E(g)$entropy using the function
#entropy.calculate(g)

#5. Entropy contribution: stored in E(g)$entro_contri using the function
E(g)$entro.contri <- entropy.structure(g)

#6. The outcome uncertainty index for linearity instead of entropy
E(g)$uncer <- round(1-(2*abs(E(g)$weight-0.5)),digits=3)

#6. The outcome uncertainty index using entropy has  been calculated, but
#they are mostly close to 1

#------------------------------------------------------#
#Overall network: This part records the calculation of analysis of 
#indexes for the whole network

Group1.count <- length(V(g)[V(g)$group==1])
Group2.count <- length(V(g)[V(g)$group==2])
Group3.count <- length(V(g)[V(g)$group==3])

Rho.g <- length(E(g))/(length(V(g))*(length(V(g))-1))
Rhos.g <- length(E(g))/((Group1.count+Group2.count)*
                          (Group2.count+Group3.count)-Group2.count)

#The Ivd of the whole network, using the formula of Bonchev and Buck
entropy.calculate(g)