#------------------------------------------------------#
#Visualizing results
#Try <- subset(Library$Record,Library$Record$Shortest==T)

#l <- ggplot(Try,aes(Entropy,Probs,color=as.factor(Jumps)))
l <- ggplot(Library$Record,aes(Entropy,Probs,color=as.factor(Jumps)))
#l is the ggplot of entropy and probability, coloring is based on number of
#causal connection involved
l <- l+geom_point(size=6,alpha=0.5)
#size and alpha of dots
l <- l+geom_rug(size=1.1,sides = "tr",alpha=0.5)
#size, side, and alpha of rug-like expression on top and right positions
l <- l+labs(x="Accumulated entropy",y="Accumulated probability",color="|n-1|")
#Labels for axes and legends
l <- l+scale_color_manual(
  values=c("navy","darkgoldenrod","darkred",
           "steelblue","springgreen4","purple4"))
#Coloring theme
l <- l+theme_bw()
#overall theme of the plot
l <- l+theme(text=element_text(size=50,family="Roboto Condensed"))
#font style
l <- l+theme(legend.text = element_text(size = 20),
             legend.position="none")
#omit legend box by putting "none" to legen position
l <- l+theme(axis.ticks.length.y = unit(-.25, "cm"),
             axis.ticks.length.x = unit(-.25, "cm"),
             axis.text.x = element_text(margin = margin(t = .6, unit = "cm")),
             axis.text.y = element_text(margin = margin(r = .6, unit = "cm")))
#axis tick marks and numbers margin set
#l <- l+theme(legend.key.size=unit(0.6,"cm"),
#        legend.direction="horizontal",
#        legend.box.background = element_rect(),
#        legend.box.margin = margin(4, 4, 4, 4))
#l <- l+guides(color = guide_legend(override.aes = list(size = 3)))
#l <- l+guides(alpha = FALSE)
#override aesthetic parameters of the legend key
l <- l+scale_y_log10(breaks = seq(-0.1, 0.8, 0.2),
                     minor_breaks = seq(0, 1, 0.05))
#change scale to log 10 and set breaks and minor breaks from, to, pace
l <- l+scale_x_continuous(breaks = seq(1, 6, 1))
#scale of the other axis
l <- l+theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))
#plot margin 
l <- l+border(size=2)+grids(axis = "xy",color="grey75")
#change theme using the ggpubr package's functions


z <- ggplot(Library$Record, aes(Probs, fill = as.factor(Jumps), color = as.factor(Jumps))) +
  geom_density(alpha = 0.4) +
  #density of probability. Have fill for color of the density areas
  scale_color_manual(values=c("navy","darkgoldenrod","darkred","steelblue","springgreen4","black"))+
  scale_fill_manual(values=c("navy","darkgoldenrod","darkred","steelblue","springgreen4","black"))
#remember that since there is only one scenario with jumps of 6 so the
#density cannot be presented
z <- z+labs(x="Probability",y="Density")
z <- z+theme_bw()
z <- z+theme(legend.position="none")
z <- z+scale_x_log10()
z <- z+theme(text=element_text(size=50,family="Roboto Condensed"))
z <- z+theme(axis.title.y = element_blank(),
             axis.text.y = element_blank(),
             axis.ticks.y = element_blank())
#having element blank for no presentation
z <- z+theme(axis.ticks.length.x = unit(-.25, "cm"),
             axis.text.x = element_text(margin = margin(t = .6, unit = "cm")))
z <- z+scale_y_continuous(breaks = seq(0, 5, 1))
z <- z+theme(plot.margin = unit(c(0.0,0.0,0,0), "cm"))
z <- z+rotate()
z <- z+border(size=2)+grids(axis = "xy",color="grey80")
#rotate to be on the right side of l



x <- ggplot(Library$Record, aes(Entropy, fill = as.factor(Jumps), color = as.factor(Jumps)))
x <- x+geom_density(alpha = 0.4)
x <- x+scale_color_manual(values=c("navy","darkgoldenrod","darkred","steelblue","springgreen4","black"))+
  scale_fill_manual(values=c("navy","darkgoldenrod","darkred","steelblue","springgreen4","black"))
x <- x+labs(x="Entropy",y="Density")
x <- x+theme_bw()
x <- x+theme(legend.position="none")
x <- x+theme(text=element_text(size=50,family="Roboto Condensed"))
x <- x+theme(plot.margin = unit(c(0.1,0.1,0,0.1), "cm"))
x <- x+theme(axis.title.x = element_blank(),
             axis.text.x = element_blank(),
             axis.ticks.x = element_blank())
x <- x+theme(axis.ticks.length.y = unit(-.25, "cm"),
             axis.text.y = element_text(margin = margin(r = .6, unit = "cm")))
x <- x+scale_x_continuous(breaks = seq(1, 6, 1))
x <- x+border(size=2)+grids(axis = "xy",color="grey80")

M1 <- ggarrange(x, NULL, NULL, NULL, NULL, NULL, l, NULL, z, 
                ncol = 3, nrow = 3,  align = "hv", legend = "none",
                widths = c(1.9,0,0.8), heights = c(0.8,-0.1,1.7),
                common.legend = T)
#arranging the plots, using a 3x3 multi-plot with the blank ones for the
#plots to be closer

CairoPNG(filename = "Multi.png", width = 1800, height = 1350,
         pointsize = 25, bg = "white")
#preparing the device for output
M1
dev.off()




#end of using device

save.image("D:/1drive/Onedrive/Documents/PhD/Data/Phase2data/RSur1.RData")


#------------------------------------------------------------#
l <- ggplot(Library$Record,aes(Probs,Std.dev,color=as.factor(Jumps)))
#l is the ggplot of entropy and probability, coloring is based on number of
#causal connection involved
l <- l+geom_point(size=8,alpha=0.5)
#size and alpha of dots
l <- l+geom_rug(size=1.1,sides = "tr",alpha=0.5)
#size, side, and alpha of rug-like expression on top and right positions
l <- l+labs(x="Accumulated probability",y="Accumulated standard deviation")
#Labels for axes and legends
l <- l+scale_color_manual(
  values=c("navy","darkgoldenrod","darkred",
           "steelblue","springgreen4","purple4"))
#Coloring theme
l <- l+theme_bw()
#overall theme of the plot
l <- l+theme(text=element_text(size=50,family="Roboto Condensed"))
#font style
l <- l+theme(legend.text = element_text(size = 20),
             legend.position="none")
#omit legend box by putting "none" to legen position
l <- l+theme(axis.ticks.length.y = unit(-.25, "cm"),
             axis.ticks.length.x = unit(-.25, "cm"),
             axis.text.x = element_text(margin = margin(t = .6, unit = "cm")),
             axis.text.y = element_text(margin = margin(r = .6, unit = "cm")))
#axis tick marks and numbers margin set
#l <- l+theme(legend.key.size=unit(0.6,"cm"),
#        legend.direction="horizontal",
#        legend.box.background = element_rect(),
#        legend.box.margin = margin(4, 4, 4, 4))
#l <- l+guides(color = guide_legend(override.aes = list(size = 3)))
#l <- l+guides(alpha = FALSE)
#override aesthetic parameters of the legend key
l <- l+scale_y_continuous(breaks = seq(0, 1, 0.1))
#change scale to log 10 and set breaks and minor breaks from, to, pace
l <- l+scale_x_continuous(breaks = seq(0, 1, 0.1))
#scale of the other axis
l <- l+theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))
#plot margin 
l <- l+border(size=2)+grids(axis = "xy",color="grey75")


z <- ggplot(Library$Record, aes(Std.dev)) +
  geom_density(fill="black",alpha = 0.4)
#density of probability. Have fill for color of the density areas
#scale_color_manual(values=c("navy","darkgoldenrod","darkred","steelblue","springgreen4","black"))+
#scale_fill_manual(values=c("navy","darkgoldenrod","darkred","steelblue","springgreen4","black"))
#remember that since there is only one scenario with jumps of 6 so the
#density cannot be presented
z <- z+labs(y="Density")
z <- z+theme_bw()
z <- z+theme(legend.position="none")
z <- z+theme(text=element_text(size=50,family="Roboto Condensed"))
z <- z+theme(axis.title.y = element_blank(),
             axis.text.y = element_blank(),
             axis.ticks.y = element_blank())
#having element blank for no presentation
z <- z+theme(axis.ticks.length.x = unit(-.25, "cm"),
             axis.text.x = element_text(margin = margin(t = .6, unit = "cm")))
#z <- z+scale_y_continuous(breaks = seq(0, 5, 1))
z <- z+theme(plot.margin = unit(c(0.0,0.0,0,0), "cm"))
z <- z+rotate()
#rotate to be on the right side of l
z <- z+border(size=2)+grids(axis = "xy",color="grey80")



x <- ggplot(Library$Record, aes(Probs))
x <- x+geom_density(fill="black",alpha = 0.4)
#x <- x+scale_color_manual(values=c("navy","darkgoldenrod","darkred","steelblue","springgreen4","black"))+
#scale_fill_manual(values=c("navy","darkgoldenrod","darkred","steelblue","springgreen4","black"))
x <- x+labs(y="Density")
x <- x+theme_bw()
x <- x+theme(legend.position="none")
x <- x+theme(text=element_text(size=50,family="Roboto Condensed"))
x <- x+theme(plot.margin = unit(c(0.1,0.1,0,0.1), "cm"))
x <- x+theme(axis.title.x = element_blank(),
             axis.text.x = element_blank(),
             axis.ticks.x = element_blank())
x <- x+theme(axis.ticks.length.y = unit(-.25, "cm"),
             axis.text.y = element_text(margin = margin(r = .6, unit = "cm")))
x <- x+scale_x_continuous(breaks = seq(0, 1, 0.1))
x <- x+border(size=2)+grids(axis = "xy",color="grey80")

M2 <- ggarrange(x, NULL, NULL, NULL, NULL, NULL, l, NULL, z, 
                ncol = 3, nrow = 3,  align = "hv", legend = "none",
                widths = c(1.9,0,0.8), heights = c(0.8,-0.1,1.7),
                common.legend = T)
#arranging the plots, using a 3x3 multi-plot with the blank ones for the
#plots to be closer
CairoPNG(filename = "Multi.png", width = 1800, height = 1350,
         pointsize = 25, bg = "white")
#preparing the device for output
M2
dev.off()

#------------------------------------------------------------#
l <- ggplot(Library$Record,aes(Std.dev,Acc.entropy,color=as.factor(Jumps)))
#l is the ggplot of entropy and probability, coloring is based on number of
#causal connection involved
l <- l+geom_point(size=8,alpha=0.5)
#size and alpha of dots
l <- l+geom_rug(size=1.1,sides = "tr",alpha=0.5)
#size, side, and alpha of rug-like expression on top and right positions
l <- l+labs(x="Accumulated standard deviation",y="Causal connection entropy")
#Labels for axes and legends
l <- l+scale_color_manual(
  values=c("navy","darkgoldenrod","darkred",
           "steelblue","springgreen4","purple4"))
#Coloring theme
l <- l+theme_bw()
#overall theme of the plot
l <- l+theme(text=element_text(size=50,family="Roboto Condensed"))
#font style
l <- l+theme(legend.text = element_text(size = 20),
             legend.position="none")
#omit legend box by putting "none" to legen position
l <- l+theme(axis.ticks.length.y = unit(-.25, "cm"),
             axis.ticks.length.x = unit(-.25, "cm"),
             axis.text.x = element_text(margin = margin(t = .6, unit = "cm")),
             axis.text.y = element_text(margin = margin(r = .6, unit = "cm")))
#axis tick marks and numbers margin set
#l <- l+theme(legend.key.size=unit(0.6,"cm"),
#        legend.direction="horizontal",
#        legend.box.background = element_rect(),
#        legend.box.margin = margin(4, 4, 4, 4))
#l <- l+guides(color = guide_legend(override.aes = list(size = 3)))
#l <- l+guides(alpha = FALSE)
#override aesthetic parameters of the legend key
l <- l+scale_y_continuous(breaks = seq(0, 1, 0.2))
#change scale to log 10 and set breaks and minor breaks from, to, pace
l <- l+scale_x_continuous(breaks = seq(0, 1, 0.1))
#scale of the other axis
l <- l+theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))
#plot margin 
l <- l+border(size=2)+grids(axis = "xy",color="grey75")


z <- ggplot(Library$Record, aes(Acc.entropy, fill = as.factor(Jumps), color = as.factor(Jumps))) +
  geom_density(alpha = 0.4) +
  #density of probability. Have fill for color of the density areas
  scale_color_manual(values=c("navy","darkgoldenrod","darkred","steelblue","springgreen4","black"))+
  scale_fill_manual(values=c("navy","darkgoldenrod","darkred","steelblue","springgreen4","black"))
#remember that since there is only one scenario with jumps of 6 so the
#density cannot be presented
z <- z+labs(y="Density")
z <- z+theme_bw()
z <- z+theme(legend.position="none")
z <- z+theme(text=element_text(size=50,family="Roboto Condensed"))
z <- z+theme(axis.title.y = element_blank(),
             axis.text.y = element_blank(),
             axis.ticks.y = element_blank())
#having element blank for no presentation
z <- z+theme(axis.ticks.length.x = unit(-.25, "cm"),
             axis.text.x = element_text(margin = margin(t = .6, unit = "cm")))
#z <- z+scale_y_continuous(breaks = seq(0, 5, 1))
z <- z+theme(plot.margin = unit(c(0.0,0.0,0,0), "cm"))
z <- z+rotate()
#rotate to be on the right side of l
z <- z+border(size=2)+grids(axis = "xy",color="grey80")



x <- ggplot(Library$Record, aes(Std.dev, fill = as.factor(Jumps), color = as.factor(Jumps)))
x <- x+geom_density(alpha = 0.4)
x <- x+scale_color_manual(values=c("navy","darkgoldenrod","darkred","steelblue","springgreen4","black"))+
  scale_fill_manual(values=c("navy","darkgoldenrod","darkred","steelblue","springgreen4","black"))
x <- x+labs(y="Density")
x <- x+theme_bw()
x <- x+theme(legend.position="none")
x <- x+theme(text=element_text(size=50,family="Roboto Condensed"))
x <- x+theme(plot.margin = unit(c(0.1,0.1,0,0.1), "cm"))
x <- x+theme(axis.title.x = element_blank(),
             axis.text.x = element_blank(),
             axis.ticks.x = element_blank())
x <- x+theme(axis.ticks.length.y = unit(-.25, "cm"),
             axis.text.y = element_text(margin = margin(r = .6, unit = "cm")))
x <- x+scale_x_continuous(breaks = seq(0, 1, 0.1))
x <- x+border(size=2)+grids(axis = "xy",color="grey80")

M3 <- ggarrange(x, NULL, NULL, NULL, NULL, NULL, l, NULL, z, 
                ncol = 3, nrow = 3,  align = "hv", legend = "none",
                widths = c(1.9,0,0.8), heights = c(0.8,-0.1,1.7),
                common.legend = T)
#arranging the plots, using a 3x3 multi-plot with the blank ones for the
#plots to be closer
CairoPNG(filename = "Multi.png", width = 1800, height = 1350,
         pointsize = 25, bg = "white")
#preparing the device for output
M3
dev.off()
#--------------------------------------------------------------#
#Presentation of degree centrality
V(g)$shape <- "circle"
#"raster""rectangle""sphere""square""vrectangle"
#"circle""crectangle""csquare""none""pie""raster"

grp_cls <- c("#000080","#B8860B","#8B0000")
#colors of the groups of edges and nodes
ColToHex(c("navy","darkgoldenrod","darkred"))
grp_cls_add <- c("#00008060","#916A0960","#8B000060")

CairoPNG(filename = "degree.png", width = 1800, height = 1350,
         pointsize = 21, bg = "white")
#preparing the device for output

plot(g,  vertex.color=grp_cls[V(g)$group],
     vertex.shapes="circle",
     vertex.size=(V(g)$centr_degree-1)*22/12+16,
     #vertex.size2=13,
     vertex.frame.color="black",
     vertex.label.color="white",
     vertex.label.cex=(V(g)$centr_degree-1)*2.1/12+1.6,
     vertex.label.dist=0,
     vertex.label.family="Roboto Condensed",
     vertex.label.degree=grp_label[V(g)$group],
     edge.arrow.size=0.0,
     edge.curved=0.0,
     edge.width=12,
     edge.color=grp_cls_add[E(g)$trigger],
     rescale=F,
     layout=cbind(V(g)$x/3.1,V(g)$y/3.38)
)

dev.off()


#--------------------------------------------------------------#
#Presentation of prevention barrier centrality

V(g)$shape <- "circle"
#"raster""rectangle""sphere""square""vrectangle"
#"circle""crectangle""csquare""none""pie""raster"

grp_cls <- c("#000080","#B8860B","#8B0000")
#colors of the groups of edges and nodes
ColToHex(c("navy","darkgoldenrod","darkred"))
grp_cls_add <- c("#00008060","#916A0960","#8B000060")

CairoPNG(filename = "prevention.png", width = 1800, height = 1350,
         pointsize = 21, bg = "white")
#preparing the device for output

plot(g,  vertex.color=grp_cls[V(g)$group],
     vertex.shapes="circle",
     vertex.size=(V(g)$prevention-6)*23/39+16,
     #vertex.size2=13,
     vertex.frame.color="black",
     vertex.label.color="white",
     vertex.label.cex=(V(g)$prevention-6)*2.1/39+1.6,
     vertex.label.dist=0,
     vertex.label.family="Roboto Condensed",
     vertex.label.degree=grp_label[V(g)$group],
     edge.arrow.size=0.0,
     edge.curved=0.0,
     edge.width=12,
     edge.color=grp_cls_add[E(g)$trigger],
     rescale=F,
     layout=cbind(V(g)$x/3.1,V(g)$y/3.57)
)

dev.off()

#--------------------------------------------------------------#
#Mapping the non-probabilistic indexes - dot plot

Non.prob <- data.frame(V(g)$prevention,V(g)$centr_degree)
colnames(Non.prob) <- c("prevention","degree")

l <- ggplot(Non.prob,aes(prevention,degree,color=as.factor(V(g)$group)))
#l is the ggplot of entropy and probability, coloring is based on number of
#causal connection involved
l <- l+geom_point(size=25,alpha=0.7)
#size and alpha of dots
l <- l+labs(x="Prevention barrier centrality (B)",
            y="Degree centrality (???)")
#Labels for axes and legends
l <- l+scale_color_manual(
  values=c("navy","darkgoldenrod","darkred"))
#Coloring theme
l <- l+theme_bw()
#overall theme of the plot
l <- l+theme(text=element_text(size=100,family="Roboto Condensed"))
#font style
l <- l+theme(legend.text = element_text(size = 30),
             legend.position="none")
#omit legend box by putting "none" to legen position
l <- l+theme(axis.ticks.length.y = unit(-.25, "cm"),
             axis.ticks.length.x = unit(-.25, "cm"),
             axis.text.x = element_text(margin = margin(t = .6, unit = "cm")),
             axis.text.y = element_text(margin = margin(r = .6, unit = "cm")))
#axis tick marks and numbers margin set
#l <- l+theme(legend.key.size=unit(0.6,"cm"),
#        legend.direction="horizontal",
#        legend.box.background = element_rect(),
#        legend.box.margin = margin(4, 4, 4, 4))
#l <- l+guides(color = guide_legend(override.aes = list(size = 3)))
#l <- l+guides(alpha = FALSE)
#override aesthetic parameters of the legend key
l <- l+scale_y_sqrt(breaks = seq(0, 12, 2))
#change scale to log 10 and set breaks and minor breaks from, to, pace
l <- l+scale_x_sqrt(breaks = seq(5, 40, 5))
#scale of the other axis
l <- l+theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))
#plot margin 
l <- l+border(size=2)+grids(axis = "xy",color="grey50")

CairoPNG(filename = "nonweight.png", width = 2200, height = 1350,
         pointsize = 27, bg = "white")
#preparing the device for output
l
dev.off()

#--------------------------------------------------------------#
#Presentation of reaching centrality (closeness centrality) and
#betweeness centrality

V(g)$shape <- "circle"
#"raster""rectangle""sphere""square""vrectangle"
#"circle""crectangle""csquare""none""pie""raster"

grp_cls <- c("#000080","#B8860B","#8B0000")
#colors of the groups of edges and nodes
grp_cls_add <- c("#00008060","#916A0960","#8B000060")

#preparing data vector for plot


Vec.vs <- rep(0,25)
#plot close vertex size
plot.close.vs <- size.mapping(V(g)$reaching.likely,16,32)
Vec.vs[1:7] <- plot.close.vs[1:7]
Vec.vs[18:25] <- plot.close.vs[18:25]
plot.bet.vs <- size.mapping(V(g)$bet.likely,16,32)
Vec.vs[8:17] <- plot.bet.vs[8:17]


Vec.ls <- rep(0,25)
#plot close label size
plot.close.ls <- size.mapping(V(g)$reaching.likely,1.6,3.1)
Vec.ls[1:7] <- plot.close.ls[1:7]
Vec.ls[18:25] <- plot.close.ls[18:25]
plot.bet.ls <- size.mapping(V(g)$bet.likely,1.6,3.1)
Vec.ls[8:17] <- plot.bet.ls[8:17]



CairoPNG(filename = "reaching.png", width = 1800, height = 1350,
         pointsize = 21, bg = "white")
#preparing the device for output

plot(g,  vertex.color=grp_cls[V(g)$group],
     vertex.shapes="circle",
     vertex.size=Vec.vs,
     #vertex.size2=13,
     vertex.frame.color="black",
     vertex.label.color="white",
     vertex.label.cex=Vec.ls,
     vertex.label.dist=0,
     vertex.label.family="Roboto Condensed",
     vertex.label.degree=grp_label[V(g)$group],
     edge.arrow.size=0.0,
     edge.curved=0.0,
     edge.width=12,
     edge.color=grp_cls_add[E(g)$trigger],
     rescale=F,
     layout=cbind(V(g)$x/3.1,V(g)$y/3.57)
)

dev.off()

Data1 <- rep(0,25)
Data1[1:7] <- V(g)$close[1:7]
Data1[18:25] <- V(g)$close[18:25]
Data1[8:17] <- V(g)$centr_trig[8:17]

Data2 <- rep(0,25)
Data2[1:7] <- V(g)$close_short[1:7]
Data2[18:25] <- V(g)$close_short[18:25]
Data2[8:17] <- V(g)$centr_trig_short[8:17]

Data <- data.frame(V(g)$name,(Data1-Data2)/Data1*100,Data1,Data2)
colnames(Data) <- c("Vertex","Decrease","Standard","Likely")
#write.xlsx(Data, 'name-of-your-excel-file.xlsx')

l <- ggplot(Data,aes(Data1,Data2,color=as.factor(V(g)$group),label=V(g)$name))
#l is the ggplot of combined data, and likely data, coloring is based on number of
#causal connection involved
l <- l+geom_point(size=19,alpha=0.7,position = "jitter")
#size and alpha of dots
l <- l+labs(x="Accumulated probability (S and W)",y="Likely variants (SL and WL)")
#Labels for axes and legends
l <- l+scale_color_manual(
  values=c("navy","darkgoldenrod","darkred"))
#Coloring theme
l <- l+theme_bw()
#overall theme of the plot
l <- l+theme(text=element_text(size=70,family="Roboto Condensed"))
#font style
l <- l+theme(legend.text = element_text(size = 30),
             legend.position="none")
#omit legend box by putting "none" to legen position
l <- l+theme(axis.ticks.length.y = unit(-.25, "cm"),
             axis.ticks.length.x = unit(-.25, "cm"),
             axis.text.x = element_text(margin = margin(t = .6, unit = "cm")),
             axis.text.y = element_text(margin = margin(r = .6, unit = "cm")))
#axis tick marks and numbers margin set
#l <- l+theme(legend.key.size=unit(0.6,"cm"),
#        legend.direction="horizontal",
#        legend.box.background = element_rect(),
#        legend.box.margin = margin(4, 4, 4, 4))
#l <- l+guides(color = guide_legend(override.aes = list(size = 3)))
#l <- l+guides(alpha = FALSE)
#override aesthetic parameters of the legend key
l <- l+scale_y_log10(breaks = seq(0, 12, 2))
#change scale to log 10 and set breaks and minor breaks from, to, pace
l <- l+scale_x_log10(breaks = seq(0, 24, 5))
#scale of the other axis
l <- l+theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))
#plot margin 
l <- l+border(size=2)+grids(axis = "xy",color="grey75")
l <- l+geom_abline(size=3,alpha=0.4,color="grey50")

l <- l+geom_text(size = 10)

CairoPNG(filename = "reaching.png", width = 1200, height = 1200,
         pointsize = 21, bg = "white")
l
dev.off()

#--------------------------------------------------------------#
#dot plot for the betweeness centrality and the short version
Data1 <- round(E(g)$centr_weight,digits=3)
Data2 <- round(E(g)$centr_weight_short,digits=3)
Data <- data.frame(E(g)$name,(Data1-Data2)/Data1*100,Data1,Data2)
colnames(Data) <- c("Edge","Decrease","Standard","Likely")

l <- ggplot(Data,aes(Data1,Data2,color=as.factor(E(g)$trigger),label=E(g)$name))
#l is the ggplot of 
l <- l+geom_point(size=13,alpha=0.7,position = "jitter")
#size and alpha of dots
l <- l+labs(x="Betweenness centrality (W)",y="Likely variant (WL)")
#Labels for axes and legends
l <- l+scale_color_manual(
  values=c("navy","darkgoldenrod","darkred"))
#Coloring theme
l <- l+theme_bw()
#overall theme of the plot
l <- l+theme(text=element_text(size=70,family="Roboto Condensed"))
#font style
l <- l+theme(legend.text = element_text(size = 30),
             legend.position="none")
#omit legend box by putting "none" to legen position
l <- l+theme(axis.ticks.length.y = unit(-.25, "cm"),
             axis.ticks.length.x = unit(-.25, "cm"),
             axis.text.x = element_text(margin = margin(t = .6, unit = "cm")),
             axis.text.y = element_text(margin = margin(r = .6, unit = "cm")))
#axis tick marks and numbers margin set
#l <- l+theme(legend.key.size=unit(0.6,"cm"),
#        legend.direction="horizontal",
#        legend.box.background = element_rect(),
#        legend.box.margin = margin(4, 4, 4, 4))
#l <- l+guides(color = guide_legend(override.aes = list(size = 3)))
#l <- l+guides(alpha = FALSE)
#override aesthetic parameters of the legend key
l <- l+scale_y_continuous(breaks = seq(0, 7, 1))
#change scale to log 10 and set breaks and minor breaks from, to, pace
l <- l+scale_x_continuous(breaks = seq(0, 8, 1))
#scale of the other axis
l <- l+theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))
#plot margin 
l <- l+border(size=2)+grids(axis = "xy",color="grey75")
l <- l+geom_abline(size=3,alpha=0.4,color="grey50")
l <- l+coord_cartesian(
  xlim = c(0.5, 4), ylim = c(0.5, 4))
l <- l+geom_text(size = 10)

CairoPNG(filename = "reaching.png", width = 1200, height = 1200,
         pointsize = 21, bg = "white")
l
dev.off()

#------------------------------------------------------------#
#standard deviation and probability for alpha and thickness of the edges
#short combination for the size of the node

Vec.vs <- rep(0,25)
#plot close vertex size
plot.close.vs <- size.mapping(V(g)$close_short,15,25)
Vec.vs[1:7] <- plot.close.vs[1:7]
Vec.vs[18:25] <- plot.close.vs[18:25]
plot.bet.vs <- size.mapping(V(g)$centr_trig_short,15,25)
Vec.vs[8:17] <- plot.bet.vs[8:17]


Vec.ls <- rep(0,25)
#plot close label size
plot.close.ls <- size.mapping(V(g)$close_short,1.6,2.8)
Vec.ls[1:7] <- plot.close.ls[1:7]
Vec.ls[18:25] <- plot.close.ls[18:25]
plot.bet.ls <- size.mapping(V(g)$centr_trig_short,1.6,2.8)
Vec.ls[8:17] <- plot.bet.ls[8:17]

Data1 <- rep(0,25)
Data1[1:7] <- V(g)$close[1:7]
Data1[18:25] <- V(g)$close[18:25]
Data1[8:17] <- V(g)$centr_trig[8:17]

Data2 <- rep(0,25)
Data2[1:7] <- V(g)$close_short[1:7]
Data2[18:25] <- V(g)$close_short[18:25]
Data2[8:17] <- V(g)$centr_trig_short[8:17]

Data <- data.frame(V(g)$name,(Data1-Data2)/Data1*100,Data1,Data2)
colnames(Data) <- c("Vertex","Decrease","Standard","Likely")

grp_cls <- c("#00006C","#8D6807","#600000")

CairoWin()

CairoPNG(filename = "reaching.png", width = 1800, height = 1350,
         pointsize = 21, bg = "white")
#preparing the device for output

e.width <- size.mapping(E(g)$weight^1.5,10,25)
e.alpha <- round(size.mapping(max(E(g)$std)-E(g)$std,30,240),0)
pallete <- colorRampPalette(brewer.pal(9,"YlOrRd"))
pallete <- pallete(1000)
e.color.pos <- round(size.mapping(E(g)$weight^1.5,50,950))
e.color <- paste(pallete[e.color.pos],as.hexmode(e.alpha),sep="")


plot(g,  vertex.color="grey90",
     vertex.shapes="circle",
     vertex.size=Vec.vs,
     #vertex.size2=13,
     vertex.frame.color=grp_cls[V(g)$group],
     vertex.label.color=grp_cls[V(g)$group],
     vertex.label.cex=Vec.ls,
     vertex.label.dist=0,
     vertex.label.family="Roboto Condensed",
     edge.arrow.size=0,
     edge.curved=0.0,
     edge.width=20,
     edge.color=e.color,
     rescale=F,
     layout=cbind(V(g)$x/3.1,V(g)$y/3.57)
)

dev.off()

#------------------------------------------------------------#
#probability and betweeness centrality for alpha and thickness of the edges
#short combination for the size of the node

Vec.vs <- rep(0,25)
#plot close vertex size
plot.close.vs <- size.mapping(V(g)$close_short,15,25)
Vec.vs[1:7] <- plot.close.vs[1:7]
Vec.vs[18:25] <- plot.close.vs[18:25]
plot.bet.vs <- size.mapping(V(g)$centr_trig_short,15,25)
Vec.vs[8:17] <- plot.bet.vs[8:17]


Vec.ls <- rep(0,25)
#plot close label size
plot.close.ls <- size.mapping(V(g)$close_short,1.6,2.8)
Vec.ls[1:7] <- plot.close.ls[1:7]
Vec.ls[18:25] <- plot.close.ls[18:25]
plot.bet.ls <- size.mapping(V(g)$centr_trig_short,1.6,2.8)
Vec.ls[8:17] <- plot.bet.ls[8:17]

Data1 <- rep(0,25)
Data1[1:7] <- V(g)$close[1:7]
Data1[18:25] <- V(g)$close[18:25]
Data1[8:17] <- V(g)$centr_trig[8:17]

Data2 <- rep(0,25)
Data2[1:7] <- V(g)$close_short[1:7]
Data2[18:25] <- V(g)$close_short[18:25]
Data2[8:17] <- V(g)$centr_trig_short[8:17]

Data <- data.frame(V(g)$name,(Data1-Data2)/Data1*100,Data1,Data2)
colnames(Data) <- c("Vertex","Decrease","Standard","Likely")


grp_cls <- c("#00006C","#8D6807","#600000")

CairoPNG(filename = "reaching.png", width = 1800, height = 1350,
         pointsize = 21, bg = "white")
#preparing the device for output

e.width <- size.mapping(E(g)$centr_weight_short+8,10,32)
e.alpha <- round(size.mapping(E(g)$weight,30,220),0)
e.color <- paste(grp_cls[E(g)$trigger],as.hexmode(e.alpha),sep="")


plot(g,  vertex.color="grey90",
     vertex.shapes="circle",
     vertex.size=Vec.vs,
     #vertex.size2=13,
     vertex.frame.color=grp_cls[V(g)$group],
     vertex.label.color=grp_cls[V(g)$group],
     vertex.label.cex=Vec.ls,
     vertex.label.dist=0,
     vertex.label.family="Roboto Condensed",
     edge.arrow.size=0,
     edge.curved=0.0,
     edge.width=e.width,
     edge.color=e.color,
     rescale=F,
     layout=cbind(V(g)$x/3.1,V(g)$y/3.57)
)

dev.off()

#--------------------------------------------------------------#
#dot plot of uncertainty for std and entropy
Data2 <- round((E(g)$entro_contri),digits=3)
Data1 <- round(E(g)$std,digits=3)
Data <- data.frame(E(g)$name,Data1,Data2)
colnames(Data) <- c("Edge","Outcome","Evidential")

l <- ggplot(Data,aes(Data1,Data2,color=as.factor(E(g)$trigger)))
#,label=E(g)$name
#l is the ggplot of 
l <- l+geom_point(size=11,alpha=0.7,position = "jitter")
#size and alpha of dots
l <- l+geom_rug(size=2,sides = "tr",alpha=0.5)

l <- l+labs(x="Standard deviation (  )",y="Causal connection entropy (H)")
#Labels for axes and legends
l <- l+scale_color_manual(
  values=c("navy","darkgoldenrod","darkred"))
#Coloring theme
l <- l+theme_bw()
#overall theme of the plot
l <- l+theme(text=element_text(size=60,family="Roboto Condensed"))
#font style
l <- l+theme(legend.text = element_text(size = 30),
             legend.position="none")
#omit legend box by putting "none" to legen position
l <- l+theme(axis.ticks.length.y = unit(-.25, "cm"),
             axis.ticks.length.x = unit(-.25, "cm"),
             axis.text.x = element_text(margin = margin(t = .6, unit = "cm")),
             axis.text.y = element_text(margin = margin(r = .6, unit = "cm")))
#axis tick marks and numbers margin set
#l <- l+theme(legend.key.size=unit(0.6,"cm"),
#        legend.direction="horizontal",
#        legend.box.background = element_rect(),
#        legend.box.margin = margin(4, 4, 4, 4))
#l <- l+guides(color = guide_legend(override.aes = list(size = 3)))
#l <- l+guides(alpha = FALSE)
#override aesthetic parameters of the legend key
l <- l+scale_x_continuous(breaks = seq(0.1, 1, 0.05))
#change scale to log 10 and set breaks and minor breaks from, to, pace
l <- l+scale_y_continuous(breaks = seq(0.1, 1, 0.05))
#scale of the other axis
l <- l+theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))
#plot margin 
l <- l+border(size=2)+grids(axis = "xy",color="grey75")
#l <- l+geom_abline(size=3,alpha=0.4,color="grey50")
#l <- l+coord_cartesian(ylim = c(0.97, max(Data2)+0.005))
#l <- l+geom_text(size = 10)

CairoPNG(filename = "reaching.png", width = 1000, height = 1000,
         pointsize = 21, bg = "white")
l
dev.off()

#------------------------------------------------------------#
#correlation diagram for edges' indexes
Data.e <- data.frame(E(g)$name,E(g)$trigger,E(g)$bet_likely,E(g)$weight,
                     E(g)$std,E(g)$entropy,E(g)$bet.avg.likely,E(g)$entro_contri)
colnames(Data.e) <- c("Edge","Group","WL","Prob","Std","Entro","WL average","Struc")
Data.multi <- cor(Data.e[3:8])
CairoPNG(filename = "reaching.png", width = 1000, height = 1000,
         pointsize = 10, bg = "white")

corrplot(Data.multi, method = "color", type = "upper",
         addCoef.col = "black",diag=F,tl.col="#FFFFFF00",
         tl.srt=0,mar = c(0.5, 0.5, 0.5, 0.5), addgrid=T, col=col(200)
)
dev.off()

write.xlsx(Data.e, 'Data.e.xlsx')
#---------------------------------------------------------------#
#correlation diagram for multi-event scenarios
Data <- Library$Record[3:7]
colnames(Data) <- c("|E|","p","K","H","Z")
Data.multi <- cor(Data)
col <- colorRampPalette(c("#000080", "#3737FF", "#FFFFFF", "#FF0202", "#8B0000"))

CairoPNG(filename = "reaching.png", width = 1000, height = 1000,
         pointsize = 30, bg = "white")

corrplot(Data.multi, method = "color", type = "upper",
         addCoef.col = "white",diag=F,tl.col="black",
         tl.srt=0,mar = c(0, 0, 0, 0), addgrid=T, col=col(200)
)
dev.off()

write.xlsx(Library$Record, 'Record.xlsx')

#---------------------------------------------------------------#
#correlation between average S+W and average SL+Wl

Data1 <- rep(0,25)
Data1[1:7] <- V(g)$reaching_likely[1:7]
Data1[18:25] <- V(g)$reaching_likely[18:25]
Data1[8:17] <- V(g)$bet_likely[8:17]

Data2 <- rep(0,25)
Data2[1:7] <- V(g)$reaching.avg.likely[1:7]
Data2[18:25] <- V(g)$reaching.avg.likely[18:25]
Data2[8:17] <- V(g)$bet.avg.likely[8:17]

Data <- data.frame(V(g)$name,Data1,Data2)

l <- ggplot(Data,aes(Data2,Data1,color=as.factor(V(g)$group),label=V(g)$name))
#l is the ggplot of combined data, and likely data, coloring is based on number of
#causal connection involved
l <- l+geom_point(size=19,alpha=0.7,position = "jitter")
#size and alpha of dots
l <- l+labs(x="Average likelihood",y="Likely centrality (SL and WL)")
#Labels for axes and legends
l <- l+scale_color_manual(
  values=c("navy","darkgoldenrod","darkred"))
#Coloring theme
l <- l+theme_bw()
#overall theme of the plot
l <- l+theme(text=element_text(size=70,family="Roboto Condensed"))
#font style
l <- l+theme(legend.text = element_text(size = 30),
             legend.position="none")
#omit legend box by putting "none" to legen position
l <- l+theme(axis.ticks.length.y = unit(-.25, "cm"),
             axis.ticks.length.x = unit(-.25, "cm"),
             axis.text.x = element_text(margin = margin(t = .6, unit = "cm")),
             axis.text.y = element_text(margin = margin(r = .6, unit = "cm")))
#axis tick marks and numbers margin set
#l <- l+theme(legend.key.size=unit(0.6,"cm"),
#        legend.direction="horizontal",
#        legend.box.background = element_rect(),
#        legend.box.margin = margin(4, 4, 4, 4))
#l <- l+guides(color = guide_legend(override.aes = list(size = 3)))
#l <- l+guides(alpha = FALSE)
#override aesthetic parameters of the legend key
l <- l+scale_x_continuous(breaks = seq(0, 1, 0.05))
#change scale to log 10 and set breaks and minor breaks from, to, pace
l <- l+scale_y_continuous(breaks = seq(0, 20, 1))
#scale of the other axis
l <- l+theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))
#plot margin 
l <- l+border(size=2)+grids(axis = "xy",color="grey75")
l <- l+geom_abline(size=3,alpha=0.4,color="grey50")
l <- l+coord_cartesian(ylim = c(min(Data1), 6.3))
l <- l+geom_text(size = 10)

CairoPNG(filename = "okay.png", width = 1200, height = 1200,
         pointsize = 21, bg = "white")
l
dev.off()

#---------------------------------------------------------------#
#risk network using average data
#Presentation of degree centrality
V(g)$shape <- "circle"
#"raster""rectangle""sphere""square""vrectangle"
#"circle""crectangle""csquare""none""pie""raster"

grp_cls <- c("#000080","#B8860B","#8B0000")
#colors of the groups of edges and nodes

v.size <- size.mapping(Data2,17,27)
v.label <- size.mapping(Data2,1.6,2.7)
pallete <- colorRampPalette(brewer.pal(9,"YlOrRd"))
pallete <- pallete(1000)
e.color.pos <- round(size.mapping(E(g)$weight^1.5,50,950))
e.color <- paste(pallete[e.color.pos],"99",sep="")

CairoPNG(filename = "degree.png", width = 1800, height = 1350,
         pointsize = 21, bg = "white")
#preparing the device for output

plot(g,  vertex.color=grp_cls[V(g)$group],
     vertex.shapes="circle",
     vertex.size=v.size,
     #vertex.size2=13,
     vertex.frame.color="black",
     vertex.label.color="white",
     vertex.label.cex=v.label,
     vertex.label.dist=0,
     vertex.label.family="Roboto Condensed",
     vertex.label.degree=grp_label[V(g)$group],
     edge.arrow.size=0.0,
     edge.curved=0.0,
     edge.width=20,
     edge.color=e.color,
     rescale=F,
     layout=cbind(V(g)$x/3.1,V(g)$y/3.42)
)

dev.off()

#------------------------------------------------------------#
#average likelihood and probability for color and thickness of the edges
#likely average combination for the size of the vertex

#Data2 for likely average of vertices
Data2 <- rep(0,25)
Data2[1:7] <- V(g)$reaching.avg.likely[1:7]
Data2[18:25] <- V(g)$reaching.avg.likely[18:25]
Data2[8:17] <- V(g)$bet.avg.likely[8:17]

Vec.vs <- size.mapping(Data2,17,27)
#plot close vertex size


Vec.ls <- size.mapping(Data2,1.6,2.8)
#plot close label size

pallete <- colorRampPalette(brewer.pal(9,"YlOrRd"))
pallete <- pallete(1000)

e.width <- size.mapping(E(g)$weight,10,32)
e.color.pos <- round(size.mapping(E(g)$bet.avg.likely^0.6,50,950))
e.color <- paste(pallete[e.color.pos],"99",sep="")
grp_cls <- c("#00006C","#8D6807","#600000")

CairoPNG(filename = "okay.png", width = 1800, height = 1350,
         pointsize = 21, bg = "white")
#preparing the device for output


plot(g,  vertex.color="grey90",
     vertex.shapes="circle",
     vertex.size=Vec.vs,
     #vertex.size2=13,
     vertex.frame.color=grp_cls[V(g)$group],
     vertex.label.color=grp_cls[V(g)$group],
     vertex.label.cex=Vec.ls,
     vertex.label.dist=0,
     vertex.label.family="Roboto Condensed",
     edge.arrow.size=0,
     edge.curved=0.0,
     edge.width=e.width,
     edge.color=e.color,
     rescale=F,
     layout=cbind(V(g)$x/3.1,V(g)$y/3.57)
)

dev.off()

#--------------------------------------------------------------#
#---------------------------------------------------------------#
#correlation between average S+W and average SL+Wl

Data1 <- E(g)$weight

Data2 <- E(g)$bet.avg.likely

Data <- data.frame(E(g)$name,Data1,Data2)

l <- ggplot(Data,aes(Data2,Data1,color=as.factor(E(g)$trigger),label=E(g)$name))
#l is the ggplot of combined data, and likely data, coloring is based on number of
#causal connection involved
l <- l+geom_point(size=10,alpha=0.7,position = "jitter")
#size and alpha of dots
l <- l+labs(x="Average betweenness likelihood (    )",y="Causal connection probability (p)")
#Labels for axes and legends
l <- l+scale_color_manual(
  values=c("navy","darkgoldenrod","darkred"))
#Coloring theme
l <- l+theme_bw()
#overall theme of the plot
l <- l+theme(text=element_text(size=70,family="Roboto Condensed"))
#font style
l <- l+theme(legend.text = element_text(size = 30),
             legend.position="none")
#omit legend box by putting "none" to legen position
l <- l+theme(axis.ticks.length.y = unit(-.25, "cm"),
             axis.ticks.length.x = unit(-.25, "cm"),
             axis.text.x = element_text(margin = margin(t = .6, unit = "cm")),
             axis.text.y = element_text(margin = margin(r = .6, unit = "cm")))
#axis tick marks and numbers margin set
#l <- l+theme(legend.key.size=unit(0.6,"cm"),
#        legend.direction="horizontal",
#        legend.box.background = element_rect(),
#        legend.box.margin = margin(4, 4, 4, 4))
#l <- l+guides(color = guide_legend(override.aes = list(size = 3)))
#l <- l+guides(alpha = FALSE)
#override aesthetic parameters of the legend key
l <- l+scale_x_continuous(breaks = seq(0, 1, 0.1))
#change scale to log 10 and set breaks and minor breaks from, to, pace
l <- l+scale_y_continuous(breaks = seq(0, 1, 0.1))
#scale of the other axis
l <- l+theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))
#plot margin 
l <- l+border(size=2)+grids(axis = "xy",color="grey75")
#l <- l+geom_abline(size=3,alpha=0.4,color="grey50")
#l <- l+coord_cartesian(ylim = c(min(Data1), 6.3))
#l <- l+geom_text(size = 10)

CairoPNG(filename = "reaching.png", width = 1200, height = 1200,
         pointsize = 21, bg = "white")
l
dev.off()

#---------------------------------------------------------------#
