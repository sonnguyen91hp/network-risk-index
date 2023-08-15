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
library(dplyr)
library(corrplot)
library(writexl)

#For Reaching (S)
Reaching <- data.frame(V(g)$name,V(g)$reaching)
colnames(Reaching) <- c("Name","Reaching_S")
Reaching <- arrange(Reaching,desc(Reaching_S))

#For Reaching average (S.avg)
Reaching.avg <- data.frame(V(g)$name,V(g)$reaching.avg)
colnames(Reaching.avg) <- c("Name","ReachingA_SA")
Reaching.avg <- arrange(Reaching.avg,desc(ReachingA_SA))

#For Likely Reaching (SL)
ReachingL <- data.frame(V(g)$name,V(g)$reaching.likely)
colnames(ReachingL) <- c("Name","ReachingL_SL")
ReachingL <- arrange(ReachingL,desc(ReachingL_SL))

#For Likely Reaching average (SL.avg)
ReachingL.avg <- data.frame(V(g)$name,V(g)$reaching.avg.likely)
colnames(ReachingL.avg) <- c("Name","ReachingLA_SLA")
ReachingL.avg <- arrange(ReachingL.avg,desc(ReachingLA_SLA))

#------------------------------------------------------------#

#For Between (W)
Between <- data.frame(V(g)$name,V(g)$bet)
colnames(Between) <- c("Name","Between_W")
Between <- arrange(Between,desc(Between_W))

#For Between average (W.avg)
Between.avg <- data.frame(V(g)$name,V(g)$bet.avg)
colnames(Between.avg) <- c("Name","BetweenA_WA")
Between.avg <- arrange(Between.avg,desc(BetweenA_WA))

#For Likely Between (WL)
BetweenL <- data.frame(V(g)$name,V(g)$bet.likely)
colnames(BetweenL) <- c("Name","BetweenL_WL")
BetweenL <- arrange(BetweenL,desc(BetweenL_WL))

#For Likely Between average (WL.avg)
BetweenL.avg <- data.frame(V(g)$name,V(g)$bet.avg.likely)
colnames(BetweenL.avg) <- c("Name","BetweenLA_WLA")
BetweenL.avg <- arrange(BetweenL.avg,desc(BetweenLA_WLA))

#-------------------------------------------------------------#
#With no sorting and writexl

Direct <- "D:\\1drive\\Onedrive\\Documents\\PhD\\Data\\Phase2data\\RNA\\"

#For Reaching (S)
Reaching <- data.frame(V(g)$name,V(g)$reaching)
colnames(Reaching) <- c("Name","Reaching_S")
Directory <- paste(Direct,"Reaching.xlsx", sep="")
write_xlsx(Reaching,Directory)

#For Reaching average (S.avg)
Reaching.avg <- data.frame(V(g)$name,V(g)$reaching.avg)
colnames(Reaching.avg) <- c("Name","ReachingA_SA")
Directory <- paste(Direct,"Reaching.avg.xlsx", sep="")
write_xlsx(Reaching.avg,Directory)

#For Likely Reaching (SL)
ReachingL <- data.frame(V(g)$name,V(g)$reaching.likely)
colnames(ReachingL) <- c("Name","ReachingL_SL")
Directory <- paste(Direct,"ReachingL.xlsx", sep="")
write_xlsx(ReachingL,Directory)

#For Likely Reaching average (SL.avg)
ReachingL.avg <- data.frame(V(g)$name,V(g)$reaching.avg.likely)
colnames(ReachingL.avg) <- c("Name","ReachingLA_SLA")
Directory <- paste(Direct,"ReachingL.avg.xlsx", sep="")
write_xlsx(ReachingL.avg,Directory)

#------------------------------------------------------------#

#For Between (W)
Between <- data.frame(V(g)$name,V(g)$bet)
colnames(Between) <- c("Name","Between_W")
Directory <- paste(Direct,"Between.xlsx", sep="")
write_xlsx(Between,Directory)

#For Between average (W.avg)
Between.avg <- data.frame(V(g)$name,V(g)$bet.avg)
colnames(Between.avg) <- c("Name","BetweenA_WA")
Directory <- paste(Direct,"Between.avg.xlsx", sep="")
write_xlsx(Between.avg,Directory)

#For Likely Between (WL)
BetweenL <- data.frame(V(g)$name,V(g)$bet.likely)
colnames(BetweenL) <- c("Name","BetweenL_WL")
Directory <- paste(Direct,"BetweenL.xlsx", sep="")
write_xlsx(BetweenL,Directory)

#For Likely Between average (WL.avg)
BetweenL.avg <- data.frame(V(g)$name,V(g)$bet.avg.likely)
colnames(BetweenL.avg) <- c("Name","BetweenLA_WLA")
Directory <- paste(Direct,"BetweenL.avg.xlsx", sep="")
write_xlsx(BetweenL.avg,Directory)

#----------------------------------------------------------------#

Vertices <- data.frame(V(g)$name, V(g)$centr.degree.in, V(g)$centr.degree.out,
                       V(g)$prevention, V(g)$reaching, V(g)$reaching.avg,
                       V(g)$reaching.likely, V(g)$reaching.avg.likely,
                       V(g)$bet, V(g)$bet.avg, V(g)$bet.likely,
                       V(g)$bet.avg.likely)
colnames(Vertices) <- c("Name", "Degreein", "Degreeout", "Prev_barrier",
                    "Reaching_S","Reaching_SA",
                    "ReachingL_SL", "ReachingLA_SLA",
                    "Bet_S","Bet_SA",
                    "BetL_SL", "BetLA_SLA")
Directory <- paste(Direct,"Vertex.xlsx", sep="")
write_xlsx(Vertices,Directory)

#-------------------------------------------------------------#

E(g)$name <- c("CyBr->StID", "CyBr->DaEr", "CyBr->PfLw", "CyBr->TrUA",
               "CyBr->ExIB", "CyBr->Acci", "SfFl->DaEr", "SfFl->PfLw",
               "DaCp->DaEr", "DaCp->PfLw", "DaCp->ExIB", "BCIn->SCEE",
               "BCIn->PfLw", "BCIn->ExIB", "IfFl->DaEr", "IfFl->PfLw",
               "SCFr->SCEE", "SCFr->TrUA", "SCUn->SCEE", "SCUn->SCDp",
               "SCUn->PtCo", "StID->DaEr", "StID->TrUA", "StID->ExIB",
               "DaEr->SCEE", "DaEr->DySC", "DaEr->SCDp", "DaEr->Acci",
               "SCEE->DySC", "SCEE->TrUA", "PfLw->DySC", "DaEr->CAct",
               "DaEr->CgDg", "DaEr->DmLi", "SCEE->CAct", "SCEE->DySP",
               "SCEE->LoSP", "SCEE->CgRr", "SCEE->TrFl", "PfLw->PtCo",
               "DySC->DySP", "DySC->CgDg", "TrUA->SCDp", "TrUA->CyRs",
               "TrUA->LoSP", "SCDp->TrFl", "Acci->PtCo", "Acci->DySP",
               "Acci->CgDg", "ExIB->CyRs", "ExIB->CAct", "ExIB->LoSP",
               "PtCo->DySP", "PtCo->CgDg")

Edge <- data.frame(E(g)$name, E(g)$bet,E(g)$bet.avg,
                   E(g)$bet.likely, E(g)$bet.avg.likely,
                   E(g)$weight, E(g)$std, E(g)$entropy, E(g)$entro.contri)
colnames(Edge) <- c("Name","Bet_W","BetA_WA",
                    "BetL_WL", "BetLA_WLA", "Prob",
                    "Std", "Entropy", "Entro_contri")
Directory <- paste(Direct,"Edge.xlsx", sep="")
write_xlsx(Edge,Directory)

