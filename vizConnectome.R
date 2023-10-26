## FUNCTIONS FOR GENERATING CONNECTOGRAM PLOTS
## LAST UPDATED: 25 October 2023
## FOR USE IN THE COGNITIVE AND BRAIN HEALTH LABORATORY

########################################################################################################
########################################################################################################
viz219=function(data,edgethickness,width,height,hot,cold, colorscheme, filename)
{
  require(ggplot2)
  require(ggraph)
  require(igraph)
  require(gridExtra)
  require(grid)
  label = read.csv("https://github.com/CogBrainHealthLab/VizConnectome/blob/main/labels/labelsFC_schaefer219.csv?raw=TRUE")
  label$regionlabel = factor(label$regionlabel, levels = c("Visual", "Somatomotor", "Dorsal Attention", 
                                                            "Ventral Attention","Limbic","Frontoparietal", 
                                                            "Default mode","Subcortical"))
  ##reconstruct FC matrices
  FC_219x219half = matrix(0, nrow = 219, ncol = 219)
  FC_219x219half[upper.tri(FC_219x219half, diag = FALSE)] = data
  FC_219X219=FC_219x219half+t(FC_219x219half)
  
  ##sorting and reordering labels according to their respective regions
  nodeorder=as.numeric(rep(NA,219))
  for (rowno in 1:219){
    nodeorder[rowno]=which(label$neworder==rowno)
  }
  
  colnames(FC_219X219)=label$labels
  rownames(FC_219X219)=label$labels
  reordered=FC_219X219[nodeorder,nodeorder]
  RegionsFC=as.factor(label$regionlabel)[nodeorder]
  
  ##graph object
  
  graphobjFC=graph_from_adjacency_matrix(reordered, mode="undirected", diag=FALSE, weighted=T)
  EvalFC=edge_attr(graphobjFC, "weight", index = E(graphobjFC))
  posnegFC=EvalFC
  posnegFC=replace(posnegFC, which(posnegFC < 0), "2_neg")
  posnegFC=replace(posnegFC, which(posnegFC!="2_neg"), "1_pos")
  
  edge_attr(graphobjFC, "weight", index = E(graphobjFC))=abs(EvalFC)
  edge_attr(graphobjFC, "posFC", index = E(graphobjFC))=posnegFC
  
  #plot
  
  FCplot=ggraph(graphobjFC, layout = 'linear', circular = TRUE) +  
    geom_edge_arc(aes(color=posnegFC, alpha=weight), edge_width=edgethickness, show.legend = T) +
    scale_edge_alpha_continuous(guide="none")+
    scale_edge_color_manual(name="Edges", labels=c("Positive","Negative"),values=c(hot,cold))+
    scale_color_manual(values =colorscheme, name="Network")+
    geom_node_point(aes(colour = RegionsFC),size=1, shape=19,show.legend = T) +
    geom_node_text(aes(label = name, x = x * 1.03, y = y* 1.03,
                       angle = ifelse(atan(-(x/y))*(180/pi) < 0,
                                      90 + atan(-(x/y))*(180/pi),
                                      270 + atan(-x/y)*(180/pi)),
                       hjust = ifelse(x > 0, 0 ,1)), size=1) +    
    guides(edge_color = guide_legend(override.aes = list(shape = NA)),
           color= guide_legend(override.aes = list(edge_width = NA))) +
    theme_graph(background = 'white', text_colour = 'black', bg_text_colour = 'black')+
    expand_limits(x = c(-1.2, 1.6), y = c(-1.15, 1.2))+
    theme(plot.margin = rep(unit(0,"null"),4),
          legend.title=element_text(size=5,face = "bold"),
          legend.text=element_text(size=5),
          legend.position = c(1,0),legend.justification=c(1,0), legend.key.height = unit(c(0, 0, 0, 0), "cm"))
  png(filename =filename, width =1550, height = 1150, res=300)
  grid.arrange(FCplot, nrow=1, ncol=1, 
               left=textGrob("Left hemisphere",gp = gpar(fontface=2,fontsize = 5),rot=90, hjust = 0.5,x = 0.5), 
               right=textGrob("Right hemisphere",gp = gpar(fontface=2,fontsize = 5),rot=90, hjust = 0.5,x = -3))
  dev.off()
}
########################################################################################################
########################################################################################################
viz119=function(data,edgethickness,width,height,hot,cold, colorscheme, filename)
{
  require(ggplot2)
  require(ggraph)
  require(igraph)
  require(gridExtra)
  require(grid)
  label = read.csv("https://github.com/CogBrainHealthLab/VizConnectome/blob/main/labels/labelsFC_schaefer119.csv?raw=TRUE")
  label$regionlabel = factor(label$regionlabel, levels = c("Visual", "Somatomotor", "Dorsal Attention", 
                                                            "Ventral Attention","Limbic","Frontoparietal", 
                                                            "Default mode","Subcortical"))
  ##reconstruct FC matrices
  FC_119x119half = matrix(0, nrow = 119, ncol = 119)
  FC_119x119half[upper.tri(FC_119x119half, diag = FALSE)] = data
  FC_119X119=FC_119x119half+t(FC_119x119half)
  
  ##sorting and reordering labels according to their respective regions
  nodeorder=as.numeric(rep(NA,119))
  for (rowno in 1:119){
    nodeorder[rowno]=which(label$neworder==rowno)
  }
  
  colnames(FC_119X119)=label$labels
  rownames(FC_119X119)=label$labels
  reordered=FC_119X119[nodeorder,nodeorder]
  RegionsFC=as.factor(label$regionlabel)[nodeorder]
  
  ##graph object
  
  graphobjFC=graph_from_adjacency_matrix(reordered, mode="undirected", diag=FALSE, weighted=T)
  EvalFC=edge_attr(graphobjFC, "weight", index = E(graphobjFC))
  posnegFC=EvalFC
  posnegFC=replace(posnegFC, which(posnegFC < 0), "2_neg")
  posnegFC=replace(posnegFC, which(posnegFC!="2_neg"), "1_pos")
  
  edge_attr(graphobjFC, "weight", index = E(graphobjFC))=abs(EvalFC)
  edge_attr(graphobjFC, "posFC", index = E(graphobjFC))=posnegFC
  
  #plot
  
  FCplot=ggraph(graphobjFC, layout = 'linear', circular = TRUE) +  
    geom_edge_arc(aes(color=posnegFC, alpha=weight), edge_width=edgethickness, show.legend = T) +
    scale_edge_alpha_continuous(guide="none")+
    scale_edge_color_manual(name="Edges", labels=c("Positive","Negative"),values=c(hot,cold))+
    scale_color_manual(values =colorscheme, name="Regions")+
    geom_node_point(aes(colour = RegionsFC),size=1.5, shape=19,show.legend = T) +
    geom_node_text(aes(label = name, x = x * 1.03, y = y* 1.03,
                       angle = ifelse(atan(-(x/y))*(180/pi) < 0,
                                      90 + atan(-(x/y))*(180/pi),
                                      270 + atan(-x/y)*(180/pi)),
                       hjust = ifelse(x > 0, 0 ,1)), size=1.5) +    
    guides(edge_color = guide_legend(override.aes = list(shape = NA)),
           color= guide_legend(override.aes = list(edge_width = NA))) +
    theme_graph(background = 'white', text_colour = 'black', bg_text_colour = 'black')+
    expand_limits(x = c(-1.15, 1.5), y = c(-1.2, 1))+
    theme(plot.margin = rep(unit(0,"null"),4),
          legend.title=element_text(size=5,face = "bold"),
          legend.text=element_text(size=5),
          legend.position = c(1,0),legend.justification=c(1,0), legend.key.height = unit(c(0, 0, 0, 0), "cm"))
  png(filename =filename, width =1550, height = 1150, res=300)
  grid.arrange(FCplot, nrow=1, ncol=1, 
               left=textGrob("Left hemisphere",gp = gpar(fontface=2,fontsize = 5),rot=90, hjust = 0.5,x = 0.5), 
               right=textGrob("Right hemisphere",gp = gpar(fontface=2,fontsize = 5),rot=90, hjust = 0.5,x = -3))
  dev.off()
}
########################################################################################################
########################################################################################################
viz90=function(data,edgethickness,width,height,hot,cold, colorscheme, filename)
{
  require(ggplot2)
  require(ggraph)
  require(igraph)
  require(gridExtra)
  require(grid)
  label = read.csv("https://github.com/CogBrainHealthLab/VizConnectome/blob/main/labels/labelsSC_AAL90.csv?raw=TRUE")
  label$regionlabel = factor(label$X,levels = c("Frontal","Central","Temporal","Parietal",
                                                 "Limbic","Subcortical","Occipital"))
  
  ##reconstruct SC matrices
  SC_90x90half = matrix(0, nrow = 90, ncol = 90)
  SC_90x90half[upper.tri(SC_90x90half, diag = FALSE)] = data
  SC_90X90=SC_90x90half+t(SC_90x90half)
  
  nodeorder=as.numeric(rep(NA,90))
  for (rowno in 1:90){
    nodeorder[rowno]=which(label$neworder==rowno)
  }
  
  colnames(SC_90X90)=label$labels
  rownames(SC_90X90)=label$labels
  reordered=SC_90X90[nodeorder,nodeorder]
  RegionsFC=as.factor(label$regionlabel)[nodeorder]
  
  ##graph object
  
  graphobjSC=graph_from_adjacency_matrix(reordered, mode="undirected", diag=FALSE, weighted=T)
  EvalSC=edge_attr(graphobjSC, "weight", index = E(graphobjSC))
  posnegSC=EvalSC
  posnegSC=replace(posnegSC, which(posnegSC < 0), "2_neg")
  posnegSC=replace(posnegSC, which(posnegSC!="2_neg"), "1_pos")
  
  edge_attr(graphobjSC, "weight", index = E(graphobjSC))=abs(EvalSC)
  edge_attr(graphobjSC, "posSC", index = E(graphobjSC))=posnegSC
  
  #plot
  
  SCplot=ggraph(graphobjSC, layout = 'linear', circular = TRUE) +  
    geom_edge_arc(aes(color=posnegSC, alpha=weight), edge_width=edgethickness, show.legend = T) +
    scale_edge_alpha_continuous(guide="none")+
    scale_edge_color_manual(name="Edges", labels=c("Positive","Negative"),values=c(hot,cold))+
    scale_color_manual(values =colorscheme, name="Regions", labels=c("frontal","central","temporal",
                                                                     "parietal","limbic","subcortical","occipital"))+
    geom_node_point(aes(colour = RegionsSC),size=1.3, shape=19,show.legend = T) +
    geom_node_text(aes(label = name, x = x * 1.03, y = y* 1.03,
                       angle = ifelse(atan(-(x/y))*(180/pi) < 0,
                                      90 + atan(-(x/y))*(180/pi),
                                      270 + atan(-x/y)*(180/pi)),
                       hjust = ifelse(x > 0, 0 ,1)), size=1.3) +    
    guides(edge_color = guide_legend(override.aes = list(shape = NA)),
           color= guide_legend(override.aes = list(edge_width = NA))) +
    theme_graph(background = 'white', text_colour = 'black', bg_text_colour = 'black')+
    expand_limits(x = c(-1.15, 1.6), y = c(-1.2, 1.2))+
    theme(plot.margin = rep(unit(0,"null"),4),
          legend.title=element_text(size=5,face = "bold"),
          legend.text=element_text(size=5),
          legend.position = c(1,0),legend.justification=c(1,0), legend.key.height = unit(c(0, 0, 0, 0), "cm"))
  png(filename =filename, width =width, height = height, res=300)
  grid.arrange(SCplot, nrow=1, ncol=1, 
               left=textGrob("Left hemisphere",gp = gpar(fontface=2,fontsize = 6),rot=90, hjust = 0.5,x = 0.5), 
               right=textGrob("Right hemisphere",gp = gpar(fontface=2,fontsize = 6),rot=90, hjust = 0.5,x = -3.5))
  dev.off()
  
}
########################################################################################################
########################################################################################################
viz246=function(data,edgethickness,width,height,hot,cold, colorscheme, filename)
{
  require(ggplot2)
  require(ggraph)
  require(igraph)
  require(gridExtra)
  require(grid)
  
  label=read.csv("https://github.com/CogBrainHealthLab/VizConnectome/blob/main/labels/labelsFC_brainnetome_yeo.csv?raw=TRUE")
  label$regionlabel = factor(label$regionlabel,levels = c("Visual","Somatomotor","Dorsal Attention", "Ventral Attention",
                                                           "Limbic","Frontoparietal","Default Mode","Subcortical"))
  FC_246x246half = matrix(0, nrow = 246, ncol = 246)
  FC_246x246half[upper.tri(FC_246x246half, diag = FALSE)] = data
  FC_246X246=FC_246x246half+t(FC_246x246half)
  
  nodeorder=as.numeric(rep(NA,246))
  for (rowno in 1:246){
    nodeorder[rowno]=which(label$neworder==rowno)
  }
  
  colnames(FC_246X246)=label$labels
  rownames(FC_246X246)=label$labels
  reordered=FC_246X246[nodeorder,nodeorder]
  RegionsFC=as.factor(label$regionlabel)[nodeorder]
  
  ##graph object
  
  graphobjFC=graph_from_adjacency_matrix(reordered, mode="undirected", diag=FALSE, weighted=T)
  EvalFC=edge_attr(graphobjFC, "weight", index = E(graphobjFC))
  posnegFC=EvalFC
  posnegFC=replace(posnegFC, which(posnegFC < 0), "2_neg")
  posnegFC=replace(posnegFC, which(posnegFC!="2_neg"), "1_pos")
  
  edge_attr(graphobjFC, "weight", index = E(graphobjFC))=abs(EvalFC)
  edge_attr(graphobjFC, "posFC", index = E(graphobjFC))=posnegFC
  
  #plot
  
  FCplot=ggraph(graphobjFC, layout = 'linear', circular = TRUE) +  
    geom_edge_arc(aes(color=posnegFC, alpha=weight), edge_width=edgethickness, show.legend = T) +
    scale_edge_alpha_continuous(guide="none")+
    scale_edge_color_manual(name="Edges", labels=c("Positive","Negative"),values=c(hot,cold))+
    scale_color_manual(values =colorscheme, name="Network")+
    geom_node_point(aes(colour = RegionsFC),size=1, shape=19,show.legend = T) +
    geom_node_text(aes(label = name, x = x * 1.03, y = y* 1.03,
                       angle = ifelse(atan(-(x/y))*(180/pi) < 0,
                                      90 + atan(-(x/y))*(180/pi),
                                      270 + atan(-x/y)*(180/pi)),
                       hjust = ifelse(x > 0, 0 ,1)), size=1) +    
    guides(edge_color = guide_legend(override.aes = list(shape = NA)),
           color= guide_legend(override.aes = list(edge_width = NA))) +
    theme_graph(background = 'white', text_colour = 'black', bg_text_colour = 'black')+
    expand_limits(x = c(-1.15, 1.5), y = c(-1.2, 1))+
    theme(plot.margin = rep(unit(0,"null"),4),
          legend.title=element_text(size=5,face = "bold"),
          legend.text=element_text(size=5),
          legend.position = c(1,0),legend.justification=c(1,0), legend.key.height = unit(c(0, 0, 0, 0), "cm"))
  
  png(filename =filename, width =width, height = height, res=300)
  grid.arrange(FCplot, nrow=1, ncol=1, 
               left=textGrob("Left hemisphere",gp = gpar(fontface=2,fontsize = 6),rot=90, hjust = 0.5,x = 0.5), 
               right=textGrob("Right hemisphere",gp = gpar(fontface=2,fontsize = 6),rot=90, hjust = 0.5,x = -3))
  dev.off()
}
########################################################################################################
########################################################################################################

vizConnectogram=function(data, hot="#F8766D", cold="#00BFC4", width, height,edgethickness=0.8,filename="conn.png", colorscheme)
{
  ## checked required packages
  list.of.packages = c("ggplot2", "ggraph","igraph","gridExtra","grid")
  new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) 
  {
    cat(paste("The following package(s) are required and will be installed:\n",new.packages,"\n"))
    install.packages(new.packages)
  }  
  if(length(data)==7021)
  {
    if(missing("colorscheme")){colorscheme = c("#D53E4F","#F46D43","#FDAE61","#FEE08B","#E6F598","#ABDDA4","#66C2A5","#3288BD")}
    if(missing("width")){width=1550}
    if(missing("height")){height=1150}
    
    viz119(data=data,hot=hot,cold=cold,width=width,height=height,edgethickness=edgethickness, filename=filename, colorscheme = colorscheme)  
  } else if (length(data)==23871) 
  {    
    if(missing("colorscheme")){colorscheme = c("#D53E4F","#F46D43","#FDAE61","#FEE08B","#E6F598","#ABDDA4","#66C2A5","#3288BD")}
    if(missing("width")){width=1550}
    if(missing("height")){height=1150}
    
    viz219(data=data,hot=hot,cold=cold,width=width,height=height,edgethickness=edgethickness, filename=filename, colorscheme = colorscheme)  
  } else if (length(data)==30135) 
  { 
    if(missing("colorscheme")){colorscheme = c("#D53E4F","#F46D43","#FDAE61","#FEE08B","#E6F598","#ABDDA4","#66C2A5","#3288BD")}
    if(missing("width")){width=1550}
    if(missing("height")){height=1150}
    
    viz246(data=data,hot=hot,cold=cold,width=width,height=height,edgethickness=edgethickness, filename=filename, colorscheme = colorscheme)  
  } else if (length(data)==4005) 
  {    
    if(missing("colorscheme")){colorscheme = c("#D53E4F","#FC8D59","#FEE08B","#FFFFBF","#E6F598","#99D594","#3288BD")}
    if(missing("width")){width=1500}
    if(missing("height")){height=1150}
    
    viz90(data=data,hot=hot,cold=cold,width=width,height=height,edgethickness=edgethickness, filename=filename, colorscheme = colorscheme)  
  } else
  {cat("The length of the input vector does not fit any of the recognized parcellation schemes. The input vector should contain 4005, 7021, 23871 or 30135 values")}
}
########################################################################################################
########################################################################################################
##EXAMPLE

##data=sample(c(1,0, -1), 238721, replace = T, prob = c(0.001, 0.998,0.001))
##vizConnectome(data=data, filename="219.png")
