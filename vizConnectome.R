## FUNCTIONS FOR GENERATING CONNECTOGRAM PLOTS
## FOR USE IN THE COGNITIVE AND BRAIN HEALTH LABORATORY

########################################################################################################
########################################################################################################

vizConnectogram=function(data, hot="#F8766D", cold="#00BFC4", edgethickness=0.8,filename="conn.png", colorscheme)
{
  ## checked require packages and load them
  list.of.packages = c("ggplot2", "ggraph","igraph","gridExtra","grid")
  new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  
  if(length(new.packages)) 
  {
    cat(paste("The following package(s) are required and will be installed:\n",new.packages,"\n"))
    install.packages(new.packages)
  }  
  require(ggplot2)
  require(ggraph)
  require(igraph)
  require(gridExtra)
  require(grid)
  
  ##selecting atlas
  labels.url=c("https://github.com/CogBrainHealthLab/VizConnectome/blob/main/labels/labelsSC_AAL90.csv?raw=TRUE",
              "https://github.com/CogBrainHealthLab/VizConnectome/blob/main/labels/labelsFC_schaefer119.csv?raw=TRUE",
              "https://github.com/CogBrainHealthLab/VizConnectome/blob/main/labels/labelsFC_schaefer219.csv?raw=TRUE",
              "https://github.com/CogBrainHealthLab/VizConnectome/blob/main/labels/labelsFC_brainnetome_yeo.csv?raw=TRUE")
  
  edge_lengths=c(4005,7021,23871,30135)
  
  if(is.na(match(length(data),edge_lengths)))
  {
    stop("The length of the input vector does not fit any of the recognized parcellation schemes. The input vector should contain 4005, 7021, 23871 or 30135 values")      
  } else
  {
    atlas=match(length(data),edge_lengths)
  }

  ##parameters for all atlases
  param=list(NA,NA,NA,NA,NA)
  names(param)=c("nodecol","nodesize","xlim","ylim","nodelevels")
  param$nodecol=list(c("#D53E4F","#FC8D59","#FEE08B","#FFFFBF","#E6F598","#99D594","#3288BD"),
                     c("#D53E4F","#F46D43","#FDAE61","#FEE08B","#E6F598","#ABDDA4","#66C2A5","#3288BD"))
  param$nodecol[3:4]=param$nodecol[2]
  param$nodesize=c(1.4,1.3,1,1)
  param$xlim=list(c(-1.15, 1.6),
                  c(-1.15, 1.5),
                  c(-1.2, 1.6),
                  c(-1.15, 1.5))
  param$ylim=list(c(-1.2, 1.2),
                  c(-1.2, 1),
                  c(-1.15, 1.2),
                  c(-1.2, 1))
  
  param$nodelevels=list(c("frontal","central","temporal","parietal","limbic","subcortical","occipital"),
                        c("Visual", "Somatomotor", "Dorsal Attention","Ventral Attention","Limbic","Frontoparietal","Default mode","Subcortical"))
  param$nodelevels[3:4]=param$nodelevels[2]
  
  ##plot parameters
  label=read.csv(labels.url[atlas])
  label$regionlabel = factor(label$regionlabel,levels = param$nodelevels[[atlas]])
  if(missing("colorscheme")){colorscheme = param$nodecol[[atlas]]}
  nnodes=nrow(label)
  
  ##rehaping data into connectivity matrix
  
  conmat_NxNhalf = matrix(0, nrow = nnodes, ncol = nnodes)
  conmat_NxNhalf[upper.tri(conmat_NxNhalf, diag = FALSE)] = data
  conmat_NxN=conmat_NxNhalf+t(conmat_NxNhalf)
  
  nodeorder=as.numeric(rep(NA,nnodes))
  for (rowno in 1:nnodes){
    nodeorder[rowno]=which(label$neworder==rowno)
  }
  
  colnames(conmat_NxN)=label$labels
  rownames(conmat_NxN)=label$labels
  reordered=conmat_NxN[nodeorder,nodeorder]
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
    scale_color_manual(values =param$nodecol[[atlas]], name="Network")+
    geom_node_point(aes(colour = RegionsFC),size=param$nodesize[atlas], shape=19,show.legend = T) +
    geom_node_text(aes(label = name, x = x * 1.03, y = y* 1.03,
                       angle = ifelse(atan(-(x/y))*(180/pi) < 0,
                                      90 + atan(-(x/y))*(180/pi),
                                      270 + atan(-x/y)*(180/pi)),
                       hjust = ifelse(x > 0, 0 ,1)), size=param$nodesize[atlas]) +    
    guides(edge_color = guide_legend(override.aes = list(shape = NA)),
           color= guide_legend(override.aes = list(edge_width = NA))) +
    theme_graph(background = 'white', text_colour = 'black', bg_text_colour = 'black')+
    expand_limits(x = param$xlim[[atlas]], y = param$ylim[[atlas]])+
    
    theme(plot.margin = rep(unit(0,"null"),4),
          legend.title=element_text(size=5,face = "bold"),
          legend.text=element_text(size=5),
          legend.position = c(1,0),legend.justification=c(1,0), legend.key.height = unit(c(0, 0, 0, 0), "cm"))
  
    png(filename=filename, width =1550, height = 1200, res=300)
      suppressWarnings(
        grid.arrange(FCplot, nrow=1, ncol=1, 
                    left=textGrob("Left hemisphere",gp = gpar(fontface=2,fontsize = 6),rot=90, hjust = 0.5,x = 0.5), 
                    right=textGrob("Right hemisphere",gp = gpar(fontface=2,fontsize = 6),rot=90, hjust = 0.5,x = -3))
      )
    dev.off()
}
########################################################################################################
########################################################################################################
##EXAMPLE

##data=sample(c(1,0, -1), 30135, replace = T, prob = c(0.001, 0.998,0.001))
##vizConnectogram(data=data, filename="FC246.png")
