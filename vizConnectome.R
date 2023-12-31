## FUNCTIONS FOR GENERATING CONNECTOGRAM PLOTS
## FOR USE IN THE COGNITIVE AND BRAIN HEALTH LABORATORY

########################################################################################################
########################################################################################################

edgelist=function(data)
{
  ## check require packages
  list.of.packages = c("ggplot2", "ggraph","igraph","gridExtra","grid")
  new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  
  if(length(new.packages)) 
  {
    cat(paste("The following package(s) are required and will be installed:\n",new.packages,"\n"))
    install.packages(new.packages)
  }  
  
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
  
  ##plot parameters
  label=read.csv(labels.url[atlas])
  label=label[order(label$oldorder),]
  label$labels=paste(label$hemi,"_",label$labels,sep="")
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
  
  graphobj=igraph::graph_from_adjacency_matrix(reordered, mode="undirected", diag=FALSE, weighted=T)
  edgelist=data.frame(cbind(igraph::get.edgelist(graphobj) , igraph::E(graphobj)$weight))
  names(edgelist)=c("node_1","node_2","weight")
  return(edgelist)
}

########################################################################################################
########################################################################################################

vizConnectogram=function(data, hot="#F8766D", cold="#00BFC4", edgethickness=0.8,filename="conn.png", colorscheme, colorbar=T)
{
  ## check require packages
  list.of.packages = c("ggplot2", "ggraph","igraph","gridExtra","grid")
  new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  
  if(length(new.packages)) 
  {
    cat(paste("The following package(s) are required and will be installed:\n",new.packages,"\n"))
    install.packages(new.packages)
  }  
  
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
  
  label=read.csv(labels.url[atlas])
  param$nodelevels=unique(label$regionlabel)
  label=label[order(label$oldorder),]
  label.neworder=label[order(label$neworder),]
  param$nodelevels=unique(label.neworder$regionlabel)
  label$regionlabel = factor(label$regionlabel,levels = param$nodelevels)
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
  
  graphobjFC=igraph::graph_from_adjacency_matrix(reordered, mode="undirected", diag=FALSE, weighted=T)
  EvalFC=igraph::edge_attr(graphobjFC, "weight", index = igraph::E(graphobjFC))
  posnegFC=EvalFC
  posnegFC=replace(posnegFC, which(posnegFC < 0), "2_neg")
  posnegFC=replace(posnegFC, which(posnegFC!="2_neg"), "1_pos")
  
  igraph::edge_attr(graphobjFC, "weight", index = igraph::E(graphobjFC))=abs(EvalFC)
  igraph::edge_attr(graphobjFC, "posFC", index = igraph::E(graphobjFC))=posnegFC
  
  #plot
  
  FCplot=ggraph::ggraph(graphobjFC, layout = 'linear', circular = TRUE) +  
    ggraph::geom_edge_arc(ggplot2::aes(color=posnegFC, alpha=weight), edge_width=edgethickness, show.legend = T) +
    ggraph::scale_edge_alpha_continuous(guide="none")+
    ggraph::scale_edge_color_manual(name="Edges", labels=c("Positive","Negative"),values=c(hot,cold))+
    ggplot2::scale_color_manual(values =colorscheme, name="Network")+
    ggraph::geom_node_point(ggplot2::aes(colour = RegionsFC),size=param$nodesize[atlas], shape=19,show.legend = T) +
    ggraph::geom_node_text(ggplot2::aes(label = name, x = x * 1.03, y = y* 1.03,
                                        angle = ifelse(atan(-(x/y))*(180/pi) < 0,90 + atan(-(x/y))*(180/pi), 270 + atan(-x/y)*(180/pi)),
                                        hjust = ifelse(x > 0, 0 ,1)), size=param$nodesize[atlas]) +    
    ggraph::theme_graph(background = 'white', text_colour = 'black', bg_text_colour = 'black')+
    ggplot2::expand_limits(x = param$xlim[[atlas]], y = param$ylim[[atlas]])+
    ggplot2::theme(plot.margin = rep(ggplot2::unit(0,"null"),4),
                   legend.title=ggplot2::element_text(size=5,face = "bold"),
                   legend.text=ggplot2::element_text(size=5),
                   legend.position = c(1,0),legend.justification=c(1,0), legend.key.height = ggplot2::unit(c(0, 0, 0, 0), "cm"))
  
  if(colorbar==T)
  {
    FCplot=FCplot+
      ggraph::scale_edge_color_manual(name="Edges", labels=c("Positive","Negative"),values=c(hot,cold))+
      ggplot2::guides(edge_color = ggplot2::guide_legend(override.aes = list(shape = NA)),color= ggplot2::guide_legend(override.aes = list(edge_width = NA)))
  } else if(colorbar==F)
  {
    FCplot=FCplot+
      ggraph::scale_edge_color_manual(name="Edges", labels=c("Positive","Negative"),values=c(hot,cold),guide="none")+
      ggplot2::guides(color= ggplot2::guide_legend(override.aes = list(edge_width = NA)))
  }
  
  
  png(filename=filename, width =1550, height = 1200, res=300)
  suppressWarnings(
    gridExtra::grid.arrange(FCplot, nrow=1, ncol=1, 
                            left=grid::textGrob("Left hemisphere",gp = grid::gpar(fontface=2,fontsize = 6),rot=90, hjust = 0.5,x = 0.5), 
                            right=grid::textGrob("Right hemisphere",gp = grid::gpar(fontface=2,fontsize = 6),rot=90, hjust = 0.5,x = -3))
  )
  dev.off()
}
########################################################################################################
########################################################################################################

##EXAMPLE
#source("https://github.com/CogBrainHealthLab/VizConnectome/blob/main/vizConnectome.R?raw=TRUE")
#data=sample(c(1,0, -1), 30135, replace = T, prob = c(0.001, 0.998,0.001))
#vizConnectogram(data=data, filename="FC246.png")
