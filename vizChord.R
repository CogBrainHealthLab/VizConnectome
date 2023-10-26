## FUNCTIONS FOR GENERATING CHORD DIAGRAMS TO ILLUSTRATE NETWORK CONNECTIVITY
## LAST UPDATED: 25 October 2023
## FOR USE IN THE COGNITIVE AND BRAIN HEALTH LABORATORY

########################################################################################################
########################################################################################################
quad = function(len)
{
  a=1
  b=-1
  c=-2*len
  a = as.complex(a)
  answer = c((-b + sqrt(b^2 - 4 * a * c)) / (2 * a),
              (-b - sqrt(b^2 - 4 * a * c)) / (2 * a))
  if(all(Im(answer) == 0)) answer = Re(answer)
  if(answer[1] == answer[2]) return(max(answer[1]))
  max(answer)
}
########################################################################################################
########################################################################################################

vizChord_edge=function(data,width,height,hot,cold, colorscheme, filename)
{
  localenv = environment() 
  require(circlize)
  require(ggplotify)
  require(ggplot2)
  require(cowplot)
  
  if(length(data)==23871){label=read.csv("https://github.com/CogBrainHealthLab/VizConnectome/blob/main/labels/labelsFC_schaefer219.csv?raw=TRUE")}
  else if(length(data)==30135){label=read.csv("https://github.com/CogBrainHealthLab/VizConnectome/blob/main/labels/labelsFC_brainnetome_yeo.csv?raw=TRUE")} 
  else if(length(data)==7021){label=read.csv("https://github.com/CogBrainHealthLab/VizConnectome/blob/main/labels/labelsFC_schaefer119.csv?raw=TRUE")} 
  else if(length(data)==4005){label=read.csv("https://github.com/CogBrainHealthLab/VizConnectome/blob/main/labels/labelsSC_AAL90.csv?raw=TRUE")}

  label=label[order(label$region),]
  labelnameFC=unique(label$regionlabel)
  
  regionnoFC=label$region
  noregions=length(labelnameFC)
  nnode=quad(length(data))
  ##color parameters
  
  pos_color_range= colorRampPalette(c("white",hot))
  neg_color_range = colorRampPalette(c("white",cold))
  pos_color_val=pos_color_range(101)
  neg_color_val=neg_color_range(101)
  
  FC_matrix=array(rep(0,nnode^2),dim=c(nnode,nnode))
  FC_matrix[lower.tri(FC_matrix, diag=FALSE)] = data
  FC_matrix=FC_matrix+t(FC_matrix)
  
  FCregionmat=array(dim=c(noregions,noregions))
  for (rowno in 1:noregions){
    for (colno in 1:noregions){
      FCregionmat[rowno,colno]=
        mean(FC_matrix
             [label$oldorder[which(regionnoFC==rowno)],label$oldorder[which(regionnoFC==colno)]])
    }
  }
  
  #region x region matrix FC
  FCregionmat[lower.tri(FCregionmat)]=NA
  x=array(dim=c(noregions^2,3))
  
  a=1
  for (rowno in 1:noregions){
    for (colno in 1:noregions){
      x[a,1]=rowno
      x[a,2]=colno
      if (rowno==colno) {
        x[a,3]=(FCregionmat[rowno,colno])/2
      }else {
        x[a,3]=FCregionmat[rowno,colno]
      }
      a=a+1
    }
  }
  
  datFC=x[-which(is.na(x[,3])),]
  datFC=data.frame(datFC, stringsAsFactors = F)
  
  colnames(datFC)=c("from","to","value")
  col_funFC = colorRamp2(range(datFC$value), c(cold,hot))
  
  for (label in 1:noregions){
    datFC$from[which(datFC$from==label)]=labelnameFC[label]
    datFC$to[which(datFC$to==label)]=labelnameFC[label]
  }
  colarrFC=array()

  if(NROW(which(datFC$value==0))==0){
    datrmFC=datFC
  } else {
    datrmFC=datFC[-which(datFC$value==0),]}

  for (datval in which(datrmFC$value>0)){
    colarrFC[datval]=pos_color_val[round(abs(datrmFC$value[datval])/max(abs(datrmFC$value))*100)+1]
  }

  for (datval in which(datrmFC$value<0)){
    colarrFC[datval]=neg_color_val[round(abs(datrmFC$value[datval])/max(abs(datrmFC$value))*100)+1]
  }

  FCchord=as.grob(~suppressWarnings(chordDiagram(datrmFC, col=colarrFC, order=labelnameFC, self.link = 2,
                                 grid.col=colorscheme,annotationTrack = c("grid","name"),link.border=colarrFC,)),envir = localenv)

  legend.plot=ggplot(datFC, aes(color=value, x=value, y=value))+
    scale_colour_gradient2(name="Connectivity strength",low=hot,mid="white",high=cold,
                           guide = "colourbar", limits=c(-1,1), breaks=c(-1,1),
                           labels=c("Strong negative","Strong positive"))+
    geom_point()+
    guides(color=guide_colorbar(ticks=F,title.position = "left",barheight = 0.5))+
    theme(legend.position = "bottom",legend.title=element_text(face="bold", size=7,vjust=1), 
          legend.text =element_text(size=6))
  legend = get_legend(legend.plot)
  
  png(filename =filename, width = width, height = height, res = 300)
    p=plot_grid(FCchord,legend,ncol=1, nrow=2, rel_heights = c(0.95,0.05))
    print(p)
  dev.off()
}

########################################################################################################
########################################################################################################

vizChord_12x12=function(data,width,height,hot,cold, colorscheme, filename)
{
  localenv = environment() 
  require(circlize)
  require(ggplotify)
  require(ggplot2)
  require(cowplot)

  nodesname<-c("Auditory","Cinguloopercular","Cinguloparietal",
               "Default\nmode","Dorsal\nattention","Frontoparietal",
               "Retrosplenial\ntemporal","Sensorimotor\nhand","Sensorimotor\nmouth",
               "Salience","Ventral\nattention","Visual")
  no_nodes<-12
  
  #color parameters
  colarrFC<-array()
  red = colorRampPalette(c("white",hot))
  blue = colorRampPalette(c("white",cold))
  redval<-red(101)
  blueval<-blue(101)
  
  #reconstruct 12 x 12 FC matrices
  FC_12X12<-array(rep(NA,12^2),dim=c(12,12))
  FC_12X12[upper.tri(FC_12X12, diag=T)] <- data #your results input
  
  a=1
  
  datFC0<-array(dim=c(144,3))
  for (rowno in 1:no_nodes){
    for (colno in 1:no_nodes){
      datFC0[a,1]<-rowno
      datFC0[a,2]<-colno
      datFC0[a,3]<-FC_12X12[rowno,colno]
      a=a+1
    }
  }
  #converting 12 x 12 FC matrices to a 'from node to node' data frame
  datFC<-datFC0[-which(is.na(datFC0[,3])),]
  datFC<-data.frame(datFC, stringsAsFactors = F)
  
  colnames(datFC)<-c("from","to","value")
  
  #sets up the color function such that darker colors correspond to stronger connections
  
  col_funFC = colorRamp2(range(datFC$value), c(hot,cold))
  
  for (label in 1:no_nodes){
    datFC$from[which(datFC$from==label)]<-nodesname[label]
    datFC$to[which(datFC$to==label)]<-nodesname[label]
  }
  
  for (datval in which(datFC$value>0)){
    colarrFC[datval]<-redval[round(abs(datFC$value[datval])/max(abs(datFC$value))*100)+1]
  }
  
  for (datval in which(datFC$value<0)){
    colarrFC[datval]<-blueval[round(abs(datFC$value[datval])/max(abs(datFC$value))*100)+1]
  }
  
  #generate and saves the FC chord diagram to the FCchord object
  FCchord<-as.grob(~chordDiagram(datFC, col=colarrFC, self.link = 1, 
                                 grid.col=colorscheme[1:12], annotationTrack = c("grid","name"),link.border=colarrFC),envir = localenv)
  legend.plot=ggplot(datFC, aes(color=value, x=value, y=value))+
    scale_colour_gradient2(name="Connectivity strength",low=hot,mid="white",high=cold,
                           guide = "colourbar", limits=c(-1,1), breaks=c(-1,1),
                           labels=c("Strong negative","Strong positive"))+
    geom_point()+
    guides(color=guide_colorbar(ticks=F,title.position = "left",barheight = 0.5))+
    theme(legend.position = "bottom",legend.title=element_text(face="bold", size=7,vjust=1), 
          legend.text =element_text(size=6))
  legend = get_legend(legend.plot)
  
  png(filename =filename, width = width, height = height, res = 300)
  p=plot_grid(FCchord,legend,ncol=1, nrow=2, rel_heights = c(0.95,0.05))
  print(p)
  dev.off()
  
}
########################################################################################################
########################################################################################################
vizChord=function(data, hot="#F8766D", cold="#00BFC4", width=1800, height=1800,filename="conn.png", colorscheme)
{
  ## checked required packages
  list.of.packages = c("ggplot2", "circlize","cowplot","ggplotify")
  new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) 
  {
    cat(paste("The following package(s) are required and will be installed:\n",new.packages,"\n"))
    install.packages(new.packages)
  }
  ##redirect to other functions depending on length of data
  if((length(data)==23871 |length(data)==7021)| length(data)==30135)
  {
    if(missing("colorscheme")){colorscheme = c("#D53E4F","#F46D43","#FDAE61","#FEE08B","#E6F598","#ABDDA4","#66C2A5","#3288BD")}
    
    vizChord_edge(data=data,hot=hot,cold=cold,width=width,height=height,filename=filename, colorscheme = colorscheme)  
    
  } else if (length(data)==4005) 
  {    
    if(missing("colorscheme")){colorscheme = c("#D53E4F","#FC8D59","#FEE08B","#FFFFBF","#E6F598","#99D594","#3288BD")}
    
    vizChord_edge(data=data,hot=hot,cold=cold,width=width,height=height,filename=filename, colorscheme = colorscheme)  
  } else if(length(data)==78)
  {
    if(missing("colorscheme"))
      {
      colorscheme=c("#A71B4BFF","#D04939FF","#EB7803FF","#F5A736FF","#FBCF6FFF","#FEF1A6FF","#E2F8B5FF","#9CE5ADFF","#43CBB1FF","#00AAB6FF","#0080B2FF","#584B9FFF") 
    }
    vizChord_12x12(data=data,hot=hot,cold=cold,width=width,height=height,filename=filename, colorscheme = colorscheme)  
  } else
  {cat("The length of the input vector does not fit any of the recognized parcellation schemes. The input vector should contain 78, 4005, 7021, 23871 or 30135 values")}
}
########################################################################################################
########################################################################################################
##EXAMPLE

#data = runif(78, min = -1, max = 1)
#vizChord(data=data, filename="FCchord.png")
