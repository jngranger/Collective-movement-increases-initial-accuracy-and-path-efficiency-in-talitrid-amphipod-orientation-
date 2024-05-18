#Violin plots that include GS5
violinplot=function(Variable,ylab=Variable, GS5=F, basesize=15, psize=5)
{
  #Variable is the Variable of interest to plot
  #ylab controls the title for the y axis
  #GS5 is either true if you are plotting the GS5 data or false if you are plotting the GS10 data
  #basesize lets you control the size of the text
  #psize lets you control the size of the text specifically for the pvalue
  
  #filter and rename the variable of interest
  tmp=GSall.alltrials |> 
    filter(Weather=="Sunny", Smoky=="Not Smoky") |> 
    rename(Variable=!!Variable) 
  
  #Select either GS5 or GS10
  if (GS5==T) {
    UD=GS1.5[[Variable]]
    color="darkred"
    df=tmp |> 
      filter(GS!=10) |>
      mutate(GS=recode(GS,"1"="GS1", "5"="GS5")) 
  }
  
  if (GS5==F) {
    UD=GS1.10[[Variable]]
    color="darkorange"
    df=tmp |> 
      filter(GS!=5) |> 
      mutate(GS=recode(GS,"1"="GS1", "10"="GS10"))
    
  }
  
  # Calculating the p-value
  MC_UD=UD-mean(UD)
  MC_V=(df.GS1.mean[[Variable]]-mean(UD))
  pvalue=round(ifelse(MC_V>0,
            (sum(MC_UD>=(MC_V)))/length(MC_UD),
            (sum(MC_UD<=(MC_V)))/length(MC_UD)
  ),4)
  if (pvalue !=0) {pvalue=paste0("p=",pvalue)}
  if (pvalue==0) { pvalue="p<0.0001" }

  # plot limits
  maxy=max(df$Variable)
  miny=min(df$Variable)
  

  #The plot
  
  df |>
    ggplot()+
    theme_classic(base_size = basesize)+
    ggforce::geom_sina(aes(GS, Variable,fill=GS),shape=21,alpha=0.5)+
    geom_violin(aes(GS, Variable), alpha=0, linewidth=1)+
    labs(y=paste0(ylab))+
    theme(axis.title.x = element_blank(), legend.position = "none")+
    annotate(geom="segment",x=1, xend=2,y=maxy+maxy/10,yend=maxy+maxy/10)+
    scale_y_continuous(limits = c(miny,maxy+maxy/5))+
    annotate(geom = "text", x=1.5, y=maxy+maxy/10, label=paste0(pvalue), vjust=-0.5,size=psize)+
    scale_fill_manual(values = c("black",color))


}