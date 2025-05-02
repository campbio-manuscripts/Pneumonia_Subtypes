library(ConsensusClusterPlus)
library(tidyverse)
library(dplyr)
library(readxl)
library(tidyverse)
library(factoextra)
library(umap)
library(iheatmapr)
library(qgraph)
library(corpcor)
library(glasso)
library(BDgraph)
library(umap)
library(dendextend)
library(cluster)
library(ggplot2)
library(bayesbio)
library(ggpubr)
library(rstatix)
library(mclust)
library(psych)
library(scales)
library(ggpubr)
library(janitor)
library(fpc)
library(ppcor)
library(circlize)
library(ComplexHeatmap)
library(gtsummary)
library(xlsx)
library(rstatix)

# On Nov 19, I updated this script to include folder_name as input to function call. 

# Removes _1 & . from column names, changes some column names. 

clean_column_names<-function(dataset,org){
  colnames(dataset) %>%
    str_replace_all(.,"_1","") %>%
    str_replace_all(.,"\\."," ") -> new_colnames
  print(new_colnames)
  colnames(dataset)<-new_colnames
  if(org == "human"){
  dataset %>%
    dplyr::rename("Alveolar fibrin" = "Fibrin alveoli") %>%
    dplyr::rename("Mvessel fibrin" = "Fibrin mvessel") %>%
    dplyr::rename("Lympho" = "Lymphoplasmacytic") %>%
    dplyr::rename("AT2 hyperplasia" =  "AT 2 pneumocyte hyperplasia")-> dataset
  }
  
  else if (org == "mouse"){
    
    dataset %>%
      dplyr::rename("Alveolar fibrin" = "Fibrin_alveoli") %>%
      dplyr::rename("Lympho" = "Lymphoplasmacytic") %>%
      dplyr::rename("AT2 hyperplasia" =  "AT 2 pneumocyte hyperplasia")-> dataset
    
  }
  
  colnames(dataset) %>%
    str_replace_all(.," ","\n") -> new_colnames 
  colnames(dataset)<-new_colnames
  
  return(dataset)
}

# Takes in dataset and a column name which has group info. Performs KW test followed by dunn test. Saves results in csv files and returns the significant ones for dunn test

get_stats<-function(dataset,testing_variable,type,folder_name){
   
  if(type == "categorical"){
  
  formula<- as.formula(paste0("score ","~ ",rlang::as_string(testing_variable)))
    dataset %>%
      gather("feature","score",-one_of(testing_variable),-CaseID) %>%
      group_by(feature) %>%
      kruskal_test(formula = formula)%>%
      adjust_pvalue(method = "bonferroni") %>%
      add_significance("p.adj") -> kw_results
  
    # dunn_test
    # rstatix has a bug where if I add adjusted p-value with dunn_test, it shows the same value as p so adding another step
    # with adjust_pvalue to get the correct value
    dataset %>%
      gather("feature","score",-one_of(testing_variable),-CaseID) %>%  
      group_by(feature) %>%
      dunn_test(formula = formula) %>%
      adjust_pvalue(method = "bonferroni") %>%
      add_significance("p.adj") -> dunn_test_all
    
    fname1<-paste0(folder_name,testing_variable,"_dunn_test.csv")
    write.csv(dunn_test_all,file = fname1)
    
    fname2<-paste0(folder_name,testing_variable,"_kw_test.csv")
    write.csv(kw_results, file = fname2)
    
    dunn_test_all %>%
      filter(p.adj < 0.05) -> dunn_for_figure
    
    return(dunn_for_figure)
    
  }
  
  else if (type == "continuous"){
    formula<- as.formula(paste0("score ","~ ",rlang::as_string(testing_variable)))
    print(formula)
    dataset %>%
      gather("feature","score",-one_of(testing_variable),-CaseID) %>%
      group_by(feature) %>% 
    #  t_test(formula) %>%
      wilcox_test(formula, paired = FALSE) %>%
      adjust_pvalue(method = "bonferroni") %>%
      add_significance("p.adj") -> wilcox_test_results
    
    fname1<-paste0(folder_name,testing_variable,"_filtered_two_tailed_wilcox_test.csv")
    write.csv(wilcox_test_results,file = fname1)
    
    wilcox_test_results %>%
      filter(p.adj < 0.05) -> wilcox_test_results
    
    return(wilcox_test_results)
  }
  
}



# Make plots

make_boxplots_dotplots<-function(dataset, varb_name,plot_type, width=12, height=12, 
                                 bx_color = NULL, stats = NULL, xlab = xlab, dtype, 
                                 organism="human",nrow=1,folder_name, max = 1,title = NULL){
  
  if(plot_type == "dot"){
    p<-dataset %>%
      mutate(new_data = .data[[varb_name]]) %>%
      gather("feature","score",-one_of(varb_name),-new_data,-CaseID) %>% 
      group_by(feature,new_data) %>%
      mutate(mean = mean(score)) %>%
      mutate(count_per_grp = n()) %>% 
      mutate(perct_samples_greater_than_zero = sum(score>0)/count_per_grp * 100) %>% 
      ungroup() %>%
      ggplot(aes(feature,new_data,size = mean, color = perct_samples_greater_than_zero)) +
      geom_point(alpha=0.3) +
      scale_size(range = c(.10, 10),name = "Average score per sample type" ) +
      scale_color_distiller(type = "div", palette = "PuOr", name = paste("Percentage of samples", "\n" ,"with scores > 0")) +
      theme_classic()+ 
      coord_flip() +
      ylab("Sample Type") +
      xlab("Features") +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size = 20)) +
      theme(legend.title = element_text("Size"))+
      # theme(axis.text.x = element_text(angle = 45,size = 12,hjust = 0,vjust = 0.5)) +
      ggstyle() 
    fname<- paste0(folder_name,varb_name,"_",dtype,"_",organism,"_dot_plots.tiff")
    fname2<-paste0(folder_name,varb_name,"_",dtype,"_",organism, "_box_violin_plots.tiff")
    ggsave(p,filename = fname, width = width, height = height, scale=1, unit= "in", dpi=300)
    ggsave(p,filename = fname2, width = width, height = height, scale=1, unit= "in", dpi=300)
    
  }   
    
    if(plot_type == "bv"){
      print(sym(varb_name))
     p<-dataset %>% 
     #  mutate(new_data = .data[[varb_name]]) %>%
       ggplot(aes(y = score, x = .data[[varb_name]])) +
       geom_violin(width = 1, fill = "white") +
       geom_boxplot(alpha = 2, outlier.shape = NA, width= 0.3, aes(fill = .data[[varb_name]])) +
       geom_jitter(color = dataset$outlier,height = 0, width = 0.3) + # NA not plotted 
       ggstyle() +
       facet_wrap(~feature, nrow = nrow)+
       scale_fill_manual(values = bx_color)+
       theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 16,face = "bold"),strip.text = element_text(size = 12,face = "bold")) +
       theme(axis.text.y = element_text(size = 16,face = "bold"), axis.title.x.bottom = element_text(size = 16,face = "bold"), 
             axis.title.y.left = element_text(size = 16,face = "bold"),legend.position = "none")+ 
       xlab(xlab)+
       ylab("Score")+
       ggtitle(title)
     
     if(nrow(stats) != 0){
       p<- p + 
         ylim(-0.1,max(stats$y.position)+0.05) +
         stat_pvalue_manual(stats, label = "p.adj.signif", y.position = stats$y.position,size = 4)
         
       
     }
     else{
       p<- p 
     }
    
    fname<-paste0(folder_name,varb_name,"_",dtype,"_",organism, "_box_violin_plots.pdf")
    fname2<-paste0(folder_name,varb_name,"_",dtype,"_",organism, "_box_violin_plots.tiff")
    ggsave(p,filename = fname, width = width, height = height, scale=1, unit= "in", dpi=300)
    ggsave(p,filename = fname2, width = width, height = height, scale=1, unit= "in", dpi=300)
  
    }
    
  if(plot_type == "bvm"){
    print(sym(varb_name))
    p<-dataset %>% 
      mutate(new_data = .data[[varb_name]]) %>%
      ggplot(aes(y = score, x = new_data)) +
      geom_violin(width = 1, fill = "white") +
      geom_boxplot(alpha = 2, outlier.shape = NA, width= 0.3, aes(fill = new_data)) +
      geom_jitter(color = dataset$outlier,height = 0, width = 0.3) + # NA not plotted 
      ggstyle() +
      facet_wrap(~feature, nrow = nrow)+
      scale_fill_manual(values = bx_color)+
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 20),strip.text = element_text(size = 13)) +
      theme(axis.text.y = element_text(size = 20), axis.title.x.bottom = element_text(size = 20), axis.title.y.left = element_text(size = 20))+ 
      xlab(xlab)+
      ylab("log scaled mIHC score")
     # ylim(min(dataset$score),max) 
    
    if(nrow(stats) != 0){
      p<- p + 
        stat_pvalue_manual(stats, label = "p.adj.signif", y.position = stats$y.position,size = 8, tip.length = 0.01)+
        ggstyle()+
        ylim(min(dataset$score),max(stats$y.position)+0.05) +
        theme(legend.position="none")
        
    }
    else{
      p<- p + 
        ggstyle()+
        theme(legend.position="none")
    }
      
    fname<-paste0(folder_name,varb_name,"_",dtype,"_",organism,"_box_violin_plots.pdf")
    fname2<-paste0(folder_name,varb_name,"_",dtype,"_",organism, "_box_violin_plots.tiff")
    ggsave(p,filename = fname, width = width, height = height, scale=1, unit= "in", dpi=300)
    ggsave(p,filename = fname2, width = width, height = height, scale=1, unit= "in", dpi=300)
    
  }
  
  
  
  
  
  
}



library(ggplot2)



ggstyle <- function(font="Helvetica", scale=1) {
  fs <- function(x) x*scale # Dynamic font scaling
  ggplot2::theme(
    plot.title = ggplot2::element_text(family=font, size=fs(26), face="bold", color="#222222"),
    plot.subtitle = ggplot2::element_text(family=font, size=fs(18), margin=ggplot2::margin(0,0,5,0)),
    plot.caption = ggplot2::element_blank(),
    legend.position = "right",
    legend.text.align = 0,
    #  legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_text(family=font, size=fs(18), color="#222222"),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(family=font, size=fs(18), color="#222222"),
    axis.title =  ggplot2::element_text(family=font, size=fs(18), color="#222222"),
    axis.text = ggplot2::element_text(family=font, size=fs(18), color="#222222"),
    axis.text.x = ggplot2::element_text(size = fs(18),margin=ggplot2::margin(5, b=10)),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_line(color="#222222"),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    # strip.background = ggplot2::element_rect(fill="white"),
    strip.text = ggplot2::element_text(size=fs(15), hjust=0.5)
  )
}









