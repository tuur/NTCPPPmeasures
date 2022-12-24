# Plotting
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpmisc)
library("animation")

library(rlist)
#library(patchwork)
library(gridExtra)
library(scales)


setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# CITL(val) against 

prefix="/Users/aleeuw15/Desktop/Research/NTCP - uitkomstmaten/code/"
plot_dir="plots-24122022/"
#df <- readRDS(paste(prefix,'results_bs1_imp1_100steps_03112021.rds', collapse='', sep=''))
#df <- readRDS(paste(prefix,'results_bs1_imp1_100steps_27042022.rds', collapse='', sep=''))

df <- readRDS(paste(prefix,'results_bs1_imp1_100steps_02052022.rds', collapse='', sep=''))


dpi=300

# select all data where citl step != NA

metric_mapping = list(citl="Intercept", slope="Slope", auc="C (ROC)")
other_metrics_mapping = list(citl=c('slope','auc'),slope=c('citl', 'auc'),auc=c('citl', 'slope'))
print_metric_mapping = list(citl="CITL", slope="CS", auc="AUC")
print_model_mapping = list(xer2="Xer2+", xer3="Xer3+", dys2="Dys2+", dys3="Dys3+")

#final_patch = plot_layout(nrow = 4, byrow = FALSE)
plist_checks = list()
plist_ntcp = list()
plist_selection = list()

for (metric in c('citl','slope','auc')){
  for (model in c('xer2','xer3','dys2','dys3')){
    stepvar = paste(model,'_',metric,'_step', collapse='', sep='')
    xvar = paste('val_results.',model,'.',metric_mapping[[metric]], collapse='', sep='')
    selected_df = df %>% filter(!is.na(!!as.symbol(stepvar)))
    x_origin = mean(unlist((selected_df %>% filter(!!as.symbol(stepvar)==0))[[xvar]]))
    print(xvar)
   
    
    # PLOT 1: metric against the other metrics (should be horizontal lines...)
    yvar1 = paste('val_results.',model,'.',metric_mapping[[other_metrics_mapping[[metric]][[1]]]], collapse='', sep='')
    yvar2 = paste('val_results.',model,'.',metric_mapping[[other_metrics_mapping[[metric]][[2]]]], collapse='', sep='')
    plot_df <- gather(selected_df, key = measure, value = Rate, 
                      c(yvar1, yvar2))
    
#    p = ggplot(plot_df, aes(x=as.numeric(!!as.symbol(xvar)), y = as.numeric(Rate), group = measure, color=measure)) + 
    p = ggplot(plot_df, aes(x=as.numeric(!!as.symbol(xvar)), y = as.numeric(Rate), group = measure, color=measure, shape=measure)) + 
      geom_point(size=3)+
      stat_smooth(method = "loess") + # ,method.args = list(family = "symmetric"))+
      #stat_poly_eq(formula = y~x, parse = TRUE, show.legend=TRUE, aes(label =  stat(eq.label)), label.y = "middle", label.x = "middle") +
      geom_vline(xintercept = x_origin, linetype='dashed', color='red')+
      xlab(print_metric_mapping[[metric]])+
      #scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(-.25,1.5))+
      #scale_x_continuous(labels = scales::percent_format(accuracy = 1))+
                           ylab("") +ylim(-.1, 1.5)+
      theme_minimal(base_size = 22)
    if(metric == 'slope'){
      p = p + scale_x_continuous(trans='log2',limits=c(0.5, 2))
    }
    if(metric == 'auc'){
      p = p + xlim(.59, NA)
    }
    if(metric == 'citl'){
     p = p + xlim(-1, 1)
     p = p + ylab(print_model_mapping[[model]]) 
    }
    #show(p)
    #p = p + theme(legend.position =  c(0.5, .35)) #, legend.title = element_blank())
    
    p =  p + scale_colour_discrete(name="", breaks=c(yvar1, yvar2),
                                 labels=c(print_metric_mapping[[other_metrics_mapping[[metric]][[1]]]],print_metric_mapping[[other_metrics_mapping[[metric]][[2]]]])) # )
    p = p + theme(legend.position = c(0.8, .98))
    plist_checks = list.append(plist_checks, p)

    # PLOT 2: metric against changes in selection
    yvar1 = 'mbs_results.n_pt_to_rt'
    yvar2 = 'mbs_results.n_rt_to_pt'
    yvar3 = 'mbs_results.n_preselected'
    yvar4 = 'mbs_results.n_pt'
    yvar5 = 'mbs_results.n_rt'
    plot_df <- gather(selected_df, key = measure, value = Rate, 
                      c(yvar1, yvar2, yvar3, yvar4, yvar5))
    p = ggplot(plot_df, aes(x=as.numeric(!!as.symbol(xvar)), y = as.numeric(Rate), group = measure, shape = measure, color=measure)) + 
      geom_point(size=3)+
      stat_smooth(method = "loess") + # ,method.args = list(family = "symmetric"))+
      #stat_poly_eq(formula = y~x, parse = TRUE, show.legend=TRUE, aes(label =  stat(eq.label)), label.y = "middle", label.x = "middle") +
      geom_vline(xintercept = x_origin, linetype='dashed', color='red')+
      xlab(paste(model, print_metric_mapping[[metric]]))+
      scale_y_continuous(limits=c(0, 300))+  xlab("")+
      ylab("") + theme(legend.position = "none") + 
      theme_minimal(base_size = 22)
    if(metric == 'slope'){
      p = p + scale_x_continuous(trans='log2',limits=c(0.5, 2))
      p = p + theme(legend.position = "none")
    }
    if(metric == 'citl'){
      p = p + xlim(-1, 1)
      p = p + theme(legend.position = "none")
      p = p + ylab(print_model_mapping[[model]]) 
      
    }
    if(metric == 'auc'){
      p = p + theme(legend.position = "none")
      p = p + scale_x_continuous(labels = function(x) sprintf("%.2f", x))
      p = p + xlim(.59, NA)
    }
    #if (model =='dys3'){
    #  
    #}
    p = p + xlab(print_metric_mapping[[metric]])
    #show(p)    
    plist_selection = list.append(plist_selection, p)
    
    
    # >>>>>>
    # PLOT 3: metric agains changes in delta-NTCP
    yvar1 = paste('mbs_results.mean_ntcp_rt_',model, collapse='', sep='')
    yvar2 = paste('mbs_results.mean_ntcp_pt_',model, collapse='', sep='')
    yvar3 = paste('mbs_results.mean_delta_ntcp_',model, collapse='', sep='')
    yvar4 = paste('mbs_results.mean_delta_lp_',model, collapse='', sep='')
    
    #plot_df <- gather(selected_df, key = measure, value = Rate, 
    #             c(yvar1, yvar2, yvar3, yvar4))
    p = ggplot(selected_df, aes(x=as.numeric(!!as.symbol(xvar)))) +
      stat_smooth(aes(y=as.numeric(!!as.symbol(yvar1))), size=1, col='blue', shape=1)+
      geom_point(aes(y=as.numeric(!!as.symbol(yvar1))), size=1, col='blue', shape=1)+
      stat_smooth(aes(y=as.numeric(!!as.symbol(yvar2))), size=1, col='green', shape=2)+
      geom_point(aes(y=as.numeric(!!as.symbol(yvar2))), size=1, col='green', shape=2)+
      stat_smooth(aes(y=as.numeric(!!as.symbol(yvar3))), size=1, col='red', shape=3)+
      geom_point(aes(y=as.numeric(!!as.symbol(yvar3))), size=1, col='red', shape=3)+
      stat_smooth(aes(y=0.1*as.numeric(!!as.symbol(yvar4))), size=1, col='orange', shape=4)+
      geom_point(aes(y=0.1*as.numeric(!!as.symbol(yvar4))), size=1, col='orange', shape=4)+
      #stat_smooth(aes(y=as.symbol(yvar1)), method = "loess") + # ,method.args = list(family = "symmetric"))+
      geom_vline(xintercept = x_origin, linetype='dashed', color='red')+
      xlab("")+
      scale_y_continuous(labels = function(x) sprintf("%.1f", x))+
#                         sec.axis = sec_axis( trans=~.*10, name="", labels = function(x) sprintf("%.1f", x)))+
      scale_x_continuous(labels = function(x) sprintf("%.1f", x))+
      ylab("")  + 
      theme_minimal(base_size = 22)+
      theme(legend.position = "none")
    if(metric == 'slope'){
      p = p + scale_x_continuous(trans='log2',limits=c(0.5, 2))
      p = p + theme(legend.position = "none")
    }
    if(metric == 'citl'){
      p = p + xlim(-1, 1)
      p = p + theme(legend.position = "none")
      #p = p + ggtitle(print_model_mapping[[model]]) 
      p = p + ylab(print_model_mapping[[model]]) 
      
    }
    if(metric == 'auc'){
      p = p + scale_x_continuous(labels = function(x) sprintf("%.2f", x), limits=c(.59,NA))
    }
    p = p + ylim(-.1,.65)
    p = p + xlab(print_metric_mapping[[metric]])
    
    #if (model =='xer2'){
    #  p = p + ylim(-.1,.65)
    #}
    #if (model =='xer3'){
    #  p = p + ylim(-.1,.25)
    #}
    #if (model =='dys2'){
    #  p = p + ylim(-.1,.4)
    #}
    #if (model =='dys3'){
    #  p = p + ylim(-.1,.2)
    #  p = p + xlab(print_metric_mapping[[metric]])
    #}
    show(p)
    # <<<<<<<
    plist_ntcp = list.append(plist_ntcp, p)

  }
  #if (exists("final_patch")){
  #  final_patch = final_patch + model_patch
  #}else{
  #  final_patch = model_path
  #}
  #dev.off()
}

#ggsave('output.pdf', final_patch)

# ============================================================= 
# ============ PLOT SET 1: CHECKS, NTCP, SELECTION ============ 
# ============================================================= 

size_factor = 4

# checks
#pdf(file = paste(plot_dir,'output_checks.pdf'), width=10*size_factor, height=6*size_factor) #, width = 1000, height = 297, units = "mm")
#png(file = paste(plot_dir,'output_checks.png'), width=10*size_factor*dpi, height=6*size_factor*dpi, res=dpi) #, width = 1000, height = 297, units = "mm")
png(file = paste(plot_dir,'output_checks.png'), width=4*size_factor*dpi, height=4*size_factor*dpi, res=dpi) #, width = 1000, height = 297, units = "mm")
#do.call("grid.arrange", c(matrix(plist_checks, ncol=4, byrow = TRUE), nrow=4, ncol=3))
do.call("grid.arrange", c(matrix(plist_checks, ncol=4, byrow = TRUE), nrow=4, ncol=3))
dev.off()

# ntcp
#pdf(file = paste(plot_dir,'output_ntcp.pdf'), width=4*size_factor, height=4*size_factor) #, width = 1000, height = 297, units = "mm")
png(file = paste(plot_dir,'output_ntcp.png'), width=4*size_factor*dpi, height=4*size_factor*dpi, res=dpi) #, width = 1000, height = 297, units = "mm")s
do.call("grid.arrange", c(matrix(plist_ntcp, ncol=4, byrow = TRUE), nrow=4, ncol=3))
dev.off()

png(file = paste(plot_dir,'output_ntcp_flipped.png'), width=4*size_factor*dpi, height=4*size_factor*dpi, res=dpi) #, width = 1000, height = 297, units = "mm")s
do.call("grid.arrange", c(matrix(plist_ntcp, ncol=3, byrow = FALSE), nrow=3, ncol=4))
dev.off()

# selection
#pdf(file = paste(plot_dir,'output_selection.pdf'), width=4*size_factor, height=4*size_factor) #, width = 1000, height = 297, units = "mm")
png(file = paste(plot_dir,'output_selection.png'), width=4*size_factor*dpi, height=4*size_factor*dpi, res=dpi) #, width = 1000, height = 297, units = "mm")
do.call("grid.arrange", c(matrix(plist_selection, ncol=4, byrow = TRUE), nrow=4, ncol=3))
dev.off()

# =================================================== 
# ============ PLOT SET 2: DISTRIBUTIONS ============ 
# =================================================== 
library(ggplot2)
library(ggpubr)
library(ggridges)


metric_ranges = list(citl=c(-1,1), slope=c(0.5, 2), auc=c(0.59, 1))

plist_probs = list(citl=list(),slope=list(),auc=list())
decisionthresholds = list(xer2=.1, xer3=.05, dys2=.1, dys3=.05)

for (metric in c('citl','slope','auc')){
  for (model in c('xer2','xer3','dys2','dys3')){
    stepvar = paste(model,'_',metric,'_step', collapse='', sep='')
    xvar = paste('val_results.',model,'.',metric_mapping[[metric]], collapse='', sep='')

    selected_df = df %>% filter(!is.na(!!as.symbol(stepvar)))
    x_origin = mean(unlist((selected_df %>% filter(!!as.symbol(stepvar)==0))[[xvar]]))
    
    # Select in ranges CITL (-1,1), CS (0.5, 2), and AUC (0.6+)
    selected_df = selected_df[selected_df[[xvar]] >= metric_ranges[[metric]][1],]
    selected_df = selected_df[selected_df[[xvar]] <= metric_ranges[[metric]][2],]
    
    oldyticklabels <- selected_df[[xvar]]
    selected_df[,xvar] <- sapply(selected_df[,xvar] , as.character)
    selected_df[,xvar] <- sapply(selected_df[,xvar] , factor)
    
    
    
    yticklabels = list(citl=sapply(oldyticklabels, round, digits = 0), slope = sapply(oldyticklabels, round, digits = 1), auc=sapply(oldyticklabels, round, digits = 2))

    for (m in c('citl','slope','auc')){
      for (i in 1:length(yticklabels[[m]])){
        if (i != length(yticklabels[[m]]) %/% 1.333 && i != length(yticklabels[[m]]) %/% 4 && i != length(yticklabels[[m]]) %/% 2 && i!= 1 && i!=length(yticklabels[[m]] )){
          yticklabels[[m]][[i]] = ""
        }
      }
    }

    #yticklabels <- 
    
    #selected_df$as.symbol(xvar) <- as.factor(selected_df$as.symbol(xvar)) #transform(selected_df, metric = as.factor(as.symbol(metric)))

    
    rt_probs_var = paste('mbs_results.','rt_probs','.',model, collapse='', sep='')
    selected_df_rt_probs  = tidyr::unnest(selected_df, cols = rt_probs_var)
    p = ggplot(selected_df_rt_probs, aes(x = .data[[rt_probs_var]], y=.data[[xvar]])) +
      geom_density_ridges(
        jittered_points = TRUE, fill='blue',
        position = position_points_jitter(width = 0.05, height = 0),
        point_shape = '|', point_size = 1, point_alpha = 1, alpha = 0.6,
      )+
      ylab(print_metric_mapping[[metric]]) + 
      xlab(paste(print_model_mapping[[model]],'RT'))+ 
#      scale_fill_manual(values = rep(c("blue"), length(selected_df[[xvar]]))) +
      theme(legend.position = "none")+
      xlim(0,1)+
      scale_y_discrete(labels= rev(yticklabels[[metric]]), limits=rev)+
      theme_minimal(base_size = 22)+ coord_flip()
    #show(p)
    plist_probs[[metric]] = list.append(plist_probs[[metric]], p)
    
    pt_probs_var = paste('mbs_results.','pt_probs','.',model, collapse='', sep='')
    selected_df_pt_probs  = tidyr::unnest(selected_df, cols = pt_probs_var)
    p = ggplot(selected_df_pt_probs, aes(x = .data[[pt_probs_var]], y=.data[[xvar]])) +
      geom_density_ridges(
        jittered_points = TRUE, fill='green',
        position = position_points_jitter(width = 0.05, height = 0),
        point_shape = '|', point_size = 1, point_alpha = 1, alpha = 0.7,
      )+
      ylab(print_metric_mapping[[metric]]) + 
      xlab(paste(print_model_mapping[[model]],'PT'))+ 
      #scale_fill_manual(values = rep(c("green"), length(selected_df[[xvar]]))) +
      theme(legend.position = "none")+
      xlim(0,1)+
      scale_y_discrete(labels= rev(yticklabels[[metric]]), limits=rev)+
      theme_minimal(base_size = 22)+ coord_flip()
    #show(p)
    plist_probs[[metric]] = list.append(plist_probs[[metric]], p)
    
    delta_probs_var = paste('mbs_results.','delta_ntcp','.',model, collapse='', sep='')
    selected_df_delta_probs = tidyr::unnest(selected_df, cols = delta_probs_var)
    p = ggplot(selected_df_delta_probs, aes(x = .data[[delta_probs_var]], y=.data[[xvar]])) +
      geom_density_ridges(
        jittered_points = TRUE, fill='red',
        position = position_points_jitter(width = 0.05, height = 0),
        point_shape = '|', point_size = 1, point_alpha = 1, alpha = 0.7,
      )+geom_vline(xintercept = decisionthresholds[[model]], linetype='solid', color='orange')+
      ylab(print_metric_mapping[[metric]]) + 
      xlab(paste(print_model_mapping[[model]],'Delta-NTCP'))+ 
      #scale_fill_manual(values = rep(c("red"), length(selected_df[[xvar]]))) +
      theme(legend.position = "none")+
      xlim(-.1,.5)+
      scale_y_discrete(labels= rev(yticklabels[[metric]]), limits=rev)+
      theme_minimal(base_size = 22)+ coord_flip()

    #show(p)
    plist_probs[[metric]] = list.append(plist_probs[[metric]], p)    
     }
}

size_factor = 2

for (metric in c('citl','slope','auc')){
  # Prob distributions per metric
  #pdf(file = paste(plot_dir,'output_distributions',metric,'.pdf'), width=7*size_factor, height=8*size_factor) #, width = 1000, height = 297, units = "mm")
  png(file = paste(plot_dir,'output_distributions',metric,'.png'), width=7*size_factor*dpi, height=8*size_factor*dpi, res=dpi) #, width = 1000, height = 297, units = "mm")
  do.call("grid.arrange", c(plist_probs[[metric]],ncol=3))
  dev.off()
}



# ========================================================== 
# ============ PLOT SET 3: CHANGE IN DELTA NTCP ============ 
# ========================================================== 
library(Metrics)

# DEVIATION
DEVIATION = 0.2

plotlist = list()
for (model in c('xer2','xer3','dys2','dys3')){
  for (metric in c('citl','slope','auc')){
    stepvar = paste(model,'_',metric,'_step', collapse='', sep='')
    xvar = paste('val_results.',model,'.',metric_mapping[[metric]], collapse='', sep='')
    delta_probs_var = paste('mbs_results.','delta_ntcp','.',model, collapse='', sep='')

    
    selected_df = df %>% filter(!is.na(!!as.symbol(stepvar)))
    selected_df = selected_df[rev(order(unlist(selected_df[[xvar]]))),]
    
    
    # Original model dNTCP
    original_model_results = selected_df %>% filter(!!as.symbol(stepvar)==0)
    original_delta_ntcps = original_model_results[[delta_probs_var]][[1]]
    original_metric_value = original_model_results[[xvar]][[1]]
    original_pt = which(original_delta_ntcps >= decisionthresholds[[model]])
        
    
    used_deviation = ifelse(metric=='auc',DEVIATION/10, DEVIATION)
    
    selected_df = selected_df[selected_df[[xvar]] >= original_metric_value - used_deviation,]
    selected_df = selected_df[selected_df[[xvar]] <= original_metric_value + used_deviation,]
    
    selected_df = selected_df[selected_df[[xvar]] >= metric_ranges[[metric]][1],]
    selected_df = selected_df[selected_df[[xvar]] <= metric_ranges[[metric]][2],]

    
    # lower bound dNTCPs
    lower_model_results = tail(selected_df, n=1)
    lower_delta_ntcps = lower_model_results[[delta_probs_var]][[1]]
    lower_metric_value = lower_model_results[[xvar]][[1]]
    lower_df = data.frame(lower_delta_ntcps, original_delta_ntcps)
    lower_rmse = rmse(lower_delta_ntcps, original_delta_ntcps)
    lower_pt = which(lower_delta_ntcps >= decisionthresholds[[model]])

    to_pt = length(setdiff(lower_pt,original_pt)) # are now pt but were not pt
    to_rt = length(setdiff(original_pt, lower_pt)) # were pt but are no longer pt
    #toptitle = ifelse(metric=='citl', print_model_mapping[[model]], "")
    lower_plot = ggplot(lower_df, aes(x=lower_delta_ntcps, y=original_delta_ntcps)) +
      geom_point() +
      geom_abline(intercept = 0, slope = 1, linetype='dotted')+
      theme_minimal(base_size = 25)+ 
#      xlab(sprintf(paste('\u0394NTCP','(',print_metric_mapping[[metric]],'=',as.character(round(lower_metric_value,2)),')')))+
      xlab(sprintf(paste('\u0394NTCP','(',print_metric_mapping[[metric]],'=',as.character(sprintf("%.2f", original_metric_value-used_deviation)),')')))+
      ylab(sprintf(paste('\u0394NTCP','(',print_metric_mapping[[metric]],'=',as.character(sprintf("%.2f", original_metric_value)),')')))+
      xlim(-.25,.25)+
      ylim(-.25,.25)+
      geom_vline(xintercept = 0, linetype='solid', color='black')+
      geom_hline(yintercept = 0, linetype='solid', color='black')+
      geom_vline(xintercept = decisionthresholds[[model]], linetype='solid', color='red')+
      geom_hline(yintercept = decisionthresholds[[model]], linetype='solid', color='red')+
      annotate(geom="text", x=-.1, y=.18, label=paste("+RT:",as.character(round(to_rt,1))),
                                 color="black",size=9)+
      annotate(geom="text", y=-.1, x=decisionthresholds[[model]]+.08, label=paste("+PT:",as.character(round(to_pt,1))),
               color="black",size=9)+
      annotate(geom="text", y=-.2, x=-.15, label=paste('RMSE:',as.character(round(lower_rmse,3))),
               color="black",size=9)
    #show(lower_plot)
    if (metric =='citl'){
      lower_plot = lower_plot + ggtitle(print_model_mapping[[model]])
    }
    
    
    plotlist = list.append(plotlist, lower_plot)
    
    
    # upper bound dNTCPs
    upper_model_results = head(selected_df, n=1)
    upper_delta_ntcps = upper_model_results[[delta_probs_var]][[1]]
    upper_metric_value = upper_model_results[[xvar]][[1]]
    upper_df = data.frame(upper_delta_ntcps, original_delta_ntcps)
    upper_rmse = rmse(upper_delta_ntcps, original_delta_ntcps)
    upper_pt = which(upper_delta_ntcps >= decisionthresholds[[model]])
    
    to_pt = length(setdiff(upper_pt,original_pt)) # are now pt but were not pt
    to_rt = length(setdiff(original_pt, upper_pt)) # were pt but are no longer pt
    
    upper_plot = ggplot(upper_df, aes(x=upper_delta_ntcps, y=original_delta_ntcps)) +
      geom_point() +
      geom_abline(intercept = 0, slope = 1, linetype='dotted')+
      theme_minimal(base_size = 25)+
      #xlab(sprintf(paste('\u0394NTCP','(',print_metric_mapping[[metric]],'=',as.character(round(upper_metric_value,2)),')')))+
      xlab(sprintf(paste('\u0394NTCP','(',print_metric_mapping[[metric]],'=',as.character(sprintf("%.2f", original_metric_value+used_deviation)),')')))+
      
      ylab(sprintf(paste('\u0394NTCP','(',print_metric_mapping[[metric]],'=',as.character(sprintf("%.2f", original_metric_value)),')')))+
      xlim(-.25,.25)+
      ylim(-.25,.25)+
      geom_vline(xintercept = 0, linetype='solid', color='black')+
      geom_hline(yintercept = 0, linetype='solid', color='black')+
      geom_vline(xintercept = decisionthresholds[[model]], linetype='solid', color='red')+
      geom_hline(yintercept = decisionthresholds[[model]], linetype='solid', color='red')+
      annotate(geom="text", x=-.1, y=.18, label=paste("+RT:",as.character(round(to_rt,1))),
               color="black",size=9)+
      annotate(geom="text", y=-.1, x=decisionthresholds[[model]]+.08, label=paste("+PT:",as.character(round(to_pt,1))),
               color="black",size=9)+
      annotate(geom="text", y=-.2, x=-.15, label=paste('RMSE:',as.character(round(upper_rmse,3))),
               color="black",size=9)
    if (metric!='auc'){
      plotlist = list.append(plotlist, upper_plot)
    }
  }
}

size_factor = 6
#cairo_pdf(file = paste(plot_dir,'output_deviation_plots.pdf'), width=4*size_factor, height=5*size_factor) #, width = 1000, height = 297, units = "mm")
png(file = paste(plot_dir,'output_deviation_plots.png'), width=4*size_factor*dpi, height=5*size_factor*dpi,res=dpi) #, width = 1000, height = 297, units = "mm")
#do.call("grid.arrange", c(plotlist,ncol=4))
do.call("grid.arrange", c(matrix(plotlist, ncol=5, byrow = TRUE), ncol=4))
dev.off()


# ========================================================== 
# ============ PLOT LOGISTIC CURVE ========================= 
# ========================================================== 

n=100

X = seq(-7, 7, by=20/n)
Y = 1 / (1+exp(-X))
Yd = exp(-X) / ((1+exp(-X))**2)
logistic_df = data.frame(LP=X,P=Y, Pd=Yd)
p = ggplot(data = logistic_df)+
  geom_line(aes(x=LP, y=P, group=1))+
  geom_line(aes(x=LP, y=Pd, group=1), linetype='dashed', color='red')+
  geom_point(aes(x=1.39,y=.8),colour="red", size=1.5)+
  geom_point(aes(x=-1.39,y=.2),colour="red", size=1.5)+
  geom_point(aes(x=1.39,y=.16),colour="red", size=1.5)+
  geom_point(aes(x=-1.39,y=.16),colour="red", size=1.5)+
  theme_minimal(base_size = 18)+
  geom_segment(x=0.4,y=1/(1+exp(-0.4)), xend=0.4, yend=(1/(1+exp(0.4))), linetype='dotted', color='red')+
  geom_segment(x=-0.4,y=1/(1+exp(0.4)), xend=0.4, yend=(1/(1+exp(0.4))), color='orange', linetype='solid')+
  xlab("Linear predictor")+
  ylab("NTCP")+
  annotate(geom="text", y=.37, x=.2, label=sprintf("\u0394LP"),
           color="orange",size=7)+
  annotate(geom="text", y=.47, x=1.55, label=sprintf("\u0394NTCP"),
           color="red",size=7)+
  geom_segment(x=2.5,y=1/(1+exp(-2.5)), xend=3.3, yend=(1/(1+exp(-2.5))), color='orange', linetype='solid')+
  geom_segment(x=3.3,y=1/(1+exp(-3.3)), xend=3.3, yend=(1/(1+exp(-2.5))), linetype='dotted', color='red')+
  annotate(geom="text", y=.895, x=3, label=sprintf("\u0394LP"),
         color="orange",size=7)+
  annotate(geom="text", y=.95, x=4.5, label=sprintf("\u0394NTCP"),
           color="red",size=7)


ggsave(paste(plot_dir,'Fig X - Exp Box20n.png'), plot=p, dpi = 300, width = 30, height = 20, units = "cm", bg = "white")
