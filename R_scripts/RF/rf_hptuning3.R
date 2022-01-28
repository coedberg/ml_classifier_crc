
set.seed(42)

##### Hyper Parameter Tuning ###################################################
#Set up tune grids for each model type and a function for data splitting which is seed dependent

fitControl <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 10, 
                           classProbs = TRUE,
                           savePredictions = TRUE)



### RF
#what was the best tunes in nzv_df rf model in the feature selection? 

rf_besttune_feature <-data.frame(mtry = 0)
for(i in 1:10) {                                   
  new <- rep(i, ncol(rf_besttune_feature))                       
  rf_besttune_feature[i,] <- as.data.frame(t(paste0(rf_nzv_evalres_comb[[i]][["bestTune"]])))
  rf_besttune_feature[nrow(rf_besttune_feature) + 1, ] <- new                   
}

rf_besttune_feature$count <- 1
rf_besttune_feature <- rf_besttune_feature[1:10,]

rf_besttune_feature <- aggregate(rf_besttune_feature$count,
                                by = list(rf_besttune_feature$mtry), FUN = sum) 
colnames(rf_besttune_feature) <- c("mtry", "count")

view(rf_besttune_feature)


#select grid corresponding to the df above
dim(nzv_df)
sqrt(183)

rfGrid_rough <- expand.grid(mtry = seq(25,183, by = 20))

rf_parameter_results_rough <- function(seed){
  
  nzv_df_inTraining <- createDataPartition(nzv_df$dx, p = .8, list = FALSE)
  nzv_df_training <- nzv_df[ nzv_df_inTraining,]
  nzv_df_testing  <- nzv_df[-nzv_df_inTraining,]
  
  train(dx ~ ., data = nzv_df_training, 
        method = "rf", 
        trControl = fitControl, 
        verbose = FALSE,
        tuneGrid = rfGrid_rough,
        metric = "Accuracy")
}




##### Run model

plan("multisession", workers = 6)

# 1 seeds = 242 s  => 1.5 h for 100 seeds
tic() 
rf_parameter_results_comb_rough <- future_map(1:100, rf_parameter_results_rough, .options = furrr_options(seed=TRUE))
toc()

plan("sequential")



##### METRICS


rf_best_tune <-data.frame(mtry = 0)
for(i in 1:100) {                                   # Head of for-loop
  new <- rep(i, ncol(rf_best_tune))                       # Create new row
  rf_best_tune[i,] <- as.data.frame(t(paste0(rf_parameter_results_comb_rough[[i]][["bestTune"]])))
  rf_best_tune[nrow(rf_best_tune) + 1, ] <- new                   # Append new row
}


rf_best_tune$count <- 1
rf_best_tune <- rf_best_tune[1:100,]

rf_best_tune <- aggregate(rf_best_tune$count,
                           by = list(rf_best_tune$mtry), FUN = sum) 
colnames(rf_best_tune) <- c("mtry", "count")

view(rf_best_tune)

rf_hp_metrics_rough <- combine_hp_performance(rf_parameter_results_comb_rough)
rf_mtry_rough <- plot_hp_performance(rf_hp_metrics_rough$dat, mtry, Accuracy) +
  geom_errorbar(ggplot2::aes(
                ymin = .data$ymin_metric,
                ymax = .data$ymax_metric),
     width = .001, color = "grey")+
  geom_line( color="black") +
  geom_point(shape=21, color="black", fill="black", size=1)+
  theme_classic()+
  #scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
  #              labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  scale_y_continuous(limits = c(0.7, 0.75))+
  labs(x = "Features", y = "Mean Accuracy")


rf_mtry_rough



#### Setup new grid and rerun for finetuning

rfGrid_fine <- expand.grid(mtry = seq(10,34, by = 2))

rf_parameter_results_fine <- function(seed){
  
  nzv_df_inTraining <- createDataPartition(nzv_df$dx, p = .8, list = FALSE)
  nzv_df_training <- nzv_df[ nzv_df_inTraining,]
  nzv_df_testing  <- nzv_df[-nzv_df_inTraining,]
  
  train(dx ~ ., data = nzv_df_training, 
        method = "rf", 
        trControl = fitControl, 
        verbose = FALSE,
        tuneGrid = rfGrid_fine,
        metric = "Accuracy")
}

plan("multisession", workers = 8) #2 seeds = 490 s => 100 seeds = 7 h
tic() 
rf_parameter_results_comb_fine <- future_map(1:100, rf_parameter_results_fine, .options = furrr_options(seed=TRUE))
toc()
plan("sequential")




rf_best_tune <-data.frame(mtry = 0)
for(i in 1:100) {                                   # Head of for-loop
  new <- rep(i, ncol(rf_best_tune))                       # Create new row
  rf_best_tune[i,] <- as.data.frame(t(paste0(rf_parameter_results_comb_fine[[i]][["bestTune"]])))
  rf_best_tune[nrow(rf_best_tune) + 1, ] <- new                   # Append new row
}


rf_best_tune$count <- 1
rf_best_tune <- rf_best_tune[1:100,]

rf_best_tune <- aggregate(rf_best_tune$count,
                          by = list(rf_best_tune$mtry), FUN = sum) 
colnames(rf_best_tune) <- c("mtry", "count")

view(rf_best_tune)

rf_hp_metrics_fine <- combine_hp_performance(rf_parameter_results_comb_fine)
rf_mtry_fine <- plot_hp_performance(rf_hp_metrics_fine$dat, mtry, Accuracy) +
  geom_errorbar(ggplot2::aes(
    ymin = .data$ymin_metric,
    ymax = .data$ymax_metric),
    width = .001, color = "grey")+
  geom_line( color="black") +
  geom_point(shape=21, color="black", fill="black", size=1)+
  theme_classic()+
  scale_y_continuous(limits = c(0.7, 0.75))+
  labs(x = "Features", y = "Mean Accuracy")


rf_mtry_fine


rf_mtry_tune <- plot_grid(rf_mtry_rough, rf_mtry_fine, labels = c("A", "B"))


rf_mtry_tune
ggsave(plot = rf_mtry_tune, filename = "rf_mtry_tune.jpeg",
       path = "C:/Users/Berg/Desktop/Thesis/Results/rf/",
       dpi = 600, width = 20, height = 10, units = "cm")


####
rfGrid_fine2 <- expand.grid(mtry = c(20,22))

rf_parameter_results_fine2 <- function(seed){
  
  nzv_df_inTraining <- createDataPartition(nzv_df$dx, p = .8, list = FALSE)
  nzv_df_training <- nzv_df[ nzv_df_inTraining,]
  nzv_df_testing  <- nzv_df[-nzv_df_inTraining,]
  
  train(dx ~ ., data = nzv_df_training, 
        method = "rf", 
        trControl = fitControl, 
        verbose = FALSE,
        tuneGrid = rfGrid_fine2)
}

plan("multisession", workers = 8)
tic() 
rf_parameter_results_comb_fine2 <- future_map(1:100, rf_parameter_results_fine2, .options = furrr_options(seed=TRUE))
toc()

plan("sequential")



rf_best_tune <-data.frame(mtry = 0)
for(i in 1:100) {                                   # Head of for-loop
  new <- rep(i, ncol(rf_best_tune))                       # Create new row
  rf_best_tune[i,] <- as.data.frame(t(paste0(rf_parameter_results_comb_fine2[[i]][["bestTune"]])))
  rf_best_tune[nrow(rf_best_tune) + 1, ] <- new                   # Append new row
}


rf_best_tune$count <- 1
rf_best_tune <- rf_best_tune[1:100,]

rf_best_tune <- aggregate(rf_best_tune$count,
                          by = list(rf_best_tune$mtry), FUN = sum) 
colnames(rf_best_tune) <- c("mtry", "count")

view(rf_best_tune)


################################################################################


rfGrid <- expand.grid(mtry = 22)

rf_final <- function(seed){
  
  nzv_df_inTraining <- createDataPartition(nzv_df$dx, p = .8, list = FALSE)
  nzv_df_training <- nzv_df[ nzv_df_inTraining,]
  nzv_df_testing  <- nzv_df[-nzv_df_inTraining,]
  
  train(dx ~ ., data = nzv_df_training, 
        method = "rf", 
        trControl = fitControl, 
        verbose = FALSE,
        tuneGrid = rfGrid)
}

plan("multisession", workers = 8)
tic() #1 seed = 33 s
rf_final_comb <- future_map(1:100, rf_final, .options = furrr_options(seed=TRUE))
toc()

plan("sequential")



models <- c(rf_final_comb)
names <- c("Final RF model")
evalm(list1 = models, 
      gnames = names,
      silent = FALSE,
      showplots = FALSE)

names <- c("RF: AUC = 0.76")

model_rf_final <- evalm(list1 = models, 
                       gnames = names, 
                       #title = "Random Forest", 
                       cols = NULL,
                       silent = TRUE, 
                       rlinethick = 0.7, 
                       fsize = 12.5,
                       dlinecol = "grey", 
                       dlinethick = 0.75, 
                       bins = 1000, 
                       optimise = "INF",
                       percent = 95, 
                       showplots = FALSE, 
                       positive = NULL, 
                       plots = c("prg","pr", "r", "cc"))


rf_final <- model_rf_final$roc + 
  scale_color_manual(values= c("#E41A1C", "#377EB8", "#4DAF4A", "#FF7F00")) + 
  theme_classic()+
  theme(legend.position = c(0.65, 0.22),
        legend.text = element_text(size=8), 
        legend.justification = "left",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5)) 

rf_final


ggsave(plot = rf_final, filename = "rf_final.jpeg",
       path = "C:/Users/Berg/Desktop/Thesis/Results/RF/",
       dpi = 600, width = 15, height = 10, units = "cm")



final_comparison <- rbind(model_rf_final[["roc"]][["data"]], model_xgb_final[["roc"]][["data"]], model_svm_final[["roc"]][["data"]])

final_plot <- ggplot(final_comparison, aes(x = FPR, y = SENS, color = Group)) + 
  geom_line(size = 0.7) +
  geom_abline(intercept = 0, slope = 1, colour = "grey", linetype = 1,
              size = 0.75) +
  coord_equal() +
  theme_bw() +
  xlab('False positive rate') +
  ylab('True positive rate') +
  #ggtitle("Random Forest")+
  scale_color_manual(values= c("#E41A1C","#377EB8", "#4DAF4A", "#FF7F00")) + 
  theme_classic()+
  theme(legend.position = c(0.65, 0.22),
        legend.text = element_text(size=8), 
        legend.justification = "left",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))


ggsave(plot = final_plot, filename = "final_model_ROC.jpeg",
       path = "C:/Users/Berg/Desktop/Thesis/Results/",
       dpi = 600, width = 15, height = 10, units = "cm")



models <- c(rf_final_comb)
names <- c("Final RF model")

rf_final_comb_stats <- evalm(list1 = models, 
                                  gnames = names,
                                  silent = FALSE,
                                  showplots = TRUE,
                                  positive = "cancer")

stats <- as.data.frame(rf_final_comb_stats[["stdres"]][["Final RF model"]])


TP <- 30
FP <- 6
TN <- 113
FN <- 49

accuracy = (TP+TN)/(TP+TN+FP+FN)

stats["accuracy",] <- c(accuracy, "NA" )

stats <- as.data.frame(t(stats))

stats <- stats %>% 
  select(-c("MCC", "Informedness", "FPR", "F1", "AUC-PR", "AUC-PRG"))


rf_stats_final <- stats
write_xlsx(
  rf_stats_final,
  path = "C:/Users/Berg/Desktop/Thesis/Results/RF/rf_stats_final.xlsx",
  col_names = TRUE,
  format_headers = TRUE,
  use_zip64 = FALSE
)






##### Feature importance

names <- varImp(rf_final_comb[[1]], scale = TRUE)$importance %>% 
  rownames_to_column() %>%
  select(-c(Overall))

rf_imp <- as.data.frame(1:182)
for (i in 1:100){
  test <- varImp(rf_final_comb[[i]], scale = TRUE)
  rf_imp[,i+1] <- test[["importance"]][["Overall"]]
}

rf_imp <- rf_imp[,-1]
#colnames(rf_imp) <- c("cv1", "cv2", "cv3","cv4","cv5","cv6","cv7","cv8","cv9","cv10")

rf_imp$row_mean <- apply(rf_imp[,1:100], 1, mean)

rf_imp$genus <- names$rowname


rf_imp_top10 <- rf_imp %>% top_n(rf_imp$row_mean, n = 10) %>% 
  select(-c(row_mean)) %>% 
  pivot_longer(-genus, names_to="CV", values_to="imp") %>% 
  mutate(imp = as.double(imp),
         genus = fct_reorder(genus, imp)) %>% 
  arrange(desc(imp))

rf_imp_plot <- ggplot(rf_imp_top10, aes(x = imp, y=genus, color = genus))+
  geom_jitter(position = position_jitterdodge(),
              alpha = 0.2)+
  stat_summary(fun.data = median_hilow, fun.args = list(conf.int=0.5),
               geom="point",
               position = position_dodge(width=0.8),
               color="black")+
  theme_classic()+
  labs(x= "Feature Importance (%)", 
       y=NULL)+
  theme(legend.position = "none")

ggsave(plot = rf_imp_plot, filename = "rf_feature_importance_plot.jpeg",
       path = "C:/Users/Berg/Desktop/Thesis/Results/RF/",
       dpi = 600, width = 15, height = 8, units = "cm")










plot(rf_final_comb[[65]]$finalModel)















plot_final_error <- as.ggplot(plot_final_error,
                              scale = 1, hjust = 0, vjust = 0, angle = 0)





plot(plot_final_error)




ggplot(rf_final_comb[[65]]$finalModel, )






library(rpart.plot)

rf_final_comb[[3]]$finalModel


rpart(dx ~ . ,data = nzv_df_inTraining)

rpart.plot(rf_final_comb[[3]]$finalModel$forest)































library(dplyr)
library(ggraph)
library(igraph)

tree_func <- function(final_model, 
                      tree_num) {
  
  # get tree by index
  tree <- randomForest::getTree(final_model, 
                                k = tree_num, 
                                labelVar = TRUE) %>%
    tibble::rownames_to_column() %>%
    # make leaf split points to NA, so the 0s won't get plotted
    mutate(`split point` = ifelse(is.na(prediction), `split point`, NA))
  
  # prepare data frame for graph
  graph_frame <- data.frame(from = rep(tree$rowname, 2),
                            to = c(tree$`left daughter`, tree$`right daughter`))
  
  # convert to graph and delete the last node that we don't want to plot
  graph <- graph_from_data_frame(graph_frame) %>%
    delete_vertices("0")
  
  # set node labels
  V(graph)$node_label <- gsub("_", " ", as.character(tree$`split var`))
  V(graph)$leaf_label <- as.character(tree$prediction)
  V(graph)$split <- as.character(round(tree$`split point`, digits = 2))
  
  # plot
  plot <- ggraph(graph, 'dendrogram') + 
    theme_bw() +
    geom_edge_link() +
    geom_node_point() +
    geom_node_text(aes(label = node_label), na.rm = TRUE, repel = TRUE) +
    geom_node_label(aes(label = split), vjust = 2.5, na.rm = TRUE, fill = "white") +
    geom_node_label(aes(label = leaf_label, fill = leaf_label), na.rm = TRUE, 
                    repel = TRUE, colour = "white", fontface = "bold", show.legend = FALSE) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          plot.background = element_rect(fill = "white"),
          panel.border = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 18))
  
  print(plot)
}





























source()

tree_num <- which(rf_final_comb[[1]]$finalModel$forest$ndbigtree == min(rf_final_comb[[65]]$finalModel$forest$ndbigtree))

tree_func(final_model = rf_final_comb[[1]]$finalMode, tree_num)




set.seed(42)
sample(1:100, 1)
rf_final_ensemble <- plot(rf_final_comb[[65]]$finalModel)


rf_final_comb[[65]]$finalModel$forest

ggplot(rf_final_ensemble, aes(x = trees, y = Error, )

save.image(plot = rf_final_ensemble, filename = "rf_final_ensemble.jpeg",
       path = "C:/Users/Berg/Desktop/Thesis/Results/RF/",
       dpi = 600, width = 15, height = 10, units = "cm")
  
