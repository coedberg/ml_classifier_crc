
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

rfGrid_rough <- expand.grid(mtry = c(7, 14, 28, 183))

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

# 2 seeds = 170 s  => 1.5 h for 100 seeds
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
  labs(x = "mtry", y = "Mean Accuracy")


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
  #scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
  #              labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  scale_y_continuous(limits = c(0.7, 0.75))+
  labs(x = "mtry", y = "Mean Accuracy")


rf_mtry_fine


rf_mtry_tune <- plot_grid(rf_mtry_rough, rf_mtry_fine)

rf_mtry_tune
ggsave(plot = rf_mtry_tune, filename = "rf_mtry_tune.jpeg",
       path = "C:/Users/Berg/Desktop/Thesis/Pictures/R",
       dpi = 600, width = 15, height = 10, units = "cm")


####
rfGrid_fine2 <- expand.grid(mtry = c(16,20,22,23,24))

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
rf_parameter_results_comb_fine2 <- future_map(1:10, rf_parameter_results_fine2, .options = furrr_options(seed=TRUE))
toc()

plan("sequential")





rf_best_tune <-data.frame(mtry = 0)
for(i in 1:10) {                                   # Head of for-loop
  new <- rep(i, ncol(rf_best_tune))                       # Create new row
  rf_best_tune[i,] <- as.data.frame(t(paste0(rf_parameter_results_comb_fine2[[i]][["bestTune"]])))
  rf_best_tune[nrow(rf_best_tune) + 1, ] <- new                   # Append new row
}


rf_best_tune$count <- 1
rf_best_tune <- rf_best_tune[1:10,]

rf_best_tune <- aggregate(rf_best_tune$count,
                          by = list(rf_best_tune$mtry), FUN = sum) 
colnames(rf_best_tune) <- c("mtry", "count")

view(rf_best_tune)











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

names <- c("Tuned \nAUC = 0.84")

model_rf_final <- evalm(list1 = models, 
                       gnames = names, 
                       title = "Random Forest", 
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




rf_final_comparison <- rbind(model_res_rf_183[["roc"]][["data"]], model_rf_final[["roc"]][["data"]])

rf_final_plot <- ggplot(rf_final_comparison, aes(x = FPR, y = SENS, color = Group)) + 
  geom_line(size = 0.7) +
  geom_abline(intercept = 0, slope = 1, colour = "grey", linetype = 1,
              size = 0.75) +
  coord_equal() +
  theme_bw() +
  xlab('False positive rate') +
  ylab('True positive rate') +
  ggtitle("Random Forest")+
  scale_color_manual(values= c("#E41A1C","#377EB8", "#4DAF4A", "#FF7F00")) + 
  theme_classic()+
  theme(legend.position = c(0.65, 0.22),
        legend.text = element_text(size=8), 
        legend.justification = "left",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))

