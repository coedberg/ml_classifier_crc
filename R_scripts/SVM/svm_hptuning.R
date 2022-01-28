
set.seed(42)

##### Hyper Parameter Tuning ###################################################
#Set up tune grids for each model type and a function for data splitting which is seed dependent

fitControl <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 10, 
                           classProbs = TRUE,
                           savePredictions = TRUE)


### SVM
#what was the best tunes in nzv_df svm model in the feature selection? 

svm_besttune_feature <-data.frame(C = 0, sigma = 0)
for(i in 1:10) {                                   
  new <- rep(i, ncol(svm_besttune_feature))                       
  svm_besttune_feature[i,] <- as.data.frame(t(paste0(svm_nzv_df_evalres_comb[[i]][["bestTune"]])))
  svm_besttune_feature[nrow(svm_besttune_feature) + 1, ] <- new                   
}

svm_besttune_feature$count <- 1
svm_besttune_feature <- svm_besttune_feature[1:10,]

svm_besttune_feature <- aggregate(svm_besttune_feature$count,
                                  by = list(svm_besttune_feature$C,
                                            svm_besttune_feature$sigma), FUN = sum) 
colnames(svm_besttune_feature) <- c("C","sigma", "count")

view(svm_besttune_feature)


#select grid corresponding to the df above

svmGrid <- expand.grid(C = c(0.00001,0.0001,0.001, 0.01, 0.1, 0.5, 1, 10, 100, 1000),
                       sigma = c(0.0000001,0.000001, 0.00001, 0.0001, 0.001, 0.01, 0.1, 0.25, 0.5, 1))

svm_parameter_results <- function(seed){
  
  nzv_df_inTraining <- createDataPartition(nzv_df$dx, p = .8, list = FALSE)
  nzv_df_training <- nzv_df[ nzv_df_inTraining,]
  nzv_df_testing  <- nzv_df[-nzv_df_inTraining,]
  
  train(dx ~ ., data = nzv_df_training, 
        method = "svmRadial", 
        trControl = fitControl, 
        verbose = FALSE,
        tuneGrid = svmGrid)
}




plan("multisession", workers = 6)

# 1 seed = 460 sec => 100 seeds = 12 hours 
tic()
svm_parameter_results_comb <- future_map(1:100, svm_parameter_results, .options = furrr_options(seed=TRUE))
toc()

plan("sequential")









svm_best_tune <-data.frame(sigma = 0, C = 0)
for(i in 1:100) {                                   # Head of for-loop
  new <- rep(i, ncol(svm_best_tune))                       # Create new row
  svm_best_tune[i,] <- as.data.frame(t(paste0(svm_parameter_results_comb[[i]][["bestTune"]])))
  svm_best_tune[nrow(svm_best_tune) + 1, ] <- new                   # Append new row
}

svm_best_tune <- svm_best_tune[1:100,]
svm_best_tune$count <- 1
svm_best_tune <- aggregate(svm_best_tune$count,
                           by = list(svm_best_tune$sigma,svm_best_tune$C), FUN = sum) 
colnames(svm_best_tune) <- c("sigma", "C", "count")





svm_final_comb_stats <-data.frame(accuracy = 0)
for(i in 1:100) {                                   
  new <- rep(i, ncol(svm_final_comb_stats))                       
  svm_final_comb_stats[i,] <- as.data.frame(t(paste0(svm_parameter_results_comb[[i]][["resample"]][["Accuracy"]])))
  svm_final_comb_stats[nrow(svm_final_comb_stats) + 1, ] <- new                   
}


svm_final_comb_stats <- as.data.frame(as.numeric(svm_final_comb_stats[1:100,]))
colnames(svm_final_comb_stats) <- "accuracy"
min(svm_final_comb_stats$accuracy)
max(svm_final_comb_stats$accuracy)
mean(svm_final_comb_stats$accuracy)





svm_hp_metrics <- combine_hp_performance(svm_parameter_results_comb)

svm_sigma <- plot_hp_performance(svm_hp_metrics$dat, sigma, Accuracy) + 
  geom_errorbar(ggplot2::aes(
    ymin = .data$ymin_metric,
    ymax = .data$ymax_metric),
    width = .001, color = "grey")+
  geom_line( color="black") +
  geom_point(shape=21, color="black", fill="black", size=1)+
  theme_classic()+
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  #scale_y_continuous(breaks = c(0.55, 0.60, 0.65), limits = c(0.55, 0.65))+
  labs(x = "Bandwidth of kernal function", y = "Mean Accuracy")

svm_C <-plot_hp_performance(svm_hp_metrics$dat, C, Accuracy) + 
  geom_errorbar(ggplot2::aes(
    ymin = .data$ymin_metric,
    ymax = .data$ymax_metric),
    width = .001, color = "grey")+
  geom_line( color="black") +
  geom_point(shape=21, color="black", fill="black", size=1)+
  theme_classic()+
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  #scale_y_continuous(breaks = c(0.55, 0.60, 0.65), limits = c(0.55, 0.65))+
  labs(x = "Cost", y = "Mean Accuracy")



svm_tune_plot <- plot_grid(svm_C, svm_sigma,
          labels = c("A", "B"))



ggsave(plot = svm_tune_plot, filename = "svm_tune_plot.jpeg",
       path = "C:/Users/Berg/Desktop/Thesis/Results/SVM/",
       dpi = 600, width = 15, height = 10, units = "cm")






################












svmGrid1 <- expand.grid(C = c(1, 10, 35, 50, 100),
                       sigma = c(10^-3))

svm_parameter_grid1 <- function(seed){
  
  nzv_df_inTraining <- createDataPartition(nzv_df$dx, p = .8, list = FALSE)
  nzv_df_training <- nzv_df[ nzv_df_inTraining,]
  nzv_df_testing  <- nzv_df[-nzv_df_inTraining,]
  
  train(dx ~ ., data = nzv_df_training, 
        method = "svmRadial", 
        trControl = fitControl, 
        verbose = FALSE,
        tuneGrid = svmGrid1)
}




plan("multisession", workers = 6)

# 1 seed = 27 sec 
tic()
svm_grid1_comb <- future_map(1:100, svm_parameter_grid1, .options = furrr_options(seed=TRUE))
toc()

plan("sequential")



svm_grid1_comb_stats <-data.frame(accuracy = 0)
for(i in 1:100) {                                   
  new <- rep(i, ncol(svm_grid1_comb_stats))                       
  svm_grid1_comb_stats[i,] <- as.data.frame(t(paste0(svm_grid1_comb[[i]][["resample"]][["Accuracy"]])))
  svm_grid1_comb_stats[nrow(svm_grid1_comb_stats) + 1, ] <- new                   
}


svm_grid1_comb_stats <- as.data.frame(as.numeric(svm_grid1_comb_stats[1:100,]))
colnames(svm_grid1_comb_stats) <- "accuracy"
min(svm_grid1_comb_stats$accuracy)
max(svm_grid1_comb_stats$accuracy)
mean(svm_grid1_comb_stats$accuracy)




svm_hp_metrics_grid1 <- combine_hp_performance(svm_grid1_comb)

svm_C_grid1 <- plot_hp_performance(svm_hp_metrics_grid1$dat, C, Accuracy) + 
  geom_errorbar(ggplot2::aes(
    ymin = .data$ymin_metric,
    ymax = .data$ymax_metric),
    width = .001, color = "grey")+
  geom_line( color="black") +
  geom_point(shape=21, color="black", fill="black", size=1)+
  theme_classic()+
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  #scale_y_continuous(breaks = c(0.55, 0.60, 0.65), limits = c(0.55, 0.65))+
  labs(x = "C", y = "Mean Accuracy")



svm_best_tune_grid1 <-data.frame(sigma = 0, C = 0)
for(i in 1:100) {                                   # Head of for-loop
  new <- rep(i, ncol(svm_best_tune_grid1))                       # Create new row
  svm_best_tune_grid1[i,] <- as.data.frame(t(paste0(svm_grid1_comb[[i]][["bestTune"]])))
  svm_best_tune_grid1[nrow(svm_best_tune_grid1) + 1, ] <- new                   # Append new row
}

svm_best_tune_grid1 <- svm_best_tune_grid1[1:100,]
svm_best_tune_grid1$count <- 1
svm_best_tune_grid1 <- aggregate(svm_best_tune_grid1$count,
                           by = list(svm_best_tune_grid1$sigma,svm_best_tune_grid1$C), FUN = sum) 
colnames(svm_best_tune_grid1) <- c("sigma", "C", "count")


view(svm_best_tune_grid1)









svmGrid2 <- expand.grid(C = c(10),
                        sigma = c(10^-7, 10^-5, 10^-3))

svm_parameter_grid2 <- function(seed){
  
  nzv_df_inTraining <- createDataPartition(nzv_df$dx, p = .8, list = FALSE)
  nzv_df_training <- nzv_df[ nzv_df_inTraining,]
  nzv_df_testing  <- nzv_df[-nzv_df_inTraining,]
  
  train(dx ~ ., data = nzv_df_training, 
        method = "svmRadial", 
        trControl = fitControl, 
        verbose = FALSE,
        tuneGrid = svmGrid2)
}




plan("multisession", workers = 6)

# 1 seed = 27 sec 
tic()
svm_grid2_comb <- future_map(1:100, svm_parameter_grid2, .options = furrr_options(seed=TRUE))
toc()

plan("sequential")



svm_grid2_comb_stats <-data.frame(accuracy = 0)
for(i in 1:100) {                                   
  new <- rep(i, ncol(svm_grid2_comb_stats))                       
  svm_grid2_comb_stats[i,] <- as.data.frame(t(paste0(svm_grid2_comb[[i]][["resample"]][["Accuracy"]])))
  svm_grid2_comb_stats[nrow(svm_grid2_comb_stats) + 1, ] <- new                   
}


svm_grid2_comb_stats <- as.data.frame(as.numeric(svm_grid2_comb_stats[1:100,]))
colnames(svm_grid2_comb_stats) <- "accuracy"
min(svm_grid2_comb_stats$accuracy)
max(svm_grid2_comb_stats$accuracy)
mean(svm_grid2_comb_stats$accuracy)




svm_hp_metrics_grid2 <- combine_hp_performance(svm_grid2_comb)

svm_C_grid2 <- plot_hp_performance(svm_hp_metrics_grid2$dat, sigma, Accuracy) + 
  geom_errorbar(ggplot2::aes(
    ymin = .data$ymin_metric,
    ymax = .data$ymax_metric),
    width = .001, color = "grey")+
  geom_line( color="black") +
  geom_point(shape=21, color="black", fill="black", size=1)+
  theme_classic()+
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  #scale_y_continuous(breaks = c(0.55, 0.60, 0.65), limits = c(0.55, 0.65))+
  labs(x = "sigma", y = "Mean Accuracy")



svm_best_tune_grid2 <-data.frame(sigma = 0, C = 0)
for(i in 1:100) {                                   # Head of for-loop
  new <- rep(i, ncol(svm_best_tune_grid2))                       # Create new row
  svm_best_tune_grid2[i,] <- as.data.frame(t(paste0(svm_grid2_comb[[i]][["bestTune"]])))
  svm_best_tune_grid2[nrow(svm_best_tune_grid2) + 1, ] <- new                   # Append new row
}

svm_best_tune_grid2 <- svm_best_tune_grid2[1:100,]
svm_best_tune_grid2$count <- 1
svm_best_tune_grid2 <- aggregate(svm_best_tune_grid2$count,
                                 by = list(svm_best_tune_grid2$sigma,svm_best_tune_grid2$C), FUN = sum) 
colnames(svm_best_tune_grid2) <- c("sigma", "C", "count")


view(svm_best_tune_grid2)



##################




svmGrid_final <- expand.grid(C = c(10),
                             sigma = c(10^-5))

svm_parameter_final <- function(seed){
  
  nzv_df_inTraining <- createDataPartition(nzv_df$dx, p = .8, list = FALSE)
  nzv_df_training <- nzv_df[ nzv_df_inTraining,]
  nzv_df_testing  <- nzv_df[-nzv_df_inTraining,]
  
  train(dx ~ ., data = nzv_df_training, 
        method = "svmRadial", 
        trControl = fitControl, 
        verbose = FALSE,
        tuneGrid = svmGrid_final)
}




plan("multisession", workers = 6)

# 1 seed = 27 sec 
tic()
svm_final_comb <- future_map(1:100, svm_parameter_final, .options = furrr_options(seed=TRUE))
toc()

plan("sequential")



svm_final_comb_stats <-data.frame(accuracy = 0)
for(i in 1:100) {                                   
  new <- rep(i, ncol(svm_final_comb_stats))                       
  svm_final_comb_stats[i,] <- as.data.frame(t(paste0(svm_final_comb[[i]][["resample"]][["Accuracy"]])))
  svm_final_comb_stats[nrow(svm_final_comb_stats) + 1, ] <- new                   
}


svm_final_comb_stats <- as.data.frame(as.numeric(svm_final_comb_stats[1:100,]))
colnames(svm_final_comb_stats) <- "accuracy"
min(svm_final_comb_stats$accuracy)
max(svm_final_comb_stats$accuracy)
mean(svm_final_comb_stats$accuracy)




models <- c(svm_final_comb)
names <- c("SVM: AUC = 0.77")


model_svm_final <- evalm(list1 = models, 
                         gnames = names, 
                         title = " ", 
                         cols = NULL,
                         silent = FALSE, 
                         rlinethick = 0.7, 
                         fsize = 12.5,
                         dlinecol = "grey", 
                         dlinethick = 0.75, 
                         bins = 1000, 
                         optimise = "INF",
                         percent = 95, 
                         showplots = FALSE, 
                         positive = "cancer", 
                         plots = c("prg","pr", "r", "cc"))


svm_final <- model_svm_final$roc + 
  scale_color_manual(values= c("#E41A1C", "#377EB8", "#4DAF4A", "#FF7F00")) + 
  theme_classic()+
  theme(legend.position = c(0.65, 0.22),
        legend.text = element_text(size=8), 
        legend.justification = "left",
        legend.title = element_blank()) 
svm_final


ggsave(plot = svm_final, filename = "svm_final.jpeg",
       path = "C:/Users/Berg/Desktop/Thesis/Results/svm/",
       dpi = 600, width = 15, height = 10, units = "cm")

##### STATS ###########


models <- c(svm_final_comb)
names <- c(" ")

svm_final_comb_stats <- evalm(list1 = models, 
                             gnames = names,
                             silent = FALSE,
                             showplots = TRUE,
                             positive = "cancer")

stats <- as.data.frame(svm_final_comb_stats[["stdres"]][[" "]])


TP <- stats[9,1]
FP <- stats[10,1]
TN <- stats[11,1]
FN <- stats[12,1]

accuracy = (TP+TN)/(TP+TN+FP+FN)

stats["accuracy",] <- c(accuracy, "NA" )

stats <- as.data.frame(t(stats))

stats <- stats %>% 
  select(-c("MCC", "Informedness", "FPR", "F1", "AUC-PR", "AUC-PRG"))


svm_stats_final <- stats

write_xlsx(
  svm_stats_final,
  path = "C:/Users/Berg/Desktop/Thesis/Results/svm/svm_stats_final.xlsx",
  col_names = TRUE,
  format_headers = TRUE,
  use_zip64 = FALSE
)





##### Feature importance

fit1 <-svm(dx ~ ., data = nzv_df)

test <- t(fit1$coefs) %*% fit1$SV                 # weight vectors
test1 <- apply(test, 2, function(v){sqrt(sum(v^2))}) # weight

svm_imp <- as.data.frame(test1) %>% 
  rownames_to_column() %>% 
  mutate(imp = (test1/sum(test1)*100),
         genus = rowname) %>% 
  select(genus, imp) %>% 
  arrange(desc(imp))



svm_imp_top10 <- svm_imp %>% 
  top_n(svm_imp$imp, n = 10) %>% 
  mutate(imp = as.double(imp),
         genus = fct_reorder(genus, imp))

svm_imp_plot <- ggplot(svm_imp_top10, aes(x = imp, y = genus, color = genus)) +
  geom_point()+#colour = "#0072B2")+
  scale_x_continuous(limits = c(1,15),
                     breaks = c(5,10,15)) +  
  scale_y_discrete(name = "") +      
  ggtitle("Support-Vector Machine \nFeature Importance") +
  theme(axis.line = element_line(colour = "black", 
                                 size = 0.25))+
  theme_classic()+
  #scale_y_discrete(limits=rev)+
  labs(tag = "C",
       x= "Feature Importance (%)", 
       y=NULL)+
  theme(legend.position = "none")


svm_imp_plot_zoom <- ggplot(svm_imp_top10, aes(x = imp, y = genus, color = genus)) +
  geom_point()+#colour = "#0072B2")+
  scale_x_continuous(limits = c(1,3),
                     breaks = c(1,2,3)) +  
  scale_y_discrete(name = "") +      
  #ggtitle("Support-Vector Machine \nFeature Importance") +
  theme(axis.line = element_line(colour = "black", 
                                 size = 0.25))+
  theme_classic()+
  #scale_y_discrete(limits=rev)+
  labs(#tag = "C",
       x= "Feature Importance (%)", 
       y=NULL)+
  theme(legend.position = "none")

ggsave(plot = svm_imp_plot_zoom, filename = "svm_imp_plot_zoom.jpeg",
       path = "C:/Users/Berg/Desktop/Thesis/Results/SVM/",
       dpi = 600, width = 15, height = 8, units = "cm")











