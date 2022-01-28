set.seed(42)

#Running Caret with 5-fold 10 repeats in a forloop with 10 data splits
fitControl <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 10, 
                           classProbs = TRUE,
                           savePredictions = TRUE)

names <- c("459 Features", "396 Features", "183 Features", "30 Features")

##### Random forest

rf_df_evalres <- function(seed){
  
  inTraining <- createDataPartition(df$dx, p = .8, list = FALSE)
  training <- df_noid[ inTraining,]
  testing  <- df_noid[-inTraining,]
  
  train(dx ~ ., data = training, 
        method = "rf", 
        trControl = fitControl, 
        verbose = FALSE)}

rf_nzv_evalres <- function(seed){
  
  nzv_inTraining <- createDataPartition(nzv_df$dx, p = .8, list = FALSE)
  nzv_training <- nzv_df[ nzv_inTraining,]
  nzv_testing  <- nzv_df[-nzv_inTraining,]
  
  train(dx ~ ., data = nzv_training, 
        method = "rf", 
        trControl = fitControl, 
        verbose = FALSE)}

rf_nzv_strict_evalres <- function(seed){
  
  nzv_strict_inTraining <- createDataPartition(nzv_strict$dx, p = .8, list = FALSE)
  nzv_strict_training <- nzv_strict[ nzv_strict_inTraining,]
  nzv_strict_testing  <- nzv_strict[-nzv_strict_inTraining,]
  
  train(dx ~ ., data = nzv_strict_training, 
        method = "rf", 
        trControl = fitControl, 
        verbose = FALSE)}

rf_zv_evalres <- function(seed){
  
  zv_inTraining <- createDataPartition(zv_df$dx, p = .8, list = FALSE)
  zv_training <- zv_df[ zv_inTraining,]
  zv_testing  <- zv_df[-zv_inTraining,]
  
  train(dx ~ ., data = zv_training, 
        method = "rf", 
        trControl = fitControl, 
        verbose = FALSE)}


plan("multisession", workers = 8)
tic()
rf_df_evalres_comb <- future_map(1:10, rf_df_evalres,
                                 .options = furrr_options(seed=TRUE))
toc()
tic()

rf_zv_evalres_comb  <- future_map(1:10, rf_zv_evalres,
                                  .options = furrr_options(seed=TRUE))
toc()
tic()

rf_nzv_evalres_comb <- future_map(1:10, rf_nzv_evalres,
                                  .options = furrr_options(seed=TRUE))
toc()
tic()

rf_nzv_strict_evalres_comb <- future_map(1:10, rf_nzv_strict_evalres,
                                         .options = furrr_options(seed=TRUE))
toc()
plan("sequential")





##### RF AUC

models <- c(rf_df_evalres_comb, rf_zv_evalres_comb, rf_nzv_evalres_comb, rf_nzv_strict_evalres_comb)
name <- c("459 Features", "396 Features", "183 Features", "30 Features")

####

models <- c(rf_df_evalres_comb )
names <- c("459 Features")

evalm(list1 = models, 
      gnames = names,
      silent = FALSE,
      showplots = FALSE)

names <- ("459 Features\nAUC = 0.80")

model_res_rf_459 <- evalm(list1 = models, 
                          gnames = names,
                          silent = TRUE,
                          showplots = FALSE)

####

models <- c(rf_zv_evalres_comb )
names <- c("396 Features")

evalm(list1 = models, 
      gnames = names,
      silent = FALSE,
      showplots = FALSE)

names <- ("396 Features\nAUC = 0.77")

model_res_rf_396 <- evalm(list1 = models, 
                          gnames = names,
                          silent = TRUE,
                          showplots = FALSE)


####

models <- c(rf_nzv_evalres_comb )
names <- c("183 Features")

evalm(list1 = models, 
      gnames = names,
      silent = FALSE,
      showplots = FALSE)

names <- ("183 Features\nAUC = 0.80")

model_res_rf_183 <- evalm(list1 = models, 
                          gnames = names,
                          silent = TRUE,
                          showplots = FALSE)


####

models <- c(rf_nzv_strict_evalres_comb )
names <- c("30 Features")

evalm(list1 = models, 
      gnames = names,
      silent = FALSE,
      showplots = FALSE)

names <- ("30 Features\nAUC = 0.60")

model_res_rf_30 <- evalm(list1 = models, 
                         gnames = names,
                         silent = TRUE,
                         showplots = FALSE)
####

rf_features <- rbind(model_res_rf_459[["roc"]][["data"]], model_res_rf_396[["roc"]][["data"]],
                     model_res_rf_183[["roc"]][["data"]], model_res_rf_30[["roc"]][["data"]])



rf_feature_plot <- ggplot(rf_features, aes(x = FPR, y = SENS, color = Group)) + 
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

rf_feature_plot

ggsave(plot = rf_feature_plot, filename = "rf_feature_plot.jpeg",
       path = "C:/Users/Berg/Desktop/Thesis/Results/RF/",
       dpi = 600, width = 15, height = 10, units = "cm")





















rf_nzv_evalres_comb_182 <- list(rf_nzv_evalres_comb[[1]],rf_nzv_evalres_comb[[8]])
rf_nzv_evalres_comb_92 <- list(rf_nzv_evalres_comb[[2]],rf_nzv_evalres_comb[[3]],
                                rf_nzv_evalres_comb[[4]],rf_nzv_evalres_comb[[5]],
                                rf_nzv_evalres_comb[[6]],rf_nzv_evalres_comb[[7]],
                                rf_nzv_evalres_comb[[9]],rf_nzv_evalres_comb[[10]])



models <- c(rf_nzv_evalres_comb_182 )
names <- c("mtry_182")

rf_df_evalres_comb_stats_182 <- evalm(list1 = models, 
                                  gnames = names,
                                  silent = FALSE,
                                  showplots = TRUE,
                                  positive = "cancer")

stats_182 <- as.data.frame(rf_df_evalres_comb_stats_182[["stdres"]][["mtry_182"]])

TP <- 43
FP <- 13
TN <- 106
FN <- 36

accuracy = (TP+TN)/(TP+TN+FP+FN)

stats_182["accuracy",] <- c(accuracy, "NA" )

stats_182["mtry",]<-rf_nzv_evalres_comb[[8]][["bestTune"]][["mtry"]]

stats_182 <- as.data.frame(t(stats_182))

stats_182 <- stats_182 %>% 
  select(-c("MCC", "Informedness", "FPR", "F1", "AUC-PR", "AUC-PRG"))








models <- c(rf_nzv_evalres_comb_92 )
names <- c("mtry_92")

rf_df_evalres_comb_stats_92 <- evalm(list1 = models, 
                                      gnames = names,
                                      silent = FALSE,
                                      showplots = TRUE,
                                      positive = "cancer")

stats_92 <- as.data.frame(rf_df_evalres_comb_stats_92[["stdres"]][["mtry_92"]])

TP <- 36
FP <- 7
TN <- 112
FN <- 43

accuracy = (TP+TN)/(TP+TN+FP+FN)

stats_92["accuracy",] <- c(accuracy, "NA" )

stats_92["mtry",]<-rf_nzv_evalres_comb[[2]][["bestTune"]][["mtry"]]

stats_92 <- as.data.frame(t(stats_92))

stats_92 <- stats_92 %>% 
  select(-c("MCC", "Informedness", "FPR", "F1", "AUC-PR", "AUC-PRG"))




rf_stats_183feat <- rbind(stats_92,stats_182)

write_xlsx(
  rf_stats_183feat,
  path = "C:/Users/Berg/Desktop/Thesis/Results/RF/rf_stats_183feat.xlsx",
  col_names = TRUE,
  format_headers = TRUE,
  use_zip64 = FALSE)










models <- c(rf_nzv_evalres_comb )
names <- c("183 Features")

rf_nzv_evalres_comb_stats<-evalm(list1 = models, 
      gnames = names,
      silent = FALSE,
      showplots = FALSE)


stats_rf <- as.data.frame(rf_nzv_evalres_comb_stats[["stdres"]][["183 Features"]])

TP <- stats_rf[9,1]
FP <- stats_rf[10,1]
TN <- stats_rf[11,1]
FN <- stats_rf[12,1]

accuracy = (TP+TN)/(TP+TN+FP+FN)

stats_rf["accuracy",] <- c(accuracy, "NA" )

stats_rf <- as.data.frame(t(stats_rf))

stats_rf <- stats_rf %>% 
  select(-c("MCC", "Informedness", "FPR", "F1", "AUC-PR", "AUC-PRG"))























