


set.seed(42)

#Running Caret with 5-fold 10 repeats in a forloop with 10 data splits
fitControl <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 10, 
                           classProbs = TRUE,
                           savePredictions = TRUE)


##### SupportVecter Machine

svm_df_evalres <- function(seed){
  
  inTraining <- createDataPartition(df$dx, p = .8, list = FALSE)
  training <- df[ inTraining,]
  testing  <- df[-inTraining,]
  
  train(dx ~ ., data = training, 
        method = "svmRadial", 
        trControl = fitControl, 
        verbose = FALSE)}

svm_nzv_evalres <- function(seed){
  
  nzv_inTraining <- createDataPartition(nzv_df$dx, p = .8, list = FALSE)
  nzv_training <- nzv_df[ nzv_inTraining,]
  nzv_testing  <- nzv_df[-nzv_inTraining,]
  
  train(dx ~ ., data = nzv_training, 
        method = "svmRadial", 
        trControl = fitControl, 
        verbose = FALSE)}

svm_nzv_strict_evalres <- function(seed){
  
  nzv_strict_inTraining <- createDataPartition(nzv_strict$dx, p = .8, list = FALSE)
  nzv_strict_training <- nzv_strict[ nzv_strict_inTraining,]
  nzv_strict_testing  <- nzv_strict[-nzv_strict_inTraining,]
  
  train(dx ~ ., data = nzv_strict_training, 
        method = "svmRadial", 
        trControl = fitControl, 
        verbose = FALSE)}

svm_zv_evalres <- function(seed){
  
  zv_inTraining <- createDataPartition(zv_df$dx, p = .8, list = FALSE)
  zv_training <- zv_df[ zv_inTraining,]
  zv_testing  <- zv_df[-zv_inTraining,]
  
  train(dx ~ ., data = zv_training, 
        method = "svmRadial", 
        trControl = fitControl, 
        verbose = FALSE)}


plan("multisession", workers = 8)
tic()
svm_df_evalres_comb <- future_map(1:10, svm_df_evalres,
                                  .options = furrr_options(seed=TRUE))
toc()
tic()
svm_zv_evalres_comb <- future_map(1:10, svm_zv_evalres,
                                  .options = furrr_options(seed=TRUE))
toc()
tic()
svm_nzv_evalres_comb <- future_map(1:10, svm_nzv_evalres,
                                   .options = furrr_options(seed=TRUE))
toc()
tic()
svm_nzv_strict_evalres_comb <- future_map(1:10, svm_nzv_strict_evalres,
                                          .options = furrr_options(seed=TRUE))
toc()
plan("sequential")


#### SVM AUC


models <- c(svm_df_evalres_comb, svm_zv_evalres_comb, svm_nzv_evalres_comb, svm_nzv_strict_evalres_comb)
name <- c("459 Features", "396 Features", "183 Features", "30 Features")

####

models <- c(svm_df_evalres_comb )
names <- c("459 Features")

evalm(list1 = models, 
      gnames = names,
      silent = FALSE,
      showplots = FALSE)

names <- ("459 Features\nAUC = 0.55")

model_res_svm_459 <- evalm(list1 = models, 
                           gnames = names,
                           silent = TRUE,
                           showplots = FALSE)

####

models <- c(svm_zv_evalres_comb )
names <- c("396 Features")

evalm(list1 = models, 
      gnames = names,
      silent = FALSE,
      showplots = FALSE)

names <- ("396 Features\nAUC = 0.63")

model_res_svm_396 <- evalm(list1 = models, 
                           gnames = names,
                           silent = TRUE,
                           showplots = FALSE)


####

models <- c(svm_nzv_evalres_comb )
names <- c("183 Features")

evalm(list1 = models, 
      gnames = names,
      silent = FALSE,
      showplots = FALSE)

names <- ("183 Features\nAUC = 0.68")

model_res_svm_183 <- evalm(list1 = models, 
                           gnames = names,
                           silent = TRUE,
                           showplots = FALSE)


####

models <- c(svm_nzv_strict_evalres_comb )
names <- c("30 Features")

evalm(list1 = models, 
      gnames = names,
      silent = FALSE,
      showplots = FALSE)

names <- ("30 Features\nAUC = 0.57")

model_res_svm_30 <- evalm(list1 = models, 
                          gnames = names,
                          silent = TRUE,
                          showplots = FALSE)
####

svm_features <- rbind(model_res_svm_459[["roc"]][["data"]], model_res_svm_396[["roc"]][["data"]],
                      model_res_svm_183[["roc"]][["data"]], model_res_svm_30[["roc"]][["data"]])



svm_feature_plot <- ggplot(svm_features, aes(x = FPR, y = SENS, color = Group)) + 
  geom_line(size = 0.7) +
  geom_abline(intercept = 0, slope = 1, colour = "grey", linetype = 1,
              size = 0.75) +
  coord_equal() +
  theme_bw() +
  xlab('False positive rate') +
  ylab('True positive rate') +
  #ggtitle("Support Vector Machine")+
  scale_color_manual(values= c("#E41A1C","#377EB8", "#4DAF4A", "#FF7F00")) + 
  theme_classic()+
  theme(legend.position = c(0.65, 0.22),
        legend.text = element_text(size=8), 
        legend.justification = "left",
        legend.title = element_blank())








 svm_nzv_evalres_comb




models <- c(svm_nzv_evalres_comb )
names <- c(" ")

svm_nzv_evalres_comb_model <- evalm(list1 = models, 
                                      gnames = names,
                                      silent = FALSE,
                                      showplots = TRUE,
                                      positive = "cancer")

svm_stats <- as.data.frame(svm_nzv_evalres_comb_model[["stdres"]][[" "]])

TP <- svm_stats[9,1]
FP <- svm_stats[10,1]
TN <- svm_stats[11,1]
FN <- svm_stats[12,1]

accuracy = (TP+TN)/(TP+TN+FP+FN)

svm_stats["accuracy",] <- c(accuracy, "NA" )

svm_stats <- as.data.frame(t(svm_stats))

svm_stats <- svm_stats %>% 
  select(-c("MCC", "Informedness", "FPR", "F1", "AUC-PR", "AUC-PRG"))
