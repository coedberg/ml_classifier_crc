
set.seed(42)

fitControl <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 10, 
                           classProbs = TRUE,
                           savePredictions = TRUE)

##### XGboost
xgb_df_evalres <- function(seed){
  
  inTraining <- createDataPartition(df$dx, p = .8, list = FALSE)
  training <- df[ inTraining,]
  testing  <- df[-inTraining,]
  
  train(dx ~ ., data = training, 
        method = "xgbTree", 
        trControl = fitControl, 
        verbose = FALSE)}

xgb_nzv_evalres <- function(seed){
  
  nzv_inTraining <- createDataPartition(nzv_df$dx, p = .8, list = FALSE)
  nzv_training <- nzv_df[ nzv_inTraining,]
  nzv_testing  <- nzv_df[-nzv_inTraining,]
  
  train(dx ~ ., data = nzv_training, 
        method = "xgbTree", 
        trControl = fitControl, 
        verbose = FALSE)}

xgb_nzv_strict_evalres <- function(seed){
  
  nzv_strict_inTraining <- createDataPartition(nzv_strict$dx, p = .8, list = FALSE)
  nzv_strict_training <- nzv_strict[ nzv_strict_inTraining,]
  nzv_strict_testing  <- nzv_strict[-nzv_strict_inTraining,]
  
  train(dx ~ ., data = nzv_strict_training, 
        method = "xgbTree", 
        trControl = fitControl, 
        verbose = FALSE)}

xgb_zv_evalres <- function(seed){
  
  zv_inTraining <- createDataPartition(zv_df$dx, p = .8, list = FALSE)
  zv_training <- zv_df[ zv_inTraining,]
  zv_testing  <- zv_df[-zv_inTraining,]
  
  train(dx ~ ., data = zv_training, 
        method = "xgbTree", 
        trControl = fitControl, 
        verbose = FALSE)}


plan("multisession", workers = 6)
tic()
xgb_df_evalres_comb <- future_map(1:10, xgb_df_evalres,
                                  .options = furrr_options(seed=TRUE))

toc()
tic()

xgb_zv_evalres_comb <- future_map(1:10, xgb_zv_evalres,
                                  .options = furrr_options(seed=TRUE))

toc()
tic()

xgb_nzv_evalres_comb <- future_map(1:10, xgb_nzv_evalres,
                                   .options = furrr_options(seed=TRUE))
toc()
tic()

xgb_nzv_strict_evalres_comb <- future_map(1:10, xgb_nzv_strict_evalres,
                                          .options = furrr_options(seed=TRUE))
toc()
plan("sequential")


##### XGB AUC

models <- c(xgb_df_evalres_comb, xgb_zv_evalres_comb, xgb_nzv_evalres_comb, xgb_nzv_strict_evalres_comb)
name <- c("459 Features", "396 Features", "183 Features", "30 Features")

####

models <- c(xgb_df_evalres_comb )
names <- c("459 Features")

evalm(list1 = models, 
      gnames = names,
      silent = FALSE,
      showplots = FALSE)

names <- ("459 Features\nAUC = 0.78")

model_res_xgb_459 <- evalm(list1 = models, 
                           gnames = names,
                           silent = TRUE,
                           showplots = FALSE)

####

models <- c(xgb_zv_evalres_comb )
names <- c("396 Features")

evalm(list1 = models, 
      gnames = names,
      silent = FALSE,
      showplots = FALSE)

names <- ("396 Features\nAUC = 0.76")

model_res_xgb_396 <- evalm(list1 = models, 
                           gnames = names,
                           silent = TRUE,
                           showplots = FALSE)


####

models <- c(xgb_nzv_evalres_comb )
names <- c("183 Features")

evalm(list1 = models, 
      gnames = names,
      silent = FALSE,
      showplots = FALSE)

names <- ("183 Features\nAUC = 0.79")

model_res_xgb_183 <- evalm(list1 = models, 
                           gnames = names,
                           silent = TRUE,
                           showplots = FALSE)


####

models <- c(xgb_nzv_strict_evalres_comb )
names <- c("30 Features")

evalm(list1 = models, 
      gnames = names,
      silent = FALSE,
      showplots = FALSE)

names <- ("30 Features\nAUC = 0.55")

model_res_xgb_30 <- evalm(list1 = models, 
                          gnames = names,
                          silent = TRUE,
                          showplots = FALSE)
####

xgb_features <- rbind(model_res_xgb_459[["roc"]][["data"]], model_res_xgb_396[["roc"]][["data"]],
                      model_res_xgb_183[["roc"]][["data"]], model_res_xgb_30[["roc"]][["data"]])



xgb_feature_plot <- ggplot(xgb_features, aes(x = FPR, y = SENS, color = Group)) + 
  geom_line(size = 0.7) +
  geom_abline(intercept = 0, slope = 1, colour = "grey", linetype = 1,
              size = 0.75) +
  coord_equal() +
  theme_bw() +
  xlab('False positive rate') +
  ylab('True positive rate') +
  #ggtitle("XGBoost")+
  scale_color_manual(values= c("#E41A1C","#377EB8", "#4DAF4A", "#FF7F00")) + 
  theme_classic()+
  theme(legend.position = c(0.65, 0.22),
        legend.text = element_text(size=8), 
        legend.justification = "left",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))

xgb_feature_plot

ggsave(plot = xgb_feature_plot, filename = "xgb_feature_plot.jpeg",
       path = "C:/Users/Berg/Desktop/Thesis/Results/XGB/",
       dpi = 600, width = 15, height = 10, units = "cm")






models <- c(xgb_nzv_evalres_comb )
names <- c("183 Features")

xgb_nzv_evalres_comb_stats<-evalm(list1 = models, 
                                 gnames = names,
                                 silent = FALSE,
                                 showplots = FALSE)


stats_xgb <- as.data.frame(xgb_nzv_evalres_comb_stats[["stdres"]][["183 Features"]])

TP <- stats_xgb[9,1]
FP <- stats_xgb[10,1]
TN <- stats_xgb[11,1]
FN <- stats_xgb[12,1]

accuracy = (TP+TN)/(TP+TN+FP+FN)

stats_xgb["accuracy",] <- c(accuracy, "NA" )

stats_xgb <- as.data.frame(t(stats_xgb))

stats_xgb <- stats_xgb %>% 
  select(-c("MCC", "Informedness", "FPR", "F1", "AUC-PR", "AUC-PRG"))










