
fitControl <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 10, 
                           classProbs = TRUE,
                           savePredictions = TRUE)

set.seed(42)



xgb_besttune_feature <-data.frame(nrounds = 0, max_depth = 0, eta = 0, gamma = 0, colsample_bytree = 0, min_child_weight = 0, subsample = 0)
for(i in 1:10) {                                   # Head of for-loop
  new <- rep(i, ncol(xgb_besttune_feature))                       # Create new row
  xgb_besttune_feature[i,] <- as.data.frame(t(paste0(xgb_nzv_evalres_comb[[i]][["bestTune"]])))
  xgb_besttune_feature[nrow(xgb_besttune_feature) + 1, ] <- new                   # Append new row
}

xgb_besttune_feature$count <- 1
xgb_besttune_feature <- xgb_besttune_feature[1:10,]

xgb_besttune_feature <- aggregate(xgb_besttune_feature$count,
                           by = list(xgb_besttune_feature$nrounds,
                                     xgb_besttune_feature$max_depth,
                                     xgb_besttune_feature$eta,
                                     xgb_besttune_feature$gamma,
                                     xgb_besttune_feature$colsample_bytree,
                                     xgb_besttune_feature$min_child_weight,
                                     xgb_besttune_feature$subsample), FUN = sum) 
colnames(xgb_besttune_feature) <- c("nrounds", "maxdepth", "eta", "gamma", "colsample", "childw", "subsample", "count")


view(xgb_besttune_feature)






##### xgb nrounds  #####

xgbGrid_nrounds <- expand.grid(nrounds = c(seq(5,25, by = 5), seq(50,500, by = 50)), # Boosting Iterations)
                       max_depth = 1, #seq(2, 10, by = 1), #(Max Tree Depth)
                       eta = 0.3, #c(0.01, 0.1, 0.5), #(Shrinkage)
                       gamma = 0, #(Minimum Loss Reduction)
                       colsample_bytree = 0.6, #seq(0.2, 1, by = 0.2), #(Subsample Ratio of Columns)
                       min_child_weight = 1, #seq(0.5, 1.5, by = 0.5), #(Minimum Sum of Instance Weight)
                       subsample = 1) #seq(0.2, 1, by = 0.2)) #(Subsample Percentage))

xgb_nrounds <- function(seed){
  
  nzv_inTraining <- createDataPartition(nzv_df$dx, p = .8, list = FALSE)
  nzv_training <- nzv_df[ nzv_inTraining,]
  nzv_testing  <- nzv_df[-nzv_inTraining,]
  
  train(dx ~ ., data = nzv_training,  
        method = "xgbTree", 
        trControl = fitControl, 
        verbose = FALSE,
        tuneGrid = xgbGrid_nrounds)
}



plan("multisession", workers = 8)
tic() #1 seed = 25 s 
xgb_nrounds_comb <- future_map(1:100, xgb_nrounds,
                                         .options = furrr_options(seed=TRUE))
toc()
plan("sequential")



xgb_best_nrounds <-data.frame(nrounds = 0)
for(i in 1:100) {                                   
  new <- rep(i, ncol(xgb_best_nrounds))                       
  xgb_best_nrounds[i,] <- as.data.frame(t(paste0(xgb_nrounds_comb[[i]][["bestTune"]][["nrounds"]])))
  xgb_best_nrounds[nrow(xgb_best_nrounds) + 1, ] <- new                   
}

xgb_best_nrounds$count <- 1
xgb_best_nrounds <- xgb_best_nrounds[1:100,]

xgb_best_nrounds <- aggregate(xgb_best_nrounds$count,
                           by = list(xgb_best_nrounds$nrounds), FUN = sum) 
colnames(xgb_best_nrounds) <- c("nrounds", "count")



top_n(xgb_best_nrounds, 3)


xgb_plot_nrounds <- combine_hp_performance(xgb_nrounds_comb)

xgb_plot_nrounds <- plot_hp_performance(xgb_plot_nrounds$dat, nrounds, Accuracy) + 
  geom_errorbar(ggplot2::aes(
    ymin = .data$ymin_metric,
    ymax = .data$ymax_metric),
    width = .001, color = "grey")+
  geom_line(color="black") +
  geom_point(shape=21, color="black", fill="black", size=1)+
  theme_classic()+
  # scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
  #               labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  # scale_y_continuous(breaks = c(0.50, 0.55, 0.60), limits = c(0.50, 0.60))+
  labs(x = "Boosting Iterations", y = "Mean Accuracy")

xgb_plot_nrounds


##### XGB eta #####
#range : [0,1]
#Typical final values : 0.01-0.2.
#It is the step size shrinkage used in update to prevent overfitting.

xgbGrid_eta <- expand.grid(nrounds = 50, #seq(50, 300, by = 50), # Boosting Iterations)
                       max_depth = 1, #seq(2, 10, by = 1), #(Max Tree Depth)
                       eta = c(0.01, 0.1, 0.2, 0.3, 0.4), #(Shrinkage)
                       gamma = 0, #(Minimum Loss Reduction)
                       colsample_bytree = 0.6, #seq(0.2, 1, by = 0.2), #(Subsample Ratio of Columns)
                       min_child_weight = 1, #seq(0.5, 1.5, by = 0.5), #(Minimum Sum of Instance Weight)
                       subsample = 1) #seq(0.2, 1, by = 0.2)) #(Subsample Percentage))

xgb_eta <- function(seed){
  
  nzv_inTraining <- createDataPartition(nzv_df$dx, p = .8, list = FALSE)
  nzv_training <- nzv_df[ nzv_inTraining,]
  nzv_testing  <- nzv_df[-nzv_inTraining,]
  
  train(dx ~ ., data = nzv_training, 
        method = "xgbTree", 
        trControl = fitControl, 
        verbose = FALSE,
        tuneGrid = xgbGrid_eta)
}


plan("multisession", workers = 8)
tic() #1 seed = 116
xgb_eta_comb <- future_map(1:100, xgb_eta, .options = furrr_options(seed=TRUE))
toc()
plan("sequential")



xgb_best_eta <-data.frame(eta = 0)
for(i in 1:100) {                                   
  new <- rep(i, ncol(xgb_best_eta))                       
  xgb_best_eta[i,] <- as.data.frame(t(paste0(xgb_eta_comb[[i]][["bestTune"]][["eta"]])))
  xgb_best_eta[nrow(xgb_best_eta) + 1, ] <- new                   
}

xgb_best_eta$count <- 1
xgb_best_eta <- xgb_best_eta[1:100,]

xgb_best_eta <- aggregate(xgb_best_eta$count,
                          by = list(xgb_best_eta$eta), FUN = sum) 
colnames(xgb_best_eta) <- c("eta", "count")

top_n(xgb_best_eta, 3)


xgb_plot_eta <- combine_hp_performance(xgb_eta_comb)

xgb_plot_eta <- plot_hp_performance(xgb_plot_eta$dat, eta, Accuracy) + 
  geom_errorbar(ggplot2::aes(
    ymin = .data$ymin_metric,
    ymax = .data$ymax_metric),
    width = .001, color = "grey")+
  geom_line(color="black") +
  geom_point(shape=21, color="black", fill="black", size=1)+
  theme_classic()+
  # scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
  #               labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  # scale_y_continuous(breaks = c(0.50, 0.55, 0.60), limits = c(0.50, 0.60))+
  labs(x = "Shrinkage (eta)", y = "Mean Accuracy")

xgb_plot_eta

##### XGB max depth  #####
#Typical values: 3-10
#increasing this value will make the model more complex and more likely to overfit.

xgbGrid_maxdepth <- expand.grid(nrounds = 50, #seq(100, 500, by = 100), # Boosting Iterations)
                       max_depth = c(seq(1, 15, by = 3), seq(20, 60, by = 5)), #(Max Tree Depth)
                       eta = 0.3, #c(0.01, 0.1, 0.5), #(Shrinkage)
                       gamma = 0, #(Minimum Loss Reduction)
                       colsample_bytree = 0.6, #seq(0.2, 1, by = 0.2), #(Subsample Ratio of Columns)
                       min_child_weight = 1, #seq(0.5, 1.5, by = 0.5), #(Minimum Sum of Instance Weight)
                       subsample = 1) #seq(0.2, 1, by = 0.2)) #(Subsample Percentage))

xgb_maxdepth <- function(seed){
  
  nzv_inTraining <- createDataPartition(nzv_df$dx, p = .8, list = FALSE)
  nzv_training <- nzv_df[ nzv_inTraining,]
  nzv_testing  <- nzv_df[-nzv_inTraining,]
  
  train(dx ~ ., data = nzv_training, 
        method = "xgbTree", 
        trControl = fitControl, 
        verbose = FALSE,
        tuneGrid = xgbGrid_maxdepth)
}


plan("multisession", workers = 8)
tic() #1 seed = 30 s 
xgb_maxdepth_comb <- future_map(1:100, xgb_maxdepth, .options = furrr_options(seed=TRUE))
toc()
plan("sequential")




xgb_best_maxdepth <-data.frame(maxdepth = 0)
for(i in 1:100) {                                   
  new <- rep(i, ncol(xgb_best_maxdepth))                       
  xgb_best_maxdepth[i,] <- as.data.frame(t(paste0(xgb_maxdepth_comb[[i]][["bestTune"]][["max_depth"]])))
  xgb_best_maxdepth[nrow(xgb_best_maxdepth) + 1, ] <- new                   
}

xgb_best_maxdepth$count <- 1
xgb_best_maxdepth <- xgb_best_maxdepth[1:100,]

xgb_best_maxdepth <- aggregate(xgb_best_maxdepth$count,
                          by = list(xgb_best_maxdepth$maxdepth), FUN = sum) 
colnames(xgb_best_maxdepth) <- c("maxdepth", "count")

top_n(xgb_best_maxdepth, 3)


xgb_plot_maxdepth <- combine_hp_performance(xgb_maxdepth_comb)

xgb_plot_maxdepth <- plot_hp_performance(xgb_plot_maxdepth$dat, max_depth, Accuracy) + 
  geom_errorbar(ggplot2::aes(
    ymin = .data$ymin_metric,
    ymax = .data$ymax_metric),
    width = .001, color = "grey")+
  geom_line(color="black") +
  geom_point(shape=21, color="black", fill="black", size=1)+
  theme_classic()+
  # scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
  #               labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  # scale_y_continuous(breaks = c(0.50, 0.55, 0.60), limits = c(0.50, 0.60))+
  labs(x = "Max Tree Depth", y = "Mean Accuracy")

xgb_plot_maxdepth


##### XGB child_weight #####
#The larger min_child_weight is, the more conservative the algorithm will be.
#range: [0,∞]
#Higher values prevent a model from learning relations which might be highly specific to the particular sample selected for a tree.
#Too high values can lead to under-fitting.

xgbGrid_minchild <- expand.grid(nrounds = 50, #seq(50, 300, by = 50), # Boosting Iterations)
                       max_depth = 1, #seq(2, 10, by = 1), #(Max Tree Depth)
                       eta = 0.3, #c(0.01, 0.1, 0.5), #(Shrinkage)
                       gamma = 0, #(Minimum Loss Reduction)
                       colsample_bytree = 0.6, #seq(0, 10, by = 1), #(Subsample Ratio of Columns)
                       min_child_weight = seq(1,10, by = 1),
                       subsample = 1) #seq(0.2, 1, by = 0.2)) #(Subsample Percentage))

xgb_minchild <- function(seed){
  
  nzv_inTraining <- createDataPartition(nzv_df$dx, p = .8, list = FALSE)
  nzv_training <- nzv_df[ nzv_inTraining,]
  nzv_testing  <- nzv_df[-nzv_inTraining,]
  
  train(dx ~ ., data = nzv_training, 
        method = "xgbTree", 
        trControl = fitControl, 
        verbose = FALSE,
        tuneGrid = xgbGrid_minchild)
}


plan("multisession", workers = 8)
tic() #1 seed = 15 s
xgb_minchild_comb <- future_map(1:100, xgb_minchild, .options = furrr_options(seed=TRUE))
toc()
plan("sequential")




xgb_best_minchild <-data.frame(minchild = 0)
for(i in 1:100) {                                   
  new <- rep(i, ncol(xgb_best_minchild))                       
  xgb_best_minchild[i,] <- as.data.frame(t(paste0(xgb_minchild_comb[[i]][["bestTune"]][["min_child_weight"]])))
  xgb_best_minchild[nrow(xgb_best_minchild) + 1, ] <- new                   
}



xgb_best_minchild$count <- 1
xgb_best_minchild <- xgb_best_minchild[1:100,]

xgb_best_minchild <- aggregate(xgb_best_minchild$count,
                               by = list(xgb_best_minchild$minchild), FUN = sum) 
colnames(xgb_best_minchild) <- c("minchild", "count")

top_n(xgb_best_minchild, 3)


xgb_plot_minchild <- combine_hp_performance(xgb_minchild_comb)

xgb_plot_minchild <- plot_hp_performance(xgb_plot_minchild$dat, min_child_weight, Accuracy) + 
  geom_errorbar(ggplot2::aes(
    ymin = .data$ymin_metric,
    ymax = .data$ymax_metric),
    width = .001, color = "grey")+
  geom_line(color="black") +
  geom_point(shape=21, color="black", fill="black", size=1)+
  theme_classic()+
  # scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
  #               labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  # scale_y_continuous(breaks = c(0.50, 0.55, 0.60), limits = c(0.50, 0.60))+
  labs(x = "Minimum Sum of Instance Weight", y = "Mean Accuracy")

xgb_plot_minchild





##### XGB colsample_bytree #####
#is the subsample ratio of columns when constructing each tree. Subsampling occurs once for every tree constructed.
#range [0,1]

xgbGrid_colsample <- expand.grid(nrounds = 50, #seq(50, 300, by = 50), # Boosting Iterations)
                       max_depth = 1, #seq(2, 10, by = 1), #(Max Tree Depth)
                       eta = 0.3, #c(0.01, 0.1, 0.5), #(Shrinkage)
                       gamma = 0, #(Minimum Loss Reduction)
                       colsample_bytree = c(0,0.1,0.2, 0.3, 0.4, 0.5, 0.6,0.7, 0.8,0.9,1), #(Subsample Ratio of Columns)
                       min_child_weight = 1, #seq(0.5, 1.5, by = 0.5), #(Minimum Sum of Instance Weight)
                       subsample = 1) #seq(0.2, 1, by = 0.2)) #(Subsample Percentage))


xgb_colsample <- function(seed){
  
  nzv_inTraining <- createDataPartition(nzv_df$dx, p = .8, list = FALSE)
  nzv_training <- nzv_df[ nzv_inTraining,]
  nzv_testing  <- nzv_df[-nzv_inTraining,]
  
  train(dx ~ ., data = nzv_training, 
        method = "xgbTree", 
        trControl = fitControl, 
        verbose = FALSE,
        tuneGrid = xgbGrid_colsample)
}


plan("multisession", workers = 8)
tic() # 1 seed = 20 s
xgb_colsample_comb <- future_map(1:100, xgb_colsample,
                                         .options = furrr_options(seed=TRUE))
toc()
plan("sequential")




xgb_best_colsample <-data.frame(colsample = 0)
for(i in 1:100) {                                   
  new <- rep(i, ncol(xgb_best_colsample))                       
  xgb_best_colsample[i,] <- as.data.frame(t(paste0(xgb_colsample_comb[[i]][["bestTune"]][["colsample_bytree"]])))
  xgb_best_colsample[nrow(xgb_best_colsample) + 1, ] <- new                   
}

xgb_best_colsample$count <- 1
xgb_best_colsample <- xgb_best_colsample[1:100,]

xgb_best_colsample <- aggregate(xgb_best_colsample$count,
                          by = list(xgb_best_colsample$colsample), FUN = sum) 
colnames(xgb_best_colsample) <- c("colsample", "count")

top_n(xgb_best_colsample, 3)


xgb_plot_colsample <- combine_hp_performance(xgb_colsample_comb)

xgb_plot_colsample <- plot_hp_performance(xgb_plot_colsample$dat, colsample_bytree, Accuracy) + 
  geom_errorbar(ggplot2::aes(
    ymin = .data$ymin_metric,
    ymax = .data$ymax_metric),
    width = .001, color = "grey")+
  geom_line(color="black") +
  geom_point(shape=21, color="black", fill="black", size=1)+
  theme_classic()+
  # scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
  #               labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  # scale_y_continuous(breaks = c(0.50, 0.55, 0.60), limits = c(0.50, 0.60))+
  labs(x = "Subsample Ratio of Columns", y = "Mean Accuracy")

xgb_plot_colsample

##### XGB subsample #####
#It denotes the fraction of observations to be randomly samples for each tree.
#Subsample ratio of the training instances.
#Setting it to 0.5 means that XGBoost would randomly sample half of the training data prior to growing trees. - This will prevent overfitting.
#Subsampling will occur once in every boosting iteration.
#Lower values make the algorithm more conservative and prevents overfitting but too small values might lead to under-fitting.
#Typical values: 0.5-1
xgbGrid_subsample <- expand.grid(nrounds = 50, #seq(50, 300, by = 50), # Boosting Iterations)
                       max_depth = 1, #seq(2, 10, by = 1), #(Max Tree Depth)
                       eta = 0.3, #c(0.01, 0.1, 0.5), #(Shrinkage)
                       gamma = 0, #(Minimum Loss Reduction)
                       colsample_bytree = 0.6, #seq(0, 10, by = 1), #(Subsample Ratio of Columns)
                       min_child_weight = 1,#"seq(1, 10, by = 1),#seq(2, 30, by=3),#seq(0.5, 10, by = 1), #(Minimum Sum of Instance Weight)
                       subsample = seq(0.2, 1, by = 0.1)) #(Subsample Percentage))

xgb_subsample <- function(seed){
  
  nzv_inTraining <- createDataPartition(nzv_df$dx, p = .8, list = FALSE)
  nzv_training <- nzv_df[ nzv_inTraining,]
  nzv_testing  <- nzv_df[-nzv_inTraining,]
  
  train(dx ~ ., data = nzv_training, 
        method = "xgbTree", 
        trControl = fitControl, 
        verbose = FALSE,
        tuneGrid = xgbGrid_subsample)
}


plan("multisession", workers = 8)
tic()
xgb_subsample_comb <- future_map(1:100, xgb_subsample,
                                         .options = furrr_options(seed=TRUE))
toc()
plan("sequential")




xgb_best_subsample <-data.frame(subsample = 0)
for(i in 1:100) {                                   
  new <- rep(i, ncol(xgb_best_subsample))                       
  xgb_best_subsample[i,] <- as.data.frame(t(paste0(xgb_subsample_comb[[i]][["bestTune"]][["subsample"]])))
  xgb_best_subsample[nrow(xgb_best_subsample) + 1, ] <- new                   
}

xgb_best_subsample$count <- 1
xgb_best_subsample <- xgb_best_subsample[1:100,]

xgb_best_subsample <- aggregate(xgb_best_subsample$count,
                          by = list(xgb_best_subsample$subsample), FUN = sum) 
colnames(xgb_best_subsample) <- c("subsample", "count")

top_n(xgb_best_subsample, 3)


xgb_plot_subsample <- combine_hp_performance(xgb_subsample_comb)

xgb_plot_subsample <- plot_hp_performance(xgb_plot_subsample$dat, subsample, Accuracy) + 
  geom_errorbar(ggplot2::aes(
    ymin = .data$ymin_metric,
    ymax = .data$ymax_metric),
    width = .001, color = "grey")+
  geom_line(color="black") +
  geom_point(shape=21, color="black", fill="black", size=1)+
  theme_classic()+
  # scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
  #               labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  # scale_y_continuous(breaks = c(0.50, 0.55, 0.60), limits = c(0.50, 0.60))+
  labs(x = "Subsample Percentage", y = "Mean Accuracy")

xgb_plot_subsample

##### XGB gamma #####
#A node is split only when the resulting split gives a positive reduction in the loss function.
#Gamma specifies the minimum loss reduction required to make a split.
#It makes the algorithm conservative. The values can vary depending on the loss function and should be tuned.
#The larger gamma is, the more conservative the algorithm will be.
#Range: [0,∞]
xgbGrid_gamma <- expand.grid(nrounds = 50, #seq(50, 300, by = 50), # Boosting Iterations)
                             max_depth = 1, #seq(2, 10, by = 1), #(Max Tree Depth)
                             eta = 0.3, #c(0.01, 0.1, 0.5), #(Shrinkage)
                             gamma = seq(0,20, by = 1), #(Minimum Loss Reduction)
                             colsample_bytree = 0.6, #seq(0.2, 1, by = 0.2), #(Subsample Ratio of Columns)
                             min_child_weight = 1, #seq(0.5, 1.5, by = 0.5), #(Minimum Sum of Instance Weight)
                             subsample = 1) #seq(0.2, 1, by = 0.2)) #(Subsample Percentage))



xgb_gamma <- function(seed){
  
  nzv_inTraining <- createDataPartition(nzv_df$dx, p = .8, list = FALSE)
  nzv_training <- nzv_df[ nzv_inTraining,]
  nzv_testing  <- nzv_df[-nzv_inTraining,]
  
  train(dx ~ ., data = nzv_training,  
        method = "xgbTree", 
        trControl = fitControl, 
        verbose = FALSE,
        tuneGrid = xgbGrid_gamma)
}


plan("multisession", workers = 8)
tic() #1 seed = 35s
xgb_gamma_comb <- future_map(1:100, xgb_gamma,
                             .options = furrr_options(seed=TRUE))
toc()
plan("sequential")

xgb_best_gamma <-data.frame(gamma = 0)
for(i in 1:100) {                                   
  new <- rep(i, ncol(xgb_best_gamma))                       
  xgb_best_gamma[i,] <- as.data.frame(t(paste0(xgb_gamma_comb[[i]][["bestTune"]][["gamma"]])))
  xgb_best_gamma[nrow(xgb_best_gamma) + 1, ] <- new                   
}

xgb_best_gamma$count <- 1
xgb_best_gamma <- xgb_best_gamma[1:100,]

xgb_best_gamma <- aggregate(xgb_best_gamma$count,
                            by = list(xgb_best_gamma$gamma), FUN = sum) 
colnames(xgb_best_gamma) <- c("gamma", "count")

top_n(xgb_best_gamma, 3)


xgb_plot_gamma <- combine_hp_performance(xgb_gamma_comb)

xgb_plot_gamma <- plot_hp_performance(xgb_plot_gamma$dat, gamma, Accuracy) + 
  geom_errorbar(ggplot2::aes(
    ymin = .data$ymin_metric,
    ymax = .data$ymax_metric),
    width = .001, color = "grey")+
  geom_line(color="black") +
  geom_point(shape=21, color="black", fill="black", size=1)+
  theme_classic()+
  # scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
  #               labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  # scale_y_continuous(breaks = c(0.50, 0.55, 0.60), limits = c(0.50, 0.60))+
  labs(x = "Minimum Loss Reduction (gamma)", y = "Mean Accuracy")

xgb_plot_gamma







xgb_tune_plot <- plot_grid(xgb_plot_nrounds, xgb_plot_eta, xgb_plot_maxdepth, xgb_plot_minchild,
          xgb_plot_colsample, xgb_plot_subsample, xgb_plot_gamma, ncol = 2)

ggsave(plot = xgb_tune_plot, filename = "xgb_tune_plot.jpeg",
       path = "C:/Users/Berg/Desktop/Thesis/Results/XGB",
       dpi = 600, width = 15, height = 20, units = "cm")








#### Final model


xgbGrid <- expand.grid(nrounds = 5,          # Boosting Iterations)
                       max_depth = 1,         #(Max Tree Depth)
                       eta = 0.1,            #(Shrinkage)
                       gamma = 6,             #(Minimum Loss Reduction)
                       colsample_bytree = 0.1,  #(Subsample Ratio of Columns)
                       min_child_weight = 2,  #(Minimum Sum of Instance Weight)
                       subsample = 1)         #(Subsample Percentage))


xgb_final <- function(seed){
  
  nzv_df_inTraining <- createDataPartition(nzv_df$dx, p = .8, list = FALSE)
  nzv_df_training <- nzv_df[ nzv_df_inTraining,]
  nzv_df_testing  <- nzv_df[-nzv_df_inTraining,]
  
  train(dx ~ ., data = nzv_df_training, 
        method = "xgbTree", 
        trControl = fitControl, 
        verbose = FALSE,
        tuneGrid = xgbGrid)
}



plan("multisession", workers = 8)
tic() 
xgb_final_comb <- future_map(1:100, xgb_final, .options = furrr_options(seed=TRUE))
toc()
plan("sequential")











xgb_final_comb[[1]][["resample"]][["Accuracy"]]


xgb_final_comb_stats <-data.frame(accuracy = 0)
for(i in 1:100) {                                   
  new <- rep(i, ncol(xgb_final_comb_stats))                       
  xgb_final_comb_stats[i,] <- as.data.frame(t(paste0(xgb_final_comb[[i]][["resample"]][["Accuracy"]])))
  xgb_final_comb_stats[nrow(xgb_final_comb_stats) + 1, ] <- new                   
}


xgb_final_comb_stats <- as.data.frame(as.numeric(xgb_final_comb_stats[1:100,]))
colnames(xgb_final_comb_stats) <- "accuracy"
min(xgb_final_comb_stats$accuracy)
max(xgb_final_comb_stats$accuracy)
mean(xgb_final_comb_stats$accuracy)



models <- c(xgb_final_comb)
names <- c("Final AUC = 0.77")


model_xgb_final <- evalm(list1 = models, 
                        gnames = names, 
                        title = "XGboost", 
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


xgb_final <- model_xgb_final$roc + 
  scale_color_manual(values= c("#E41A1C", "#377EB8", "#4DAF4A", "#FF7F00")) + 
  theme_classic()+
  theme(legend.position = c(0.65, 0.22),
        legend.text = element_text(size=8), 
        legend.justification = "left",
        legend.title = element_blank()) 
xgb_final


ggsave(plot = xgb_final, filename = "xgb_final",
       path = "C:/Users/Berg/Desktop/Thesis/Results/XGB/",
       dpi = 600, width = 15, height = 10, units = "cm")










xgbGrid <- expand.grid(nrounds = 25,          # Boosting Iterations)
                         max_depth = 1,         #(Max Tree Depth)
                         eta = 0.1,            #(Shrinkage)
                         gamma = 1,             #(Minimum Loss Reduction)
                         colsample_bytree = 1,  #(Subsample Ratio of Columns)
                         min_child_weight = 2,  #(Minimum Sum of Instance Weight)
                         subsample = 1)         #(Subsample Percentage))


xgb_final <- function(seed){
  
  nzv_df_inTraining <- createDataPartition(nzv_df$dx, p = .8, list = FALSE)
  nzv_df_training <- nzv_df[ nzv_df_inTraining,]
  nzv_df_testing  <- nzv_df[-nzv_df_inTraining,]
  
  train(dx ~ ., data = nzv_df_training, 
        method = "xgbTree", 
        trControl = fitControl, 
        verbose = FALSE,
        tuneGrid = xgbGrid)
}



plan("multisession", workers = 8)
tic() 
xgb_final_comb <- future_map(1:100, xgb_final, .options = furrr_options(seed=TRUE))
toc()
plan("sequential")


models <- c(xgb_final_comb)
names <- c("XGB: AUC = 0.77")


model_xgb_final <- evalm(list1 = models, 
                           gnames = names, 
                           title = "XGboost", 
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


xgb_final <- model_xgb_final$roc + 
  scale_color_manual(values= c("#E41A1C", "#377EB8", "#4DAF4A", "#FF7F00")) + 
  theme_classic()+
  theme(legend.position = c(0.65, 0.22),
        legend.text = element_text(size=8), 
        legend.justification = "left",
        legend.title = element_blank()) 
xgb_final


ggsave(plot = xgb_final, filename = "xgb_final.jpeg",
       path = "C:/Users/Berg/Desktop/Thesis/Results/XGB/",
       dpi = 600, width = 15, height = 10, units = "cm")





xgb_final_comb[[1]][["resample"]][["Accuracy"]]


xgb_final_comb_stats <-data.frame(accuracy = 0)
for(i in 1:100) {                                   
  new <- rep(i, ncol(xgb_final_comb_stats))                       
  xgb_final_comb_stats[i,] <- as.data.frame(t(paste0(xgb_final_comb[[i]][["resample"]][["Accuracy"]])))
  xgb_final_comb_stats[nrow(xgb_final_comb_stats) + 1, ] <- new                   
}


xgb_final_comb_stats <- as.data.frame(as.numeric(xgb_final_comb_stats[1:100,]))
colnames(xgb_final_comb_stats) <- "accuracy"
min(xgb_final_comb_stats$accuracy)
max(xgb_final_comb_stats$accuracy)
mean(xgb_final_comb_stats$accuracy)



xgb.plot.tree(model = xgb_final_comb[[1]]$finalModel)
xgb.importance(model = xgb_final_comb[[2]]$finalModel)



##### Feature importance

names <- varImp(xgb_final_comb[[1]], scale = TRUE)$importance %>% 
  rownames_to_column() %>%
  select(-c(Overall))

xgb_imp <- as.data.frame(1:182)
for (i in 1:100){
  test <- varImp(xgb_final_comb[[i]], scale = TRUE)
  xgb_imp[,i+1] <- test[["importance"]][["Overall"]]
}

xgb_imp <- xgb_imp[,-1]
#colnames(xgb_imp) <- c("cv1", "cv2", "cv3","cv4","cv5","cv6","cv7","cv8","cv9","cv10")

xgb_imp$row_mean <- apply(xgb_imp[,1:100], 1, mean)

xgb_imp$genus <- names$rowname


xgb_imp_top10 <- xgb_imp %>% top_n(xgb_imp$row_mean, n = 9) %>% 
  select(-c(row_mean)) %>% 
  pivot_longer(-genus, names_to="CV", values_to="imp") %>% 
  mutate(imp = as.double(imp),
         genus = fct_reorder(genus, imp)) %>% 
  arrange(desc(imp))

xgb_imp_plot <- ggplot(xgb_imp_top10, aes(x = imp, y=genus, color = genus))+
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

ggsave(plot = xgb_imp_plot, filename = "xgb_feature_importance_plot.jpeg",
       path = "C:/Users/Berg/Desktop/Thesis/Results/XGB/",
       dpi = 600, width = 15, height = 8, units = "cm")

##### STATS ###########


models <- c(xgb_final_comb)
names <- c(" ")

xgb_final_comb_stats <- evalm(list1 = models, 
                              gnames = names,
                              silent = FALSE,
                              showplots = TRUE,
                              positive = "cancer")

stats <- as.data.frame(xgb_final_comb_stats[["stdres"]][[" "]])


TP <- stats[9,1]
FP <- stats[10,1]
TN <- stats[11,1]
FN <- stats[12,1]

accuracy = (TP+TN)/(TP+TN+FP+FN)

stats["accuracy",] <- c(accuracy, "NA" )

stats <- as.data.frame(t(stats))

stats <- stats %>% 
  select(-c("MCC", "Informedness", "FPR", "F1", "AUC-PR", "AUC-PRG"))


xgb_stats_final <- stats







