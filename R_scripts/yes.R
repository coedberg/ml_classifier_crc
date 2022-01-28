
models <- c(xgb_df_evalres_comb )
names <- c("459 Features")

model_res_xgb_459 <- evalm(list1 = models, 
                           gnames = names,
                           silent = TRUE,
                           showplots = FALSE)

####

models <- c(xgb_zv_evalres_comb )
names <- c("396 Features")

model_res_xgb_396 <- evalm(list1 = models, 
                           gnames = names,
                           silent = TRUE,
                           showplots = FALSE)


####

models <- c(xgb_nzv_evalres_comb )
names <- c("183 Features")

model_res_xgb_183 <- evalm(list1 = models, 
                           gnames = names,
                           silent = TRUE,
                           showplots = FALSE)


####

models <- c(xgb_nzv_strict_evalres_comb )
names <- c("30 Features")

model_res_xgb_30 <- evalm(list1 = models, 
                          gnames = names,
                          silent = TRUE,
                          showplots = FALSE)
####

xgb_features <- rbind(model_res_xgb_459[["roc"]][["data"]], model_res_xgb_396[["roc"]][["data"]],
                      model_res_xgb_183[["roc"]][["data"]], model_res_xgb_30[["roc"]][["data"]])



xgb_feature_plot2 <- ggplot(xgb_features2, aes(x = FPR, y = SENS, color = Group)) + 
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

legend_shared <- get_legend(
  xgb_feature_plot2 + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)


models <- c(rf_df_evalres_comb )
names <- ("AUC = 0.80")

model_res_rf_459 <- evalm(list1 = models, 
                          gnames = names,
                          silent = TRUE,
                          showplots = FALSE)

####

models <- c(rf_zv_evalres_comb )
names <- c("AUC = 0.77")

model_res_rf_396 <- evalm(list1 = models, 
                          gnames = names,
                          silent = TRUE,
                          showplots = FALSE)


####

models <- c(rf_nzv_evalres_comb )
names <- c("AUC = 0.80 ")

model_res_rf_183 <- evalm(list1 = models, 
                          gnames = names,
                          silent = TRUE,
                          showplots = FALSE)


####

models <- c(rf_nzv_strict_evalres_comb )
names <- c("AUC = 0.60")

model_res_rf_30 <- evalm(list1 = models, 
                         gnames = names,
                         silent = TRUE,
                         showplots = FALSE)
####

rf_features <- rbind(model_res_rf_459[["roc"]][["data"]], model_res_rf_396[["roc"]][["data"]],
                     model_res_rf_183[["roc"]][["data"]], model_res_rf_30[["roc"]][["data"]])





################################################################################


models <- c(svm_df_evalres_comb )
names <- c("AUC = 0.55")

model_res_svm_459 <- evalm(list1 = models, 
                           gnames = names,
                           silent = TRUE,
                           showplots = FALSE)

####

models <- c(svm_zv_evalres_comb )
names <- c("AUC = 0.63")

model_res_svm_396 <- evalm(list1 = models, 
                           gnames = names,
                           silent = TRUE,
                           showplots = FALSE)


####

models <- c(svm_nzv_evalres_comb )
names <- c("AUC = 0.68")

model_res_svm_183 <- evalm(list1 = models, 
                           gnames = names,
                           silent = TRUE,
                           showplots = FALSE)


####

models <- c(svm_nzv_strict_evalres_comb )
names <- c("AUC = 0.57")

model_res_svm_30 <- evalm(list1 = models, 
                          gnames = names,
                          silent = TRUE,
                          showplots = FALSE)
####

svm_features <- rbind(model_res_svm_459[["roc"]][["data"]], model_res_svm_396[["roc"]][["data"]],
                      model_res_svm_183[["roc"]][["data"]], model_res_svm_30[["roc"]][["data"]])



################################################################################

models <- c(xgb_df_evalres_comb )
names <- c("AUC = 0.78")

model_res_xgb_459 <- evalm(list1 = models, 
                           gnames = names,
                           silent = TRUE,
                           showplots = FALSE)

####

models <- c(xgb_zv_evalres_comb )
names <- c("AUC = 0.76")

model_res_xgb_396 <- evalm(list1 = models, 
                           gnames = names,
                           silent = TRUE,
                           showplots = FALSE)


####

models <- c(xgb_nzv_evalres_comb )
names <- c("AUC = 0.79")

model_res_xgb_183 <- evalm(list1 = models, 
                           gnames = names,
                           silent = TRUE,
                           showplots = FALSE)


####

models <- c(xgb_nzv_strict_evalres_comb )
names <- c("AUC = 0.55")

model_res_xgb_30 <- evalm(list1 = models, 
                          gnames = names,
                          silent = TRUE,
                          showplots = FALSE)
####

xgb_features <- rbind(model_res_xgb_459[["roc"]][["data"]], model_res_xgb_396[["roc"]][["data"]],
                      model_res_xgb_183[["roc"]][["data"]], model_res_xgb_30[["roc"]][["data"]])


################################################################################




xgb_feature_plot2 <- ggplot(xgb_features2, aes(x = FPR, y = SENS, color = Group)) + 
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

legend_shared <- get_legend(
  xgb_feature_plot2 + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)
################################################################################

rf_feature_plot <- ggplot(rf_features, aes(x = FPR, y = SENS, color = Group)) + 
  geom_line(size = 0.7) +
  geom_abline(intercept = 0, slope = 1, colour = "grey", linetype = 1,
              size = 0.75) +
  coord_equal() +
  theme_bw() +
  xlab('False positive rate') +
  ylab('True positive rate') +
  ggtitle("RF")+
  scale_color_manual(values= c("#377EB8","#E41A1C", "#4DAF4A", "#FF7F00")) + 
  theme_classic()+
  theme(legend.position = c(0.65, 0.22),
        legend.text = element_text(size=8), 
        legend.justification = "left",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))


svm_feature_plot <- ggplot(svm_features, aes(x = FPR, y = SENS, color = Group)) + 
  geom_line(size = 0.7) +
  geom_abline(intercept = 0, slope = 1, colour = "grey", linetype = 1,
              size = 0.75) +
  coord_equal() +
  theme_bw() +
  xlab('False positive rate') +
  ylab('True positive rate') +
  ggtitle("SVM")+
  scale_color_manual(values= c("#377EB8","#E41A1C", "#4DAF4A", "#FF7F00")) + 
  theme_classic()+
  theme(legend.position = c(0.65, 0.22),
        legend.text = element_text(size=8), 
        legend.justification = "left",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))


xgb_feature_plot <- ggplot(xgb_features, aes(x = FPR, y = SENS, color = Group)) + 
  geom_line(size = 0.7) +
  geom_abline(intercept = 0, slope = 1, colour = "grey", linetype = 1,
              size = 0.75) +
  coord_equal() +
  theme_bw() +
  xlab('False positive rate') +
  ylab('True positive rate') +
  ggtitle("XGB")+
  scale_color_manual(values= c("#377EB8","#E41A1C", "#4DAF4A", "#FF7F00")) + 
  theme_classic()+
  theme(legend.position = c(0.65, 0.22),
        legend.text = element_text(size=8), 
        legend.justification = "left",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))


################################################################################

plot_a<- plot_grid(rf_feature_plot, xgb_feature_plot, svm_feature_plot, ncol = 3)


ggsave(plot = plot_a, filename = "shared.jpeg",
       path = "C:/Users/Berg/Desktop/Thesis/Results",
       dpi = 600, width = 30, height = 10, units = "cm")
