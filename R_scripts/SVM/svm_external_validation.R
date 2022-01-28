
exval_df_healthy <- subset(exval_df, dx == "normal")

### svm prediction
svm_pred_ex <- predict(svm_final_comb, newdata = exval_df_healthy, type = "prob")


svm_internal_pred_ex <- svm_pred_ex[[1]]
for (i in 2:100){
  svm_internal_pred_ex <- (svm_internal_pred_ex + svm_pred_ex[[i]])
}
svm_internal_pred_ex <- svm_internal_pred_ex / i 

svm_internal_pred_ex$obs <- exval_df_healthy$dx
svm_internal_pred_ex$pred <- as.factor(ifelse(svm_internal_pred_ex$cancer >= .5, "cancer", "normal"))

view(svm_internal_pred_ex)


#confidence of model is not high, as they do not fall close to zero or one
wrong_pred_ex <- subset(svm_internal_pred_ex, obs != pred)

#Of the 11 samples tested, 7 of the prediction were wrong.

coinflip_ex <- subset(svm_internal_pred_ex, cancer > 0.45 & cancer < 0.65) 

# Of the 11 samples tested, 1 of these predictions are basically a coinflip 


strong_pred_ex <- subset(svm_internal_pred_ex, normal > 0.8 & cancer < 0.2 ) 
strong_pred_ex2 <- subset(svm_internal_pred_ex, cancer > 0.8 & normal < 0.2) 
strong_pred_ex <- rbind(strong_pred_ex, strong_pred_ex2)

# 7 strong predictions




svm_internal_pred_ex_stats <- confusionMatrix(data = svm_internal_pred_ex$pred, reference = svm_internal_pred_ex$obs)



svm_internal_pred_ex_stats[["table"]]
svm_cf_intval_ex <- as.data.frame(svm_internal_pred_ex_stats[["table"]])

write_xlsx(
  svm_cf_intval_ex,
  path = "C:/Users/Berg/Desktop/Thesis/Results/svm/svm_cf_intval_ex.xlsx",
  col_names = TRUE,
  format_headers = TRUE,
  use_zip64 = FALSE
)

svm_stats_int <- t(data.frame(Accuracy = svm_internal_pred_ex_stats[["overall"]][["Accuracy"]],
                             Sensitivity = svm_internal_pred_ex_stats[["byClass"]][["Sensitivity"]],
                             Specificity = svm_internal_pred_ex_stats[["byClass"]][["Specificity"]],
                             F1 = svm_internal_pred_ex_stats[["byClass"]][["F1"]],
                             NPV = svm_internal_pred_ex_stats[["byClass"]][["Neg Pred Value"]],
                             PPV = svm_internal_pred_ex_stats[["byClass"]][["Pos Pred Value"]]))



#BRIER SCORE

ft = svm_pred_ex[[1]][,2]
ot = as.numeric(exval_df_healthy$dx)-1
brier <-  mean((ft - ot)^2)

brier <- as.data.frame(brier)
for (i in 2:50){
  ft = svm_pred_ex[[i]][,2]
  brier <-  brier + mean((ft - ot)^2)
}
brier <- brier / i

brier

svm_stats_int <- rbind(svm_stats_int, t(brier))

svm_stats_int_ex <- as.data.frame(svm_stats_int)
write_xlsx(
  svm_stats_int_ex,
  path = "C:/Users/Berg/Desktop/Thesis/Results/svm/svm_stats_int_ex.xlsx",
  col_names = TRUE,
  format_headers = TRUE,
  use_zip64 = FALSE
)








# Histogram visualisation of distributions

hist <- as.data.frame(svm_pred_ex[[1]][["cancer"]])
colnames(hist) <- "cancer seed 1"

for(i in 2:100) {                                   # Head of for-loop
  new <- rep(i, nrow(hist))                       # Create new column
  hist[ , ncol(hist) + 1] <- as.data.frame(svm_pred_ex[[i]][["cancer"]]) # Append new column
  colnames(hist)[ncol(hist)] <- paste0("cancer seed ", i)  # Rename column name
}

histogram <- pivot_longer(hist, cols = 1:100)

histogram[,2] <- histogram[,2]*100

histograms <- cbind(histogram, svm_internal_pred$obs)
colnames(histograms) <- c("seeds", "predictions", "observed")

vlines <- data.frame(xint = c(20, 45, 55, 80))

svm_histogram_ex <- ggplot(histograms, aes(x=predictions, color=observed)) +
  geom_histogram(binwidth = 1, fill = "white")+
  geom_vline(data = vlines,
             aes(xintercept = c(20, 45, 55, 80)), 
             linetype = "dashed")+
  theme(legend.position="top")+
  scale_color_brewer(palette="Set1")+
  theme_classic()+
  labs(x="Risk predictions", y="Counts")+
  theme(plot.tag.position = "topleft",
        legend.position = c(0.9, 0.9))+
  scale_x_continuous(limits = c(0,100), 
                     breaks = seq(0,100, by = 5))


ggsave(plot = svm_histogram_ex, filename = "svm_histogram_ex.jpeg",
       path = "C:/Users/Berg/Desktop/Thesis/Results/svm/",
       dpi = 600, width = 15, height = 10, units = "cm")







##########################################################




exval_df_colitis <- subset(exval_df, dx == "cancer")

### svm prediction
svm_pred_ex_col <- predict(svm_final_comb, newdata = exval_df_colitis, type = "prob")


svm_internal_pred_ex_col <- svm_pred_ex_col[[1]]
for (i in 2:100){
  svm_internal_pred_ex_col <- (svm_internal_pred_ex_col + svm_pred_ex_col[[i]])
}
svm_internal_pred_ex_col <- svm_internal_pred_ex_col / i 

svm_internal_pred_ex_col$obs <- exval_df_colitis$dx
svm_internal_pred_ex_col$pred <- as.factor(ifelse(svm_internal_pred_ex_col$cancer >= .5, "cancer", "normal"))

view(svm_internal_pred_ex_col)
write_xlsx(
  svm_internal_pred_ex_col,
  path = "C:/Users/Berg/Desktop/Thesis/Results/svm/svm_internal_pred_ex_col.xlsx",
  col_names = TRUE,
  format_headers = TRUE,
  use_zip64 = FALSE
)

#confidence of model is not high, as they do not fall close to zero or one
wrong_pred_ex_col <- subset(svm_internal_pred_ex_col, obs != pred)

#Of the 50 samples tested, 4 samples are predicted as cancer, and 46 as normal

coinflip_ex_col <- subset(svm_internal_pred_ex_col, cancer > 0.45 & cancer < 0.65) 

# Of the 50 samples tested, 7 of these predictions are basically a coinflip 


strong_pred_ex_col <- subset(svm_internal_pred_ex_col, normal > 0.8 & cancer < 0.2 ) 
strong_pred_ex_col2 <- subset(svm_internal_pred_ex_col, cancer > 0.8 & normal < 0.2) 
strong_pred_ex_col <- rbind(strong_pred_ex_col, strong_pred_ex_col2)

# no strong predictions




svm_internal_pred_ex_col_stats <- confusionMatrix(data = svm_internal_pred_ex_col$pred, reference = svm_internal_pred_ex_col$obs)



svm_internal_pred_ex_col_stats[["table"]]
svm_cf_intval_ex_col <- as.data.frame(svm_internal_pred_ex_col_stats[["table"]])

write_xlsx(
  svm_cf_intval_ex_col,
  path = "C:/Users/Berg/Desktop/Thesis/Results/svm/svm_cf_intval_ex_col.xlsx",
  col_names = TRUE,
  format_headers = TRUE,
  use_zip64 = FALSE
)

svm_stats_int <- t(data.frame(Accuracy = svm_internal_pred_ex_col_stats[["overall"]][["Accuracy"]],
                             Sensitivity = svm_internal_pred_ex_col_stats[["byClass"]][["Sensitivity"]],
                             Specificity = svm_internal_pred_ex_col_stats[["byClass"]][["Specificity"]],
                             F1 = svm_internal_pred_ex_col_stats[["byClass"]][["F1"]],
                             NPV = svm_internal_pred_ex_col_stats[["byClass"]][["Neg Pred Value"]],
                             PPV = svm_internal_pred_ex_col_stats[["byClass"]][["Pos Pred Value"]]))



#BRIER SCORE

ft = svm_pred_ex_col[[1]][,2]
ot = as.numeric(exval_df_colitis$dx)-1
brier <-  mean((ft - ot)^2)

brier <- as.data.frame(brier)
for (i in 2:50){
  ft = svm_pred_ex_col[[i]][,2]
  brier <-  brier + mean((ft - ot)^2)
}
brier <- brier / i

brier

svm_stats_int <- rbind(svm_stats_int, t(brier))

svm_stats_int_ex_col <- as.data.frame(svm_stats_int)
write_xlsx(
  svm_stats_int_ex_col,
  path = "C:/Users/Berg/Desktop/Thesis/Results/svm/svm_stats_int_ex_col.xlsx",
  col_names = TRUE,
  format_headers = TRUE,
  use_zip64 = FALSE
)








# Histogram visualisation of distributions

hist <- as.data.frame(svm_pred_ex_col[[1]][["cancer"]])
colnames(hist) <- "cancer seed 1"

for(i in 2:100) {                                   # Head of for-loop
  new <- rep(i, nrow(hist))                       # Create new column
  hist[ , ncol(hist) + 1] <- as.data.frame(svm_pred_ex_col[[i]][["cancer"]]) # Append new column
  colnames(hist)[ncol(hist)] <- paste0("cancer seed ", i)  # Rename column name
}

histogram <- pivot_longer(hist, cols = 1:100)

histogram[,2] <- histogram[,2]*100

histograms <- cbind(histogram, svm_internal_pred$obs)
colnames(histograms) <- c("seeds", "predictions", "observed")

vlines <- data.frame(xint = c(20, 45, 55, 80))

svm_histogram_ex_col <- ggplot(histograms, aes(x=predictions, color=observed)) +
  geom_histogram(binwidth = 1, fill = "white")+
  geom_vline(data = vlines,
             aes(xintercept = c(20, 45, 55, 80)), 
             linetype = "dashed")+
  theme(legend.position="top")+
  scale_color_brewer(palette="Set1")+
  theme_classic()+
  labs(x="Risk predictions", y="Counts")+
  theme(plot.tag.position = "topleft",
        legend.position = c(0.9, 0.9))+
  scale_x_continuous(limits = c(0,100), 
                     breaks = seq(0,100, by = 5))


ggsave(plot = svm_histogram_ex_col, filename = "svm_histogram_ex_col.jpeg",
       path = "C:/Users/Berg/Desktop/Thesis/Results/svm/",
       dpi = 600, width = 15, height = 10, units = "cm")

















