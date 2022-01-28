
exval_df_healthy <- subset(exval_df, dx == "normal")

### xgb prediction
xgb_pred_ex <- predict(xgb_final_comb, newdata = exval_df_healthy, type = "prob")


xgb_internal_pred_ex <- xgb_pred_ex[[1]]
for (i in 2:100){
  xgb_internal_pred_ex <- (xgb_internal_pred_ex + xgb_pred_ex[[i]])
}
xgb_internal_pred_ex <- xgb_internal_pred_ex / i 

xgb_internal_pred_ex$obs <- exval_df_healthy$dx
xgb_internal_pred_ex$pred <- as.factor(ifelse(xgb_internal_pred_ex$cancer >= .5, "cancer", "normal"))

view(xgb_internal_pred_ex)
write_xlsx(
  xgb_internal_pred_ex,
  path = "C:/Users/Berg/Desktop/Thesis/Results/xgb/xgb_external_predictions.xlsx",
  col_names = TRUE,
  format_headers = TRUE,
  use_zip64 = FALSE
)

#confidence of model is not high, as they do not fall close to zero or one
wrong_pred_ex <- subset(xgb_internal_pred_ex, obs != pred)

#Of the 11 samples tested, 0 of the prediction were wrong.

coinflip_ex <- subset(xgb_internal_pred_ex, cancer > 0.45 & cancer < 0.65) 

# Of the 11 samples tested, 1 of these predictions are basically a coinflip 


strong_pred_ex <- subset(xgb_internal_pred_ex, normal > 0.8 & cancer < 0.2 ) 
strong_pred_ex2 <- subset(xgb_internal_pred_ex, cancer > 0.8 & normal < 0.2) 
strong_pred_ex <- rbind(strong_pred_ex, strong_pred_ex2)

# no strong predictions


xgb_internal_pred_ex_stats <- confusionMatrix(data = xgb_internal_pred_ex$pred, reference = xgb_internal_pred_ex$obs)



xgb_internal_pred_ex_stats[["table"]]
xgb_cf_intval_ex <- as.data.frame(xgb_internal_pred_ex_stats[["table"]])



xgb_stats_int <- t(data.frame(Accuracy = xgb_internal_pred_ex_stats[["overall"]][["Accuracy"]],
                             Sensitivity = xgb_internal_pred_ex_stats[["byClass"]][["Sensitivity"]],
                             Specificity = xgb_internal_pred_ex_stats[["byClass"]][["Specificity"]],
                             F1 = xgb_internal_pred_ex_stats[["byClass"]][["F1"]],
                             NPV = xgb_internal_pred_ex_stats[["byClass"]][["Neg Pred Value"]],
                             PPV = xgb_internal_pred_ex_stats[["byClass"]][["Pos Pred Value"]]))



#BRIER SCORE
o = as.numeric(exval_df_healthy$dx)-1


p <-data.frame(V1 = 0)
for(i in 1:100) {                                   # Head of for-loop
  new <- rep(i, ncol(p))                       # Create new row
  p[i,1:11] <- t(as.numeric(paste0(xgb_pred_ex[[i]][,2])))
  p[nrow(p) + 1, ] <- new                   # Append new row
}
p <- p[1:100,]

p<- colMeans(p)

brier <- mean((o-p)^2)

brier_max = mean(p) * (1-mean(p))^2 + (1-mean(p)) * mean(p)^2

brier_scaled = (1 - (brier/ brier_max))*100









# Histogram visualisation of distributions

hist <- as.data.frame(xgb_pred_ex[[1]][["cancer"]])
colnames(hist) <- "cancer seed 1"

for(i in 2:100) {                                   # Head of for-loop
  new <- rep(i, nrow(hist))                       # Create new column
  hist[ , ncol(hist) + 1] <- as.data.frame(xgb_pred_ex[[i]][["cancer"]]) # Append new column
  colnames(hist)[ncol(hist)] <- paste0("cancer seed ", i)  # Rename column name
}

histogram <- pivot_longer(hist, cols = 1:100)

histogram[,2] <- histogram[,2]*100

histograms <- cbind(histogram, xgb_internal_pred$obs)
colnames(histograms) <- c("seeds", "predictions", "observed")

vlines <- data.frame(xint = c(20, 45, 55, 80))

xgb_histogram_ex <- ggplot(histograms, aes(x=predictions, color=observed)) +
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


ggsave(plot = xgb_histogram_ex, filename = "xgb_histogram_ex.jpeg",
       path = "C:/Users/Berg/Desktop/Thesis/Results/xgb/",
       dpi = 600, width = 15, height = 10, units = "cm")







##########################################################




exval_df_colitis <- subset(exval_df, dx == "cancer")

### xgb prediction
xgb_pred_ex_col <- predict(xgb_final_comb, newdata = exval_df_colitis, type = "prob")


xgb_internal_pred_ex_col <- xgb_pred_ex_col[[1]]
for (i in 2:100){
  xgb_internal_pred_ex_col <- (xgb_internal_pred_ex_col + xgb_pred_ex_col[[i]])
}
xgb_internal_pred_ex_col <- xgb_internal_pred_ex_col / i 

xgb_internal_pred_ex_col$obs <- exval_df_colitis$dx
xgb_internal_pred_ex_col$pred <- as.factor(ifelse(xgb_internal_pred_ex_col$cancer >= .5, "cancer", "normal"))

view(xgb_internal_pred_ex_col)
write_xlsx(
  xgb_internal_pred_ex_col,
  path = "C:/Users/Berg/Desktop/Thesis/Results/xgb/xgb_external_prediction_colitis.xlsx",
  col_names = TRUE,
  format_headers = TRUE,
  use_zip64 = FALSE
)

#confidence of model is not high, as they do not fall close to zero or one
wrong_pred_ex_col <- subset(xgb_internal_pred_ex_col, obs != pred)

#Of the 50 samples tested, 4 samples are predicted as cancer, and 46 as normal

coinflip_ex_col <- subset(xgb_internal_pred_ex_col, cancer > 0.45 & cancer < 0.65) 

# Of the 50 samples tested, 7 of these predictions are basically a coinflip 


strong_pred_ex_col <- subset(xgb_internal_pred_ex_col, normal > 0.8 & cancer < 0.2 ) 
strong_pred_ex_col2 <- subset(xgb_internal_pred_ex_col, cancer > 0.8 & normal < 0.2) 
strong_pred_ex_col <- rbind(strong_pred_ex_col, strong_pred_ex_col2)

# no strong predictions




xgb_internal_pred_ex_col_stats <- confusionMatrix(data = xgb_internal_pred_ex_col$pred, reference = xgb_internal_pred_ex_col$obs)



xgb_internal_pred_ex_col_stats[["table"]]
xgb_cf_intval_ex_col <- as.data.frame(xgb_internal_pred_ex_col_stats[["table"]])


xgb_stats_int <- t(data.frame(Accuracy = xgb_internal_pred_ex_col_stats[["overall"]][["Accuracy"]],
                             Sensitivity = xgb_internal_pred_ex_col_stats[["byClass"]][["Sensitivity"]],
                             Specificity = xgb_internal_pred_ex_col_stats[["byClass"]][["Specificity"]],
                             F1 = xgb_internal_pred_ex_col_stats[["byClass"]][["F1"]],
                             NPV = xgb_internal_pred_ex_col_stats[["byClass"]][["Neg Pred Value"]],
                             PPV = xgb_internal_pred_ex_col_stats[["byClass"]][["Pos Pred Value"]]))



#BRIER SCORE
o = abs(as.numeric(exval_df_colitis$dx)-2)


p <-data.frame(V1 = 0)
for(i in 1:100) {                                   # Head of for-loop
  new <- rep(i, ncol(p))                       # Create new row
  p[i,1:50] <- t(as.numeric(paste0(xgb_pred_ex_col[[i]][,2])))
  p[nrow(p) + 1, ] <- new                   # Append new row
}
p <- p[1:100,]

p<- colMeans(p)

brier <- mean((o-p)^2)

brier_max = mean(p) * (1-mean(p))^2 + (1-mean(p)) * mean(p)^2

brier_scaled = (1 - (brier/ brier_max))*100





# Histogram visualisation of distributions

hist <- as.data.frame(xgb_pred_ex_col[[1]][["cancer"]])
colnames(hist) <- "cancer seed 1"

for(i in 2:100) {                                   # Head of for-loop
  new <- rep(i, nrow(hist))                       # Create new column
  hist[ , ncol(hist) + 1] <- as.data.frame(xgb_pred_ex_col[[i]][["cancer"]]) # Append new column
  colnames(hist)[ncol(hist)] <- paste0("cancer seed ", i)  # Rename column name
}

histogram <- pivot_longer(hist, cols = 1:100)

histogram[,2] <- histogram[,2]*100

histograms <- cbind(histogram, xgb_internal_pred$obs)
colnames(histograms) <- c("seeds", "predictions", "observed")

vlines <- data.frame(xint = c(20, 45, 55, 80))

xgb_histogram_ex_col <- ggplot(histograms, aes(x=predictions, color=observed)) +
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


ggsave(plot = xgb_histogram_ex_col, filename = "xgb_histogram_ex_col.jpeg",
       path = "C:/Users/Berg/Desktop/Thesis/Results/xgb/",
       dpi = 600, width = 15, height = 10, units = "cm")

















