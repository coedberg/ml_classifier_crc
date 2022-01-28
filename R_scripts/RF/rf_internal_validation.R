
### RF prediction
rf_pred <- predict(rf_final_comb, newdata = df_validation, type = "prob")


rf_internal_pred <- rf_pred[[1]]
for (i in 2:100){
  rf_internal_pred <- (rf_internal_pred + rf_pred[[i]])
}
rf_internal_pred <- rf_internal_pred / i 

rf_internal_pred$obs <- df_validation$dx
rf_internal_pred$pred <- as.factor(ifelse(rf_internal_pred$cancer >= .5, "cancer", "normal"))

view(rf_internal_pred)
write_xlsx(
  rf_internal_pred,
  path = "C:/Users/Berg/Desktop/Thesis/Results/RF/rf_internal_pred.xlsx",
  col_names = TRUE,
  format_headers = TRUE,
  use_zip64 = FALSE
)

#confidence of model is not high, as they do not fall close to zero or one
wrong_pred <- subset(rf_internal_pred, obs != pred)

#Of the 50 samples tested, 14 of the prediction were wrong.

coinflip <- subset(rf_internal_pred, cancer > 0.45 & cancer < 0.55) 

# Of the 50 samples tested, 7 of these predictions are basically a coinflip 


strong_pred <- subset(rf_internal_pred, normal > 0.8 & cancer < 0.2 ) 
strong_pred2 <- subset(rf_internal_pred, cancer > 0.8 & normal < 0.2) 
strong_pred <- rbind(strong_pred, strong_pred2)


strong_but_wrong <- subset(strong_pred, obs != pred)
# 1 strong but wrong prediction 


rf_internal_pred_stats <- confusionMatrix(data = rf_internal_pred$pred, reference = rf_internal_pred$obs)



rf_internal_pred_stats[["table"]]
rf_cf_intval <- as.data.frame(rf_internal_pred_stats[["table"]])

write_xlsx(
  rf_cf_intval,
  path = "C:/Users/Berg/Desktop/Thesis/Results/RF/rf_cf_intval.xlsx",
  col_names = TRUE,
  format_headers = TRUE,
  use_zip64 = FALSE
)

rf_stats_int <- t(data.frame(Accuracy = rf_internal_pred_stats[["overall"]][["Accuracy"]],
                             Sensitivity = rf_internal_pred_stats[["byClass"]][["Sensitivity"]],
                             Specificity = rf_internal_pred_stats[["byClass"]][["Specificity"]],
                             F1 = rf_internal_pred_stats[["byClass"]][["F1"]],
                             NPV = rf_internal_pred_stats[["byClass"]][["Neg Pred Value"]],
                             PPV = rf_internal_pred_stats[["byClass"]][["Pos Pred Value"]]))



#BRIER SCORE
o = abs(as.numeric(df_validation$dx)-1)

p <-data.frame(V1 = 0)
for(i in 1:100) {                                   # Head of for-loop
  new <- rep(i, ncol(p))                       # Create new row
  p[i,1:50] <- t(as.numeric(paste0(rf_pred[[i]][,2])))
  p[nrow(p) + 1, ] <- new                   # Append new row
}
p <- p[1:100,]

p<- colMeans(p)

brier <- mean((o-p)^2)

brier_max = mean(p) * (1-mean(p))^2 + (1-mean(p)) * mean(p)^2

brier_scaled = (1 - (brier/ brier_max))*100



# Histogram visualisation of distributions

hist <- as.data.frame(rf_pred[[1]][["cancer"]])
colnames(hist) <- "cancer seed 1"

for(i in 2:100) {                                   # Head of for-loop
  new <- rep(i, nrow(hist))                       # Create new column
  hist[ , ncol(hist) + 1] <- as.data.frame(rf_pred[[i]][["cancer"]]) # Append new column
  colnames(hist)[ncol(hist)] <- paste0("cancer seed ", i)  # Rename column name
}

histogram <- pivot_longer(hist, cols = 1:100)

histogram[,2] <- histogram[,2]*100

histograms <- cbind(histogram, rf_internal_pred$obs)
colnames(histograms) <- c("seeds", "predictions", "observed")

vlines <- data.frame(xint = c(20, 45, 55, 80))

rf_histogram <- ggplot(histograms, aes(x=predictions, color=observed)) +
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


ggsave(plot = rf_histogram, filename = "rf_histogram.jpeg",
       path = "C:/Users/Berg/Desktop/Thesis/Results/rf/",
       dpi = 600, width = 15, height = 10, units = "cm")











hist2 <- as.data.frame(rf_pred[[1]][["cancer"]])
colnames(hist2) <- "cancer seed 1"

histogram2 <- hist2*100

histograms2 <- cbind(histogram2, rf_internal_pred$obs)
colnames(histograms2) <- c("predictions", "observed")

vlines <- data.frame(xint = c(20, 45, 55, 80))

rf_histogram2 <- ggplot(histograms2, aes(x=predictions, color=observed)) +
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


ggsave(plot = rf_histogram2, filename = "rf_histogram2.jpeg",
       path = "C:/Users/Berg/Desktop/Thesis/Results/rf/",
       dpi = 600, width = 15, height = 10, units = "cm")









