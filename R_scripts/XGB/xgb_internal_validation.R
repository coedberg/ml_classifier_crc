
### xgb prediction
xgb_pred <- predict(xgb_final_comb, newdata = df_validation, type = "prob")


xgb_internal_pred <- xgb_pred[[1]]
for (i in 2:100){
  xgb_internal_pred <- (xgb_internal_pred + xgb_pred[[i]])
}
xgb_internal_pred <- xgb_internal_pred / i 

xgb_internal_pred$obs <- df_validation$dx
xgb_internal_pred$pred <- as.factor(ifelse(xgb_internal_pred$cancer >= .5, "cancer", "normal"))

view(xgb_internal_pred)


#confidence of model is not too high, as they do not fall close to zero or one
wrong_pred <- subset(xgb_internal_pred, obs != pred) 

#Of the 50 samples tested, 15 of the prediction were wrong.

coinflip <- subset(xgb_internal_pred, cancer > 0.45 & cancer < 0.55) 

# 2 sample prediction values are 0.45-0.55, suggesting that they are very close to a coinflip


strong_pred <- subset(xgb_internal_pred, normal > 0.8 & cancer < 0.2 ) 
strong_pred2 <- subset(xgb_internal_pred, cancer > 0.8 & normal < 0.2) 
strong_pred <- rbind(strong_pred, strong_pred2)

# 6 strong predictions

strong_but_wrong <- subset(strong_pred, obs != pred)
# 0 strong but wrong prediction 




xgb_internal_pred_stats <- confusionMatrix(data = xgb_internal_pred$pred, reference = xgb_internal_pred$obs)



xgb_internal_pred_stats[["table"]]

# no normal subjects were misclassified as cancer, however, 15 cancer subjects were misclassified as haveing a normal microbiota


xgb_cf_intval <- as.data.frame(xgb_internal_pred_stats[["table"]])


xgb_stats_int <- t(data.frame(Accuracy = xgb_internal_pred_stats[["overall"]][["Accuracy"]],
                             Sensitivity = xgb_internal_pred_stats[["byClass"]][["Sensitivity"]],
                             Specificity = xgb_internal_pred_stats[["byClass"]][["Specificity"]],
                             F1 = xgb_internal_pred_stats[["byClass"]][["F1"]],
                             NPV = xgb_internal_pred_stats[["byClass"]][["Neg Pred Value"]],
                             PPV = xgb_internal_pred_stats[["byClass"]][["Pos Pred Value"]]))



#BRIER SCORE

o = abs(as.numeric(df_validation$dx)-1)

p <-data.frame(V1 = 0)
for(i in 1:100) {                                   # Head of for-loop
  new <- rep(i, ncol(p))                       # Create new row
  p[i,1:50] <- t(as.numeric(paste0(xgb_pred[[i]][,2])))
  p[nrow(p) + 1, ] <- new                   # Append new row
}
p <- p[1:100,]

p<- colMeans(p)

brier <- mean((o-p)^2)

brier_max = mean(p) * (1-mean(p))^2 + (1-mean(p)) * mean(p)^2

brier_scaled = (1 - (brier/ brier_max))*100





# Histogram visualisation of distributions

hist <- as.data.frame(xgb_pred[[1]][["cancer"]])
colnames(hist) <- "cancer seed 1"

for(i in 2:100) {                                   # Head of for-loop
  new <- rep(i, nrow(hist))                       # Create new column
  hist[ , ncol(hist) + 1] <- as.data.frame(xgb_pred[[i]][["cancer"]]) # Append new column
  colnames(hist)[ncol(hist)] <- paste0("cancer seed ", i)  # Rename column name
}

histogram <- pivot_longer(hist, cols = 1:100)

histogram[,2] <- histogram[,2]*100

histograms <- cbind(histogram, xgb_internal_pred$obs)
colnames(histograms) <- c("seeds", "predictions", "observed")

vlines <- data.frame(xint = c(20, 45, 55, 80))

xgb_histogram <- ggplot(histograms, aes(x=predictions, color=observed)) +
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

  
ggsave(plot = xgb_histogram, filename = "xgb_histogram.jpeg",
       path = "C:/Users/Berg/Desktop/Thesis/Results/XGB/",
       dpi = 600, width = 15, height = 10, units = "cm")


