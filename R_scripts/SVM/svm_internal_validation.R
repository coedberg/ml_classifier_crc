
### svm prediction
svm_pred <- predict(svm_final_comb, newdata = df_validation, type = "prob")


svm_internal_pred <- svm_pred[[1]]
for (i in 2:100){
  svm_internal_pred <- (svm_internal_pred + svm_pred[[i]])
}
svm_internal_pred <- svm_internal_pred / i 

svm_internal_pred$obs <- df_validation$dx
svm_internal_pred$pred <- as.factor(ifelse(svm_internal_pred$cancer >= .5, "cancer", "normal"))

view(svm_internal_pred)


#confidence of model is not too high, as they do not fall close to zero or one
wrong_pred <- subset(svm_internal_pred, obs != pred) 

#Of the 50 samples tested, 20 of the prediction were wrong.

coinflip <- subset(svm_internal_pred, cancer > 0.45 & cancer < 0.55) 

# 9 sample prediction values are 0.45-0.55, suggesting that they are very close to a coinflip


strong_pred <- subset(svm_internal_pred, normal > 0.8 & cancer < 0.2 ) 
strong_pred2 <- subset(svm_internal_pred, cancer > 0.8 & normal < 0.2) 
strong_pred <- rbind(strong_pred, strong_pred2)

# 9 strong predictions

strong_but_wrong <- subset(strong_pred, obs != pred)
# 1 strong but wrong prediction 


svm_internal_pred_stats <- confusionMatrix(data = svm_internal_pred$pred, reference = svm_internal_pred$obs)



svm_internal_pred_stats[["table"]]

# 9 TP, 7 FP, 13 FN and 21 TN 

svm_cf_intval <- as.data.frame(svm_internal_pred_stats[["table"]])

write_xlsx(
  svm_cf_intval,
  path = "C:/Users/Berg/Desktop/Thesis/Results/svm/svm_cf_intval.xlsx",
  col_names = TRUE,
  format_headers = TRUE,
  use_zip64 = FALSE
)

svm_stats_int <- t(data.frame(Accuracy = svm_internal_pred_stats[["overall"]][["Accuracy"]],
                             Sensitivity = svm_internal_pred_stats[["byClass"]][["Sensitivity"]],
                             Specificity = svm_internal_pred_stats[["byClass"]][["Specificity"]],
                             F1 = svm_internal_pred_stats[["byClass"]][["F1"]],
                             NPV = svm_internal_pred_stats[["byClass"]][["Neg Pred Value"]],
                             PPV = svm_internal_pred_stats[["byClass"]][["Pos Pred Value"]]))



#BRIER SCORE
o = abs(as.numeric(df_validation$dx)-1)

p <-data.frame(V1 = 0)
for(i in 1:100) {                                   # Head of for-loop
  new <- rep(i, ncol(p))                       # Create new row
  p[i,1:50] <- t(as.numeric(paste0(svm_pred[[i]][,2])))
  p[nrow(p) + 1, ] <- new                   # Append new row
}
p <- p[1:100,]

p<- colMeans(p)

brier <- mean((o-p)^2)

brier_max = mean(p) * (1-mean(p))^2 + (1-mean(p)) * mean(p)^2

brier_scaled = (1 - (brier/ brier_max))*100








hist2 <- as.data.frame(svm_pred[[1]][["cancer"]])
colnames(hist2) <- "cancer seed 1"

histogram2 <- hist2*100

histograms2 <- cbind(histogram2, svm_internal_pred$obs)
colnames(histograms2) <- c("predictions", "observed")

vlines <- data.frame(xint = c(20, 45, 55, 80))

svm_histogram2 <- ggplot(histograms2, aes(x=predictions, color=observed)) +
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


ggsave(plot = svm_histogram2, filename = "svm_histogram2.jpeg",
       path = "C:/Users/Berg/Desktop/Thesis/Results/svm/",
       dpi = 600, width = 15, height = 10, units = "cm")


# Histogram visualisation of distributions

hist <- as.data.frame(svm_pred[[1]][["cancer"]])
colnames(hist) <- "cancer seed 1"

for(i in 2:100) {                                   # Head of for-loop
  new <- rep(i, nrow(hist))                       # Create new column
  hist[ , ncol(hist) + 1] <- as.data.frame(svm_pred[[i]][["cancer"]]) # Append new column
  colnames(hist)[ncol(hist)] <- paste0("cancer seed ", i)  # Rename column name
}

histogram <- pivot_longer(hist, cols = 1:100)

histogram[,2] <- histogram[,2]*100

histograms <- cbind(histogram, svm_internal_pred$obs)
colnames(histograms) <- c("seeds", "predictions", "observed")


vlines <- data.frame(xint = c(20, 45, 55, 80))

svm_histogram <- ggplot(histograms, aes(x=predictions, color=observed)) +
  geom_histogram(binwidth = 1, fill = "white")+
  geom_vline(data = vlines,
             aes(xintercept = xint), 
             linetype = "dashed")+
  theme(legend.position="top")+
  scale_color_brewer(palette="Set1")+
  theme_classic()+
  labs(x="Risk predictions", y="Counts")+
  theme(plot.tag.position = "topleft",
        legend.position = c(0.9, 0.9))+
  scale_x_continuous(limits = c(0,100), 
                     breaks = seq(0,100, by = 5))

  
ggsave(plot = svm_histogram, filename = "svm_histogram.jpeg",
       path = "C:/Users/Berg/Desktop/Thesis/Results/svm/",
       dpi = 600, width = 15, height = 10, units = "cm")

