# Plot model performance

setwd("~/PEIfuture/Working")

library(data.table)
library(ggplot2)

fls <- list.files("./Data/modelOutputDiagnostics/", full.names = T)

dat <- data.frame()

for (i in fls) {
  print(i)
  d <- fread(i, select = c("sp",
                             "season",
                             "climate",
                             "method",
                             "Accuracy",
                             "Kappa",
                             "ROC",
                             "Sens",
                             "Spec",
                             "AUC",
                             "Precision",
                             "Recall",
                             "F",
                             "AccuracySD",
                             "KappaSD",
                             "ROCSD",
                             "SensSD",
                             "SpecSD",
                             "AUCSD",
                             "PrecisionSD",
                             "RecallSD",
                             "FSD"))

dat <- rbind(dat, d)

}

pdf("./Data/other/modelAUC.pdf", width = 8.5, height = 5, useDingbats = F)
ggplot(data = dat, aes(x = ROC, y = sp, color = method)) +
  geom_jitter(height = 0.3) +
  scale_y_discrete(labels = c("Antarctic fur seal",
                              "Sooty albatross",
                              "Indian yellow-nosed albatross",
                              "King penguin",
                              "Light-mantled albatross",
                              "Macaroni penguin",
                              "Northern giant petrel",
                              "Killer whale",
                              "Southern elephant seal",
                              "Subantarctic fur seal",
                              "Southern rockhopper penguin",
                              "Wandering albatross",
                              "White-chinned petrel"
                              )) +
  scale_color_brewer(type = "qual", palette = "Set1", name = "Algorithm") +
  facet_wrap(~season) +
  labs(x = "AUC", y = "Species") +
  theme_bw() +
  theme(axis.text = element_text(colour = "black"),
        plot.background = element_rect(fill = "white", colour = "white"))
dev.off()

val <- mean(dat[dat$season == "summer", ]$ROC)
val

dat[which(dat$ROC == val), ]$sp
dat[which(dat$ROC == val), ]$climate


# Mean by algorithm
mean(dat[dat$method == "rf", ]$ROC, na.rm = T)
sd(dat[dat$method == "rf", ]$ROC, na.rm = T)

mean(dat[dat$method == "gbm", ]$ROC, na.rm = T)
sd(dat[dat$method == "gbm", ]$ROC, na.rm = T)

mean(dat[dat$method == "gam", ]$ROC, na.rm = T)
sd(dat[dat$method == "gam", ]$ROC, na.rm = T)

mean(dat[dat$method == "svm", ]$ROC, na.rm = T)
sd(dat[dat$method == "svm", ]$ROC, na.rm = T)

mean(dat[dat$method == "ann", ]$ROC, na.rm = T)
sd(dat[dat$method == "ann", ]$ROC, na.rm = T)
