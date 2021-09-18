# Plot model performance

setwd("~/PEIfuture/Working")

library(data.table)
library(ggplot2)
library(patchwork)
library(dplyr)


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

#------------------------------------
# Performance summary

foo <- dplyr::filter(dat, method == "rf")

filter(foo, season == "winter") %>% 
  arrange(ROC) %>% 
  select(sp, climate, ROC, ROCSD)

mean(filter(foo, season == "summer")$ROC)
sd(filter(foo, season == "summer")$ROC)
#------------------------------------

#------------------------------------
# Performance
#------------------------------------

#------------------------------------
# AUC - ROC

p1 <- 
ggplot(data = dplyr::filter(dat, method == "rf"), aes(x = ROC, y = sp, color = climate)) +
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
  scale_color_brewer(type = "qual", palette = "Set1", name = "Climate\nmodel") +
  facet_wrap(~season) +
  labs(x = "AUC-ROC", y = "Species",
       subtitle = "a)") +
  theme_bw() +
  theme(axis.text = element_text(colour = "black"),
        plot.background = element_rect(fill = "white", colour = "white"))

#------------------------------------
# AUC - PR
 p2 <- 
ggplot(data = dplyr::filter(dat, method == "rf"), aes(x = AUC, y = sp, color = climate)) +
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
  scale_color_brewer(type = "qual", palette = "Set1", name = "Climate\nmodel") +
  facet_wrap(~season) +
  labs(x = "AUC-PR", y = "Species",
       subtitle = "b)") +
  theme_bw() +
  theme(axis.text = element_text(colour = "black"),
        plot.background = element_rect(fill = "white", colour = "white"))

p3 <- 
  ggplot(data = dplyr::filter(dat, method == "rf"), aes(x = F, y = sp, color = climate)) +
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
  scale_color_brewer(type = "qual", palette = "Set1", name = "Climate\nmodel") +
  facet_wrap(~season) +
  labs(x = "F measure", y = "Species") +
  theme_bw() +
  theme(axis.text = element_text(colour = "black"),
        plot.background = element_rect(fill = "white", colour = "white"))

p4 <- 
  ggplot(data = dplyr::filter(dat, method == "rf"), aes(x = Accuracy, y = sp, color = climate)) +
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
  scale_color_brewer(type = "qual", palette = "Set1", name = "Climate\nmodel") +
  facet_wrap(~season) +
  labs(x = "Accuracy", y = "Species") +
  theme_bw() +
  theme(axis.text = element_text(colour = "black"),
        plot.background = element_rect(fill = "white", colour = "white"))

p5 <- 
  ggplot(data = dplyr::filter(dat, method == "rf"), aes(x = Kappa, y = sp, color = climate)) +
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
  scale_color_brewer(type = "qual", palette = "Set1", name = "Climate\nmodel") +
  facet_wrap(~season) +
  labs(x = "Kappa", y = "Species") +
  theme_bw() +
  theme(axis.text = element_text(colour = "black"),
        plot.background = element_rect(fill = "white", colour = "white"))




pdf("./Data/other/modelPerformance.pdf", width = 8.5, height = 11, useDingbats = F)
print(p1 + p2 + p3 + p4 + p5 +
  plot_layout(guides = 'collect', ncol = 2))
dev.off()

# Correspondence between AUC ROC and AUC P-R
pdf("./Data/other/modelPerformance-ROCvAUC.pdf", width = 6, height = 6, useDingbats = F)
p_r <- ggplot(data = dplyr::filter(dat, method == "rf"), aes(x = ROC, y = AUC, color = climate)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point() +
  scale_color_brewer(type = "qual", palette = "Set1", name = "Climate\nmodel") +
  labs(x = "AUC-ROC", y = "AUC-PR",
       subtitle = "c)") +
  theme_bw() +
  theme(axis.text = element_text(colour = "black"),
        plot.background = element_rect(fill = "white", colour = "white"))
plot(p_r)
dev.off()

pdf("./Data/other/modelPerformanceForManuscript.pdf", width = 8.5, height = 11, useDingbats = F)
print(p1 + p2 + p_r +
        plot_layout(guides = 'collect', ncol = 1))
dev.off()

#------------------------------------
# Plot by algorithm
#------------------------------------

pdf("./Data/other/modelAUC_byalgorithm.pdf", width = 8.5, height = 5, useDingbats = F)
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
