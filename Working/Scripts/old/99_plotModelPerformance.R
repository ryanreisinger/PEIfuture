# Plot model performance

setwd("~/PEIfuture/Working")

library(ggplot2)

fls <- list.files("./Data/modelOutputDiagnostics/", full.names = T)
fls <- fls[grep("_rf", fls)]

dat <- do.call(rbind, lapply(fls, read.csv, stringsAsFactors = F))

pdf("./Data/other/modelAUC.pdf", width = 8.5, height = 5, useDingbats = F)
ggplot(data = dat, aes(x = ROC, y = sp, color = climate)) +
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
  labs(x = "AUC", y = "Species") +
  theme_bw() +
  theme(axis.text = element_text(colour = "black"),
        plot.background = element_rect(fill = "white", colour = "white"))
dev.off()

val <- mean(dat[dat$season == "summer", ]$ROC)
val

dat[which(dat$ROC == val), ]$sp
dat[which(dat$ROC == val), ]$climate
