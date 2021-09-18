# Compare present and future species clusters

setwd("~/PEIfuture/Working")

library(raster)
library(apcluster)
library(vegan)
library(magrittr)
library(dendextend)

#-------------------------------------------------------------------
# Common variables
source("./Scripts/00 - Project variables.R")

spNamesSummer <- spNamesSummer[spNamesSummer != "GHA"]

# Scenario
which_scenario <- "rcp45"

if (which_scenario == "rcp45") {
  scenario_label = "RCP 4.5"
} else {
  scenario_label = "RCP 8.5"
}

# Raster for cropping
rst.crop <- raster(res = 0.1, xmn = -10, xmx = 70, ymn = -75, ymx = -30, crs = "+proj=longlat +datum=WGS84")

# Function for habitat importance
suitability_to_percentiles <- function(x, N=21) {
  ## expect that x is a raster containing predicted usage probability (suitability multiplied by availability)
  cell_areas <- raster::values(area(x))
  vals <- raster::values(x)
  total_area <- sum(cell_areas, na.rm = T)
  tst <- seq(min(vals,na.rm=TRUE),max(vals,na.rm=TRUE),length.out=N)
  ## calculate the percentage of area corresponding to each of these tst values
  s2p <- function(z) sum(cell_areas[which(vals<=z)])/total_area*100
  arp <- vapply(tst,s2p,FUN.VALUE=1)
  values(x) <- approx(tst,arp,vals)$y
  x
}

#-------------------------------------------------------------------
# For each climate model
# this.climate <- all.climate.mods[1]

for (m in all.climate.mods) {
  this.climate <- m
  print(m)
  
  #-------------------------------------------------------------------
  
  
  #-------------------------------------------------------------------
  # Get data together for all species
  
  for (i in c("summer", "winter")) {
    
    this.season <- i
    
    if(this.season == "summer") {
      these_species <- spNamesSummer
    } else {
      these_species <- spNamesWinter
    }
    
    his_stk <- stack()
    fut_stk <- stack()
    
    for (k in 1:length(these_species)) {
      
      this.species <- these_species[k]
      
      
      # Get files
      his_rst <- raster(
        paste0("./Data/predictionOutput/rasters/pred_", this.species, "_", this.climate, "_", this.season, "_historical.grd"))
      
      fut_rst <- raster(
        paste0("./Data/predictionOutput/rasters/pred_", this.species, "_", this.climate, "_", this.season, "_future_", which_scenario, ".grd"))
      
      # Crop
      his_rst <- crop(his_rst, rst.crop)
      fut_rst <- crop(fut_rst, rst.crop)
      
      # Importance
      his_rst <- suitability_to_percentiles(his_rst)
      fut_rst <- suitability_to_percentiles(fut_rst)
      
      # Add to stack
      his_stk <- stack(his_stk, his_rst)
      fut_stk <- stack(fut_stk, fut_rst)
      
    }
    
    names(his_stk) <- these_species
    names(fut_stk) <- these_species
    
    
    #------------------------------------------
    ## Cluster analysis
    
    ## Extract species scores
    hold.hist <- as.data.frame(his_stk, xy = TRUE)[,-(1:2)]
    hold.fut <- as.data.frame(fut_stk, xy = TRUE)[,-(1:2)]
    
    ## Cluster
    hist.clust <- apclusterL(negDistMat(r=2), t(hold.hist), frac = 0.1, sweeps = 5)
    hist.clusters <- labels(hist.clust, type = "enum")
    #hold.hist$cluster <- hist.clusters
    
    fut.clust <- apclusterL(negDistMat(r=2), t(hold.fut), frac = 0.1, sweeps = 5)
    fut.clusters <- labels(fut.clust, type = "enum")
    #hold.fut$cluster <- fut.clusters
    
    hist.dist <- vegdist(x=t(hold.hist), method = "canberra", na.rm = TRUE)
    hist.clust <- hclust(d=hist.dist, method="average")
    
    #get the CCC to compare methods - correlation between distance matrix and the clustering matrix
    hist.coph <- cophenetic(hist.clust)
    cor(hist.dist, hist.coph)
    
    #plot
    plot(hist.clust, hang=-1)
    
    fut.dist <- vegdist(x=t(hold.fut), method = "canberra", na.rm = T)
    fut.clust <- hclust(d=fut.dist, method="average")
    
    #get the CCC to compare methods - correlation between distance matrix and the clustering matrix
    fut.coph <- cophenetic(fut.clust)
    cor(fut.dist, fut.coph)
    
    #plot
    plot(fut.clust, hang=-1)
    
    #------------------------------------------
    ## Cluster differences
    #http://www.sthda.com/english/articles/28-hierarchical-clustering-essentials/91-comparing-dendrograms-essentials/
    #https://cran.r-project.org/web/packages/dendextend/vignettes/introduction.html
    
    dHis <- dist(x = t(hold.hist), method = "canberra") %>%
      hclust(., method = "average") %>%
      as.dendrogram(.)
    
    dFut <- dist(x = t(hold.fut), method = "canberra") %>%
      hclust(., method = "average") %>%
      as.dendrogram(.)
    
    dl <- dendlist(dHis, dFut)
    
    # pdf("./plots/tanglegram.pdf", height = 5, width = 6)
    # tanglegram(dl,
    #            sort = TRUE,
    #            common_subtrees_color_lines = FALSE,
    #            highlight_distinct_edges  = FALSE,
    #            highlight_branches_lwd = FALSE)
    # dev.off()
    
    # Entanglement (lower is better)
    entanglement(dl)
    
    # Baker's gamma correlation (values near -1 or +1 indicate similarity)
    cor_bakers_gamma(dl)
    
    # Permutation test of Baker's gamma
    the_cor <- cor_bakers_gamma(dHis, dFut)
    
    R <- 1000
    cor_bakers_gamma_results <- numeric(R)
    dend_mixed <- dFut
    for(i in 1:R) {
      dend_mixed <- sample.dendrogram(dend_mixed, replace = FALSE)
      cor_bakers_gamma_results[i] <- cor_bakers_gamma(dFut, dend_mixed)
    }
    
    pdf(paste0("./Data/dendrogram_plots_bakers/bakers", "_", "_", this.climate, "_", this.season, "_", which_scenario, ".pdf"),
        height = 5, width = 5)
    plot(density(cor_bakers_gamma_results),
         main = paste0("Baker's gamma distribution under H0\n", this.climate, " - ", this.season, " - ", scenario_label),
         xlim = c(-1,1))
    abline(v = 0, lty = 2)
    abline(v = the_cor, lty = 2, col = 2)
    legend("topleft", legend = c("Observed correlation"), fill = c(2,4))
    round(sum(the_cor < cor_bakers_gamma_results)/ R, 4)
    title(sub = paste("One sided p-value:",
                      "cor =",  round(sum(the_cor < cor_bakers_gamma_results)/ R, 4)
    ))
    dev.off()
    
    pdf(paste0("./Data/dendrogram_plots_tanglegram/tanglegram", "_", this.climate, "_", this.season, "_", which_scenario, ".pdf"),
        height = 3.5,
        width = 4.5)
    tanglegram(dl,
               sort = TRUE,
               common_subtrees_color_lines = FALSE,
               highlight_distinct_edges  = FALSE,
               highlight_branches_lwd = FALSE,
               main = this.climate,
               cex_main = 1,
               # sub = this.season,
               sub = paste0("R = ", round(the_cor, 2), ". p = ",  round(sum(the_cor < cor_bakers_gamma_results)/ R, 4)),
               main_left = "",
               main_right = ""
    )
    dev.off()
    
    
    # if (this.season == "summer") {
    #   his_summer <- his_stk
    #   fut_summer <- fut_stk
    # } else {
    #   his_winter <- his_stk
    #   fut_winter <- fut_stk
    # }
    
  }
  
}