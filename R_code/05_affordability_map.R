# lead libraries
library(sp)
library(rgdal)
library(raster)
library(dplyr)
library(sf)
library(rgeos)
library(MetaLandSim)
library(migflow)
library(igraph)

data_dir = "E:/backup/madagascar_cluster/scales"#"C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/madagascar/data/"
# captain_dir = "C:/Users/kdh10kg/Documents/github/captain-project/captain-dev/"

data_dir = "E:/backup/madagascar_cluster/scales"#"C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/madagascar/data/"

# /48139)*100
budget = 48139
pop = 40000000000000
loc = c("MDG_adm0","MDG_adm1","MDG_adm2","MDG_adm3","MDG_adm4")
run = c("equal_cost", "pop_cost","area_cost", "biodiversity_cost") #"equal_cost", "biodiversity_cost"
runname = c("equally", "by population","by area", "by biodiversity")
locname = c("national","province","region","district","commune")


filepath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/madagascar/data/captain/population_cost_data/5km"
pathi = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/madagascar"

Planning_Units_ZONES = read.csv(file.path(filepath, "Planning_units_zones_pop_5km.csv"))


##################################
# plot unspent money


#######################
# plot unspent money as 4 plot
subplot_label= c("(a)","(b)","(c)","(d)")

# for(runi in 1:length(run)){
#   for(loci in 1:length(loc)){
#
#     pts = read.csv(file.path(data_dir, loc[loci],
#                              paste0("summary_PAs_",loc[loci],"_",run[runi],".csv")))
#

    # # which cells are too expensive
    # grDevices::png(file = file.path(pathi, paste0("unaffordable_",loc[loci],"_",run[runi],".png")),
    #                width = 110, height = 200, units='mm', res = 300)
    #
    # par(mfrow=c(1,1), mar = c(5, 4, 4, 2),oma = c(0, 0, 0, 0))
    # plot(pts$Coord_x,
    #      pts$Coord_y,
    #      pch=15, cex=0.3,
    #      xlab= "Longitude",
    #      ylab = "Latitude",
    #      main = "",
    #      col = viridis::viridis(100)[pts$Unaffordable+1])
    # # col =  data$Remaining_Budget)
    #
    # legend("topleft",
    #        legend=c("unaffordable","affordable"),
    #        fill = c("darkorchid4", "gold2"),
    # )
    #
    # dev.off()
    #

    #########
#     # do the map
#     grDevices::png(file = file.path(filepath, paste0("budget_remaining_",loc[loci],"_",run[runi],".png")),
#                    width = 110, height = 200, units='mm', res = 300)
#
#     par(mfrow=c(1,1), mar = c(5, 4, 4, 2),oma = c(0, 0, 0, 0))
#     plot(pts$Coord_x,
#          pts$Coord_y,
#          pch=15, cex=0.3,
#          xlab= "Longitude",
#          ylab = "Latitude",
#          main = "",
#          col = viridis::viridis(max(pts$Remaining_Budget,na.rm=T)+1)[round(pts$Remaining_Budget)+1])
#     # col =  data$Remaining_Budget)
#
#     quantiles = summary(pts$Remaining_Budget)
#     legend("topleft",
#            legend=c(round(quantiles[[1]]),
#                     round(quantiles[[2]]),
#                     round(quantiles[[3]]),
#                     round(quantiles[[4]]),
#                     round(quantiles[[5]])),
#            fill = viridis::viridis(101)[c(1,25,50,75,100)],
#     )
#
#     dev.off()
#   }
# }




for (loci in 1:length(loc)){

  # which cells are too expensive
  grDevices::png(file = file.path(pathi,paste0("unaffordable_",loc[loci],".png")),
                 width = 180, height = 280, units='mm', res = 300)
  par(mfrow=c(2,2), mar=c(2,2,0.5,0.5), oma=c(5,5,0,0))
  #par(mfrow=c(2,2), mar = c(5, 4, 4, 2),oma = c(0, 0, 0, 0))

  for(runi in 1:length(run)){

    # file = paste0(loc[loci],"/all_results_",loc[loci],"_",run[runi],"_pop_5km.csv")

    pts = read.csv(file.path(data_dir, loc[loci],paste0("summary_PAs_",loc[loci],"_",run[runi],".csv")))
    # colnames(pts) = c("FID","PUID","Coord_x","Coord_y","Optim","Cost",
    #                   "Disturbance","Species_per_PU","Budget","Remaining_Budget","Species_number_below_target","Proportion_below_target",
    #                   "Region"  )

    #
    # pts = left_join(pts,
    #                 Planning_Units_ZONES[c("PUID","adm0","adm1","adm2","adm3","adm4")],
    #                 by="PUID" )
    #
    # if (strsplit(loc[loci],"_")[[1]][2] == "adm0"){
    #   pts = pts %>%
    #     group_by(adm0) %>%
    #     mutate( min_cost = min(Cost, na.rm=T)) %>%
    #     ungroup
    # }
    # if (strsplit(loc[loci],"_")[[1]][2] == "adm1"){
    #   pts = pts %>%
    #     group_by(adm1) %>%
    #     mutate( min_cost = min(Cost, na.rm=T)) %>%
    #     ungroup
    # }
    # if (strsplit(loc[loci],"_")[[1]][2] == "adm2"){
    #   pts = pts %>%
    #     group_by(adm2) %>%
    #     mutate( min_cost = min(Cost, na.rm=T)) %>%
    #     ungroup
    # }
    # if (strsplit(loc[loci],"_")[[1]][2] == "adm3"){
    #   pts = pts %>%
    #     group_by(adm3) %>%
    #     mutate( min_cost = min(Cost, na.rm=T)) %>%
    #     ungroup
    # }
    # if (strsplit(loc[loci],"_")[[1]][2] == "adm4"){
    #   pts = pts %>%
    #     group_by(adm4) %>%
    #     mutate( min_cost = min(Cost, na.rm=T)) %>%
    #     ungroup
    # }
    # # pts$budget_per_adm = NA
    # # for (B in unique(pts[strsplit(loc[loci],"_")[[1]][2]])[[1]]){
    # #   idxi = which(pts[strsplit(loc[loci],"_")[[1]][2]][[1]] == B)
    # #   pts$budget_per_adm[idxi] = pts$Budget[idxi][1] / length(unique(pts$adm4[idxi]))
    # # }
    # ###############################
    #
    plot(pts$Coord_x,
         pts$Coord_y,
         pch=15, cex=0.3,
         xlab= "Longitude",
         ylab = "Latitude",
         main = "",
         col = ifelse(pts$Unaffordable > 1, "gold2", "darkorchid4"))
    # col =  data$Remaining_Budget)
    text(44,-12, paste0(subplot_label[runi],
                        " ", round(length(which(pts$Unaffordable <= 1))/ length(which(!is.na(pts$Unaffordable))),2)*100,
                        "%"),
         cex=1.5) #add text
    if (run[runi] == "biodiversity_cost"){
      legend("bottomright",
             legend=c("affordable","unaffordable"),
             fill = c( "gold2","darkorchid4"),
      )
    }
    if(runi %in% c(3,4)){
      mtext("Longitude",side = 1, line=4)

    }
    if(runi %in% c(1,3)){
      mtext("Latitude",side = 2, line=4)

    }
  }
  dev.off()
}
