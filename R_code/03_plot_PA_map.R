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

# /48139)*100
budget = 48139
cols =  viridis::viridis(101)#rev()
colorz = rev(RColorBrewer::brewer.pal(11,"Spectral")[1:5])
dispersal_dist = 10#100#300
pop = 40000000000000
loc = c("MDG_adm0","MDG_adm1","MDG_adm2","MDG_adm3","MDG_adm4")
run = c("equal_cost", "pop_cost","area_cost", "biodiversity_cost") #"equal_cost", "biodiversity_cost"
runname = c("equally", "by population","by area", "by biodiversity")
locname = c("National","Province","Region","District","Commune")
# a. Read it into R as a data frame, with columns coord_x, Coord_y, Optim, yield.
filepath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/madagascar/data/captain/population_cost_data/5km"
files = list.files(path = data_dir, pattern = "all_results_MDG_.*\\.csv$", recursive = TRUE)


# grDevices::pdf(file = file.path(filepath, paste0("all_results_plotted_",dispersal_dist,".pdf")),
#                width = 14, height = 20)
# #width = 5, height = 6)

pathi = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/madagascar"

Planning_Units_ZONES = read.csv(file.path(filepath, "Planning_units_zones_pop_5km.csv"))

# function for normalising the data
normalise <- function(x){(x - min(x,na.rm=T))/ ((max(x,na.rm=T) - min(x,na.rm=T)) + 0.01)}

normalize_fixed <- function(x, min_val = 0, max_val = 1000) {
  return ((x - min_val) / (max_val - min_val))
}



#############################################
# 5 panels
#############################################

subplot_label= c("(a)","(b)","(c)","(d)","(e)")
par(mfrow=c(2,2), mar=c(2,1,0.5,0.5), oma=c(5,5,0,0))
# Loop over everything to create staggered histgrams
for(runi in 1:length(run)){

  grDevices::png(file = file.path(pathi, paste0("propotion_portected_",run[runi],".png")),
                 width = 300, height = 100, units='mm', res = 300)
  # par(mfrow=c(1,5))
  par(mfrow=c(1,5), mar=c(5,4,2,0.5),oma=c(0,0,0,0))
  for(loci in 1:length(loc)){
    pts = read.csv(file.path(data_dir, loc[loci],
                             paste0("summary_PAs_",loc[loci],"_",run[runi],".csv")))

    if(loci==1){

      plot(pts$Coord_x,
           pts$Coord_y,
           pch=15, cex=0.3,
           xlab= "Longitude",
           ylab = "Latitude",
           main = locname[loci],#"Protected Area Network",
           col =  viridis::viridis(100)[pts$Optim+1])#scales::alpha(,0.1)
      text(43.5,-12,subplot_label[loci],cex=1.5)
    } else{

      plot(pts$Coord_x,
           pts$Coord_y,
           pch=15, cex=0.3,
           xlab= "Longitude",
           ylab = "",
           main = locname[loci],#"Protected Area Network",
           col =  viridis::viridis(100)[pts$Optim+1])#scales::alpha(,0.1)
      text(43.5,-12,subplot_label[loci],cex=1.5)

    }

  }
  dev.off()
}


############ 3 panels

subplot_label= c("(a)","(b)","(c)","(d)","(e)")
# Loop over everything to create staggered histgrams
for(runi in 1:length(run)){

  grDevices::png(file = file.path(pathi, paste0("propotion_portected_",run[runi],"_3.png")),
                 width = 180, height = 100, units='mm', res = 300)
  # par(mfrow=c(1,5))
  par(mfrow=c(1,3), mar=c(5,4,2,0.5),oma=c(0,0,0,0))
  iter=1
  for(loci in c(1,3,5)){
    pts = read.csv(file.path(data_dir, loc[loci],
                             paste0("summary_PAs_",loc[loci],"_",run[runi],".csv")))

    if(loci==1){

      plot(pts$Coord_x,
           pts$Coord_y,
           pch=15, cex=0.25,
           xlab= "Longitude",
           ylab = "Latitude",
           main = locname[loci],#"Protected Area Network",
           col =  viridis::viridis(100)[pts$Optim+1])#scales::alpha(,0.1)
      text(43.5,-12,subplot_label[iter],cex=1.5)
    } else{

      plot(pts$Coord_x,
           pts$Coord_y,
           pch=15, cex=0.25,
           xlab= "Longitude",
           ylab = "",
           main = locname[loci],#"Protected Area Network",
           col =  viridis::viridis(100)[pts$Optim+1])#scales::alpha(,0.1)
      text(43.5,-12,subplot_label[iter],cex=1.5)

    }
    if(loci == 5){
      legend("bottomright",
             legend=c(0,25,50,75,100),
             cex=0.8,
             fill=viridis::viridis(100)[c(1,25,50,75,100)],
             bg="white",
             title="Selection   \nFrequency",
             # title.adj = 0.5,
             # title.cex=0.9,
             inset=0.02)
    }
    iter = iter + 1
  }

  dev.off()
}








for(runi in 1:length(run)){

  grDevices::png(file = file.path(pathi, paste0("propotion_portected_",run[runi],"_panels.png")),
                 width = 120, height = 260, units='mm', res = 300)
  # par(mfrow=c(1,5))
  par(mfrow=c(3,2), mar=c(4,4,0.4,0.5),oma=c(0,0,0,0))
  for(loci in 1:length(loc)){
    pts = read.csv(file.path(data_dir, loc[loci],
                             paste0("summary_PAs_",loc[loci],"_",run[runi],".csv")))

      plot(pts$Coord_x,
           pts$Coord_y,
           pch=15, cex=0.3,
           xlab= "Longitude",
           ylab = "Latitude",
           # main = locname[loci],#"Protected Area Network",
           col =  viridis::viridis(100)[pts$Optim+1])#scales::alpha(,0.1)
      text(43.8,-12.5,subplot_label[loci],cex=1.5)


  }
  dev.off()
}





grDevices::png(file = file.path(pathi, paste0("propotion_portected_4x4.png")),
               width = 180, height = 270, units='mm', res = 300)
par(mfrow=c(2,2), mar=c(4,4,1,0.5), oma=c(1,1,0,0))
# Loop over everything to create staggered histgrams
for(runi in 1:length(run)){
  loci=5
  pts = read.csv(file.path(data_dir, loc[loci],
                           paste0("summary_PAs_",loc[loci],"_",run[runi],".csv")))


  plot(pts$Coord_x,
       pts$Coord_y,
       pch=15, cex=0.3,
       xlab= "Longitude",
       ylab = "Latitude",
       # main = runname[runi],#"Protected Area Network",
       col =  viridis::viridis(100)[pts$Optim+1])#scales::alpha(,0.1)
  text(43.5,-12,subplot_label[runi],cex=1.5) #add text



}
dev.off()





##############################################################
# 8 panel figure


subplot_label= c("(a)","(b)","(c)","(d)","(e)", "(f)", "(g)", "(h)")
# Loop over everything to create staggered histgrams
for(runi in 1:length(run)){

  grDevices::png(file = file.path(pathi, paste0("propotion_invested_",run[runi],"_8.png")),
                 width = 250, height = 200, units='mm', res = 300)
  # par(mfrow=c(1,5))
  par(mfrow=c(2,4), mar=c(4,4,2,0.5),oma=c(0,0,0,0))
  iter=1

  # colorz <- colorRampPalette(c("grey95","chocolate", "red", "brown", "brown4")) #"orange",
  # colorz <- colorRampPalette(c("grey90","chocolate", "red", "brown", "brown4"))
  colorz <- colorRampPalette(c("grey95","#c1ddb3","#a6cc99","#8cbb80","#71a966", "#4e9345",
                               "#2b7d24", "#1f6b24", "#145927"))
  # colorz <- colorRampPalette(c("gold","chocolate", "red", "brown", "brown4"))
  # colorz <- colorRampPalette(c("grey95","brown1","brown2","brown3", "brown4","black")) #"orange",
  color_pal = colorz(101)
  # colorz <- colorRampPalette(c("chocolate","red", "black")) #"orange",
  # color_pal = c("grey95",colorz(100))

  for(loci in c(1,3,4,5)){
    pts = read.csv(file.path(data_dir, loc[1],
                             paste0("summary_PAs_",loc[1],"_",run[runi],".csv")))
    colnames(pts)[colnames(pts) == "Optim"] = "Optimmm"
    pts_2 = read.csv(file.path(data_dir, loc[loci],
                               paste0("summary_PAs_",loc[loci],"_",run[runi],".csv")))

    pts = left_join(pts,
                    pts_2[c("PUID","Optim")],
                    by="PUID" )

    pts$Optim[which(is.na(pts$Optim))] = 0

    if(loci==1){

      plot(pts$Coord_x,
           pts$Coord_y,
           pch=15, cex=0.25,
           xlab= "",
           ylab = "Latitude",
           main = locname[loci],#"Protected Area Network",
           col =  color_pal[pts$Optim+1])#scales::alpha(,0.1)
      text(43.5,-12,subplot_label[iter],cex=1.5)
    } else{

      plot(pts$Coord_x,
           pts$Coord_y,
           pch=15, cex=0.25,
           xlab= "",
           ylab = "",
           main = locname[loci],#"Protected Area Network",
           col =   color_pal[pts$Optim+1])#scales::alpha(,0.1)
      text(43.5,-12,subplot_label[iter],cex=1.5)

    }
    if(loci == 5){
      legend("bottomright",
             legend=c(0,25,50,75,100),
             cex=0.8,
             fill= color_pal[c(1,25,50,75,100)],
             bg="white",
             title="Selection   \nFrequency",
             # title.adj = 0.5,
             # title.cex=0.9,
             inset=0.02)
    }
    iter = iter + 1

  }
  colorz <- colorRampPalette(c("gold","chocolate", "red", "brown", "brown4"))
  pts_budget = read.csv(file.path(data_dir, loc[5],
                                  paste0("summary_PAs_",loc[5],"_",run[runi],".csv")))
  pts_budget$adm4_budget = pts_budget$Budget
  # cost map
  # iter=1
  for(loci in c(1,3,4,5)){
    pts = read.csv(file.path(data_dir, loc[1],
                             paste0("summary_PAs_",loc[1],"_",run[runi],".csv")))
    # colnames(pts)[colnames(pts) == "Optim"] = "Optimmm"
    pts_2 = read.csv(file.path(data_dir, loc[loci],
                               paste0("summary_PAs_",loc[loci],"_",run[runi],".csv")))

    pts = left_join(pts[c("PUID", "Cost", "Species_per_PU")],
                    pts_2[,!names(pts_2) %in% c("Cost", "Species_per_PU")],
                    by="PUID" )

    # #load the data
    # pts = read.csv(file.path(data_dir, loc[loci],
    #                          paste0("summary_PAs_",loc[loci],"_",run[runi],".csv")))
    pts$Optim[which(is.na(pts$Optim))] = 0


    # attach regional data
    pts = left_join(pts,
                    Planning_Units_ZONES[c("PUID","adm4")],
                    by="PUID" )

    pts = left_join(pts,
                    pts_budget[c("PUID","adm4_budget")],
                    by="PUID" )



    total_cost = sum(pts$Spending,na.rm=T)
    # summarise spending per administrative unit
    pts = pts %>%
      group_by(adm4) %>%
      mutate( adm4_spending = sum(Cost*Optim, na.rm=T)) %>% #total_cost) %>%
      ungroup

    pts = pts %>%
      group_by(adm4) %>%
      mutate( adm4_cost = sum(Cost, na.rm=T)) %>% #total_cost) %>%
      ungroup

    pts = pts %>%
      group_by(adm4) %>%
      mutate( adm4_prop_budget = sum(Cost*Optim, na.rm=T)/adm4_budget) %>% #total_cost) %>%
      ungroup


    pts$to_plot = (pts$adm4_spending/pts$adm4_budget)#/ budget)*100#+0.001#*1433#/sum(pts$Spending,na.rm=T))*1000 #(pts$adm4_spending / pts$Disturbance)
    pts$to_plot[pts$to_plot>100] = 100


    if(loci==1){

      plot(pts$Coord_x,
           pts$Coord_y,
           pch=15, cex=0.25,
           xlab= "Longitude",
           ylab = "Latitude",
           col =ifelse(pts$Cost > pts$Budget, "black",
                       colorz(101)[(round(pts$to_plot))+1]))#scales::alpha(,0.1)
      text(43.5,-12,subplot_label[iter],cex=1.5)
    } else{

      plot(pts$Coord_x,
           pts$Coord_y,
           pch=15, cex=0.25,
           xlab= "Longitude",
           ylab = "",
           # main = locname[loci],#"Protected Area Network",
           col =  ifelse(pts$Cost > pts$Budget, "black",
                         colorz(101)[(round(pts$to_plot))+1]))#scales::alpha(,0.1)
      text(43.5,-12,subplot_label[iter],cex=1.5)

    }
    if(loci == 5){
      legend(x=44, y=-11.8,
             legend=c("0 (unaffordable)","0 (unspent)","25","50","75",">100"),
             cex=0.7,
             fill=c("black",colorz(101)[c(1,25,50,75,100)]),
             bg="white",
             title="% Budget used"
             # title.adj = 0.5,
             # title.cex=0.9,
             # inset=0.05
      )
    }
    iter = iter + 1

  }

  dev.off()
}

###################################################################################################

subplot_label= c("(a)","(b)","(c)","(d)","(e)", "(f)", "(g)", "(h)")
# Loop over everything to create staggered histgrams
for(runi in 1:length(run)){

  grDevices::png(file = file.path(pathi, paste0("propotion_spent_",run[runi],"_8.png")),
                 width = 250, height = 200, units='mm', res = 300)
  # par(mfrow=c(1,5))
  par(mfrow=c(2,4), mar=c(4,4,2,0.5),oma=c(0,0,0,0))
  iter=1

  # colorz <- colorRampPalette(c("grey95","chocolate", "red", "brown", "brown4")) #"orange",
  # colorz <- colorRampPalette(c("grey90","chocolate", "red", "brown", "brown4"))
  colorz <- colorRampPalette(c("grey95","#c1ddb3","#a6cc99","#8cbb80","#71a966", "#4e9345",
                               "#2b7d24", "#1f6b24", "#145927"))
  # colorz <- colorRampPalette(c("gold","chocolate", "red", "brown", "brown4"))
  # colorz <- colorRampPalette(c("grey95","brown1","brown2","brown3", "brown4","black")) #"orange",
  color_pal = colorz(101)
  # colorz <- colorRampPalette(c("chocolate","red", "black")) #"orange",
  # color_pal = c("grey95",colorz(100))

  for(loci in c(1,3,4,5)){
    pts = read.csv(file.path(data_dir, loc[1],
                             paste0("summary_PAs_",loc[1],"_",run[runi],".csv")))
    colnames(pts)[colnames(pts) == "Optim"] = "Optimmm"
    pts_2 = read.csv(file.path(data_dir, loc[loci],
                             paste0("summary_PAs_",loc[loci],"_",run[runi],".csv")))

    pts = left_join(pts,
                    pts_2[c("PUID","Optim")],
                    by="PUID" )

    pts$Optim[which(is.na(pts$Optim))] = 0

    if(loci==1){

      plot(pts$Coord_x,
           pts$Coord_y,
           pch=15, cex=0.25,
           xlab= "",
           ylab = "Latitude",
           main = locname[loci],#"Protected Area Network",
           col =  color_pal[pts$Optim+1])#scales::alpha(,0.1)
      text(43.5,-12,subplot_label[iter],cex=1.5)
    } else{

      plot(pts$Coord_x,
           pts$Coord_y,
           pch=15, cex=0.25,
           xlab= "",
           ylab = "",
           main = locname[loci],#"Protected Area Network",
           col =   color_pal[pts$Optim+1])#scales::alpha(,0.1)
      text(43.5,-12,subplot_label[iter],cex=1.5)

    }
    if(loci == 5){
      legend("bottomright",
             legend=c(0,25,50,75,100),
             cex=0.8,
             fill= color_pal[c(1,25,50,75,100)],
             bg="white",
             title="Selection   \nFrequency",
             # title.adj = 0.5,
             # title.cex=0.9,
             inset=0.02)
    }
    iter = iter + 1

  }
  colorz <- colorRampPalette(c("gold","chocolate", "red", "brown", "brown4"))
  pts_budget = read.csv(file.path(data_dir, loc[5],
                                   paste0("summary_PAs_",loc[5],"_",run[runi],".csv")))
  pts_budget$adm4_budget = pts_budget$Budget
  # cost map
  # iter=1
  for(loci in c(1,3,4,5)){
    pts = read.csv(file.path(data_dir, loc[1],
                             paste0("summary_PAs_",loc[1],"_",run[runi],".csv")))
    # colnames(pts)[colnames(pts) == "Optim"] = "Optimmm"
    pts_2 = read.csv(file.path(data_dir, loc[loci],
                               paste0("summary_PAs_",loc[loci],"_",run[runi],".csv")))

    pts = left_join(pts[c("PUID", "Cost", "Species_per_PU")],
                    pts_2[,!names(pts_2) %in% c("Cost", "Species_per_PU")],
                    by="PUID" )

    # #load the data
    # pts = read.csv(file.path(data_dir, loc[loci],
    #                          paste0("summary_PAs_",loc[loci],"_",run[runi],".csv")))
    pts$Optim[which(is.na(pts$Optim))] = 0


    # attach regional data
    pts = left_join(pts,
                    Planning_Units_ZONES[c("PUID","adm4")],
                    by="PUID" )

    pts = left_join(pts,
                    pts_budget[c("PUID","adm4_budget")],
                    by="PUID" )



    total_cost = sum(pts$Spending,na.rm=T)
    # summarise spending per administrative unit
    pts = pts %>%
      group_by(adm4) %>%
      mutate( adm4_spending = sum(Cost*Optim, na.rm=T)) %>% #total_cost) %>%
      ungroup

    pts = pts %>%
      group_by(adm4) %>%
      mutate( adm4_cost = sum(Cost, na.rm=T)) %>% #total_cost) %>%
      ungroup

    # pts = pts %>%
    #   group_by(adm4) %>%
    #   mutate( adm4_prop_budget = sum(Cost*Optim, na.rm=T)/adm4_budget*100) %>% #total_cost) %>%
    #   ungroup

    pts = pts %>%
      group_by(adm4) %>%
      mutate(adm4_area = length(Cost)) %>% #total_cost) %>%
      ungroup

    pts = pts %>%
      group_by(adm4) %>%
      mutate(adm4_biodiv = sum(Species_per_PU, na.rm=T)) %>% #total_cost) %>%
      ungroup


    pts$to_plot = pts$adm4_spending / pts$adm4_cost#normalize_fixed(pts$adm4_spending, 0, 18451528)#/(pts$adm4_budget*100) #normalise(pts$adm4_spending) / normalise(pts$adm4_budget)#normalise(pts$adm4_spending) * 3/ normalise(pts$adm4_area) * normalise(pts$adm4_cost) * normalise(pts$adm4_biodiv)#pts$adm4_spending / (pts$adm4_spending/pts$adm4_budget)#/ budget)*100#+0.001#*1433#/sum(pts$Spending,na.rm=T))*1000 #(pts$adm4_spending / pts$Disturbance)
    # pts$to_plot = pts$to_plot * 100
    # pts$to_plot[is.na(pts$to_plot)] = 0
    # pts$to_plot = log(pts$to_plot+0.001)
    # pts$to_plot = normalize_fixed( pts$to_plot, min_val = min( pts$to_plot), max_val = max( pts$to_plot)) *100
    # pts$to_plot[pts$to_plot>100] = 100

    # plot(pts$Coord_x, pts$Coord_y,
    #      col = colorz(101)[round(normalise(pts$to_plot)*100) +1],
    #      pch=15, cex=0.25)


    if(loci==1){

      plot(pts$Coord_x,
           pts$Coord_y,
           pch=15, cex=0.25,
           xlab= "Longitude",
           ylab = "Latitude",
           col =ifelse(pts$Cost > pts$Budget, "black",colorz(101)[round((pts$to_plot)) +1]))
                          # colorz(101)[(round(pts$to_plot))+1])
           #scales::alpha(,0.1)
      text(43.5,-12,subplot_label[iter],cex=1.5)
    } else{

      plot(pts$Coord_x,
           pts$Coord_y,
           pch=15, cex=0.25,
           xlab= "Longitude",
           ylab = "",
           # main = locname[loci],#"Protected Area Network",
           col =  ifelse(pts$Cost > pts$Budget, "black",colorz(101)[round((pts$to_plot)) +1]))
             # colorz(101)[(round(pts$to_plot))+1]))#scales::alpha(,0.1)
      text(43.5,-12,subplot_label[iter],cex=1.5)

    }
    if(loci == 5){
      legend(x=44, y=-11.8,
             legend=c("0 (unaffordable)","0 (unspent)","25","50","75",">100"),
             cex=0.7,
             fill=c("black",colorz(101)[c(1,25,50,75,100)]),
             bg="white",
             title="% Budget used"
             # title.adj = 0.5,
             # title.cex=0.9,
             # inset=0.05
             )
    }
    iter = iter + 1
  }
  dev.off()
}



####################################################################
# 6 panels


###################################################################################################

subplot_label= c("(a)","(b)","(c)","(d)","(e)", "(f)", "(g)", "(h)")
# Loop over everything to create staggered histgrams
for(runi in 1:length(run)){

  grDevices::png(file = file.path(pathi, paste0("propotion_spent_",run[runi],"_6.png")),
                 width = 187, height = 200, units='mm', res = 300)
  # par(mfrow=c(1,5))
  par(mfrow=c(2,3), mar=c(4,4,2,0.5),oma=c(0,0,0,0))
  iter=1

  # colorz <- colorRampPalette(c("grey95","chocolate", "red", "brown", "brown4")) #"orange",
  # colorz <- colorRampPalette(c("grey90","chocolate", "red", "brown", "brown4"))
  colorz <- colorRampPalette(c("grey95","#c1ddb3","#a6cc99","#8cbb80","#71a966", "#4e9345",
                               "#2b7d24", "#1f6b24", "#145927"))
  # colorz <- colorRampPalette(c("gold","chocolate", "red", "brown", "brown4"))
  # colorz <- colorRampPalette(c("grey95","brown1","brown2","brown3", "brown4","black")) #"orange",
  color_pal = colorz(101)
  # colorz <- colorRampPalette(c("chocolate","red", "black")) #"orange",
  # color_pal = c("grey95",colorz(100))

  for(loci in c(1,3,5)){
    pts = read.csv(file.path(data_dir, loc[1],
                             paste0("summary_PAs_",loc[1],"_",run[runi],".csv")))
    colnames(pts)[colnames(pts) == "Optim"] = "Optimmm"
    pts_2 = read.csv(file.path(data_dir, loc[loci],
                               paste0("summary_PAs_",loc[loci],"_",run[runi],".csv")))

    pts = left_join(pts,
                    pts_2[c("PUID","Optim")],
                    by="PUID" )

    pts$Optim[which(is.na(pts$Optim))] = 0

    if(loci==1){

      plot(pts$Coord_x,
           pts$Coord_y,
           pch=15, cex=0.25,
           xlab= "",
           ylab = "Latitude",
           main = locname[loci],#"Protected Area Network",
           col =  color_pal[pts$Optim+1])#scales::alpha(,0.1)
      text(43.5,-12,subplot_label[iter],cex=1.5)
    } else{

      plot(pts$Coord_x,
           pts$Coord_y,
           pch=15, cex=0.25,
           xlab= "",
           ylab = "",
           main = locname[loci],#"Protected Area Network",
           col =   color_pal[pts$Optim+1])#scales::alpha(,0.1)
      text(43.5,-12,subplot_label[iter],cex=1.5)

    }
    if(loci == 5){
      legend("bottomright",
             legend=c(0,25,50,75,100),
             cex=0.8,
             fill= color_pal[c(1,25,50,75,100)],
             bg="white",
             title="Selection   \nFrequency",
             # title.adj = 0.5,
             # title.cex=0.9,
             inset=0.02)
    }
    iter = iter + 1

  }
  colorz <- colorRampPalette(c("gold","chocolate", "red", "brown", "brown4"))
  pts_budget = read.csv(file.path(data_dir, loc[5],
                                  paste0("summary_PAs_",loc[5],"_",run[runi],".csv")))
  pts_budget$adm4_budget = pts_budget$Budget *100
  # cost map
  # iter=1
  for(loci in c(1,3,5)){

    # base = Planning_Units_ZONES[c("PUID","adm4")]
    pts = read.csv(file.path(data_dir, loc[1],
                             paste0("summary_PAs_",loc[1],"_",run[runi],".csv")))
    # colnames(pts)[colnames(pts) == "Optim"] = "Optimmm"
    pts_2 = read.csv(file.path(data_dir, loc[loci],
                               paste0("summary_PAs_",loc[loci],"_",run[runi],".csv")))

    pts = left_join(pts[c("PUID", "Cost", "Species_per_PU")],
                    pts_2[,!names(pts_2) %in% c("Cost", "Species_per_PU")],
                    by="PUID" )

    # #load the data
    # pts = read.csv(file.path(data_dir, loc[loci],
    #                          paste0("summary_PAs_",loc[loci],"_",run[runi],".csv")))
    pts$Optim[which(is.na(pts$Optim))] = 0


    # attach regional data
    pts = left_join(pts,
                    Planning_Units_ZONES[c("PUID","adm4")],
                    by="PUID" )

    pts = left_join(pts,
                    pts_budget[c("PUID","adm4_budget")],
                    by="PUID" )



    total_cost = sum(pts$Spending,na.rm=T)
    # summarise spending per administrative unit
    pts = pts %>%
      group_by(adm4) %>%
      mutate( adm4_spending = sum(Cost*Optim, na.rm=T)) %>% #total_cost) %>%
      ungroup

    pts = pts %>%
      group_by(adm4) %>%
      mutate( adm4_cost = sum(Cost, na.rm=T)) %>% #total_cost) %>%
      ungroup

    # pts = pts %>%
    #   group_by(adm4) %>%
    #   mutate( adm4_prop_budget = sum(Cost*Optim, na.rm=T)/adm4_budget*100) %>% #total_cost) %>%
    #   ungroup

    pts = pts %>%
      group_by(adm4) %>%
      mutate(adm4_area = length(Cost)) %>% #total_cost) %>%
      ungroup

    pts = pts %>%
      group_by(adm4) %>%
      mutate(adm4_biodiv = sum(Species_per_PU, na.rm=T)) %>% #total_cost) %>%
      ungroup


    pts$to_plot = (pts$adm4_spending / pts$adm4_budget)*100#/ pts$adm4_cost#normalize_fixed(pts$adm4_spending, 0, 18451528)#/(pts$adm4_budget*100) #normalise(pts$adm4_spending) / normalise(pts$adm4_budget)#normalise(pts$adm4_spending) * 3/ normalise(pts$adm4_area) * normalise(pts$adm4_cost) * normalise(pts$adm4_biodiv)#pts$adm4_spending / (pts$adm4_spending/pts$adm4_budget)#/ budget)*100#+0.001#*1433#/sum(pts$Spending,na.rm=T))*1000 #(pts$adm4_spending / pts$Disturbance)
    pts$to_plot[pts$to_plot>100] = 200
    # pts$to_plot = pts$to_plot * 100
    pts$to_plot[which(is.na(pts$to_plot))] = 0
    pts$Cost[which(is.na(pts$Cost))] = 0#max(pts$Cost,na.rm=T)
    pts$Budget[which(is.na(pts$Budget))] =0

    pts$colors = ifelse(pts$Cost > pts$Budget, "grey",colorz(101)[round((pts$to_plot)) +1])
    pts$colors = ifelse(pts$to_plot == 200, "black", pts$colors)
    # pts$to_plot = log(pts$to_plot+0.001)
    # pts$to_plot = normalize_fixed( pts$to_plot, min_val = min( pts$to_plot), max_val = max( pts$to_plot)) *100
    # pts$to_plot[pts$to_plot>100] = 100

    # plot(pts$Coord_x, pts$Coord_y,
    #      col = colorz(101)[round(normalise(pts$to_plot)*100) +1],
    #      pch=15, cex=0.25)


    if(loci==1){

      plot(pts$Coord_x,
           pts$Coord_y,
           pch=15, cex=0.25,
           xlab= "Longitude",
           ylab = "Latitude",
           col = pts$colors)#ifelse(pts$Cost > pts$Budget, "grey",colorz(101)[round((pts$to_plot)) +1]))
      # colorz(101)[(round(pts$to_plot))+1])
      #scales::alpha(,0.1)
      text(43.5,-12,subplot_label[iter],cex=1.5)
    } else{

      plot(pts$Coord_x,
           pts$Coord_y,
           pch=15, cex=0.25,
           xlab= "Longitude",
           ylab = "",
           # main = locname[loci],#"Protected Area Network",
           col =  pts$colors)#ifelse(pts$Cost > pts$Budget, "grey",colorz(101)[round((pts$to_plot)) +1]))
      # colorz(101)[(round(pts$to_plot))+1]))#scales::alpha(,0.1)
      text(43.5,-12,subplot_label[iter],cex=1.5)

    }
    if(loci == 5){
      legend(x=44, y=-11.8,
             legend=c("0 (unaffordable)","0 (unspent)","25","50","75","100",">100"),
             cex=0.7,
             fill=c("grey",colorz(101)[c(1,25,50,75,100)],"black"),
             bg="white",
             title="% Budget used"
             # title.adj = 0.5,
             # title.cex=0.9,
             # inset=0.05
      )
    }
    iter = iter + 1
  }
  dev.off()
}




####################################################################

# PER CAPITA

###################################################################################################

subplot_label= c("(a)","(b)","(c)",
                 "(d)","(e)", "(f)",
                 "(g)", "(h)", "(i)",
                 "(j)", "(k)", "(l)")
# Loop over everything to create staggered histgrams
for(runi in 1:length(run)){

  grDevices::png(file = file.path(pathi, paste0("equity_comparison_",run[runi],"_12.png")),
                 width = 187, height = 400, units='mm', res = 300)
  # par(mfrow=c(1,5))
  par(mfrow=c(4,3), mar=c(2,2,0.5,0.5), oma=c(5,5,0,0))#mar=c(4,4,2,0.5),oma=c(0,0,0,0))
  iter=1

  # colorz <- colorRampPalette(c("grey95","chocolate", "red", "brown", "brown4")) #"orange",
  # colorz <- colorRampPalette(c("grey90","chocolate", "red", "brown", "brown4"))
  colorz <- colorRampPalette(c("grey95","#c1ddb3","#a6cc99","#8cbb80","#71a966", "#4e9345",
                               "#2b7d24", "#1f6b24", "#145927"))
  # colorz <- colorRampPalette(c("gold","chocolate", "red", "brown", "brown4"))
  # colorz <- colorRampPalette(c("grey95","brown1","brown2","brown3", "brown4","black")) #"orange",
  color_pal = colorz(101)
  # colorz <- colorRampPalette(c("chocolate","red", "black")) #"orange",
  # color_pal = c("grey95",colorz(100))


  for(loci in c(1,3,5)){

    pts = read.csv(file.path(data_dir, loc[1],
                             paste0("summary_PAs_",loc[1],"_",run[runi],".csv")))
    colnames(pts)[colnames(pts) == "Optim"] = "Optimmm"
    pts_2 = read.csv(file.path(data_dir, loc[loci],
                               paste0("summary_PAs_",loc[loci],"_",run[runi],".csv")))

    pts = left_join(pts,
                    pts_2[c("PUID","Optim")],
                    by="PUID" )

    pts$Optim[which(is.na(pts$Optim))] = 0

    if(loci==1){

      plot(pts$Coord_x,
           pts$Coord_y,
           pch=15, cex=0.25,
           xlab= "",
           ylab = "Latitude",
           main = locname[loci],#"Protected Area Network",
           col =  color_pal[pts$Optim+1])#scales::alpha(,0.1)
      text(43.5,-12,subplot_label[iter],cex=1.5)
    } else{

      plot(pts$Coord_x,
           pts$Coord_y,
           pch=15, cex=0.25,
           xlab= "",
           ylab = "",
           main = locname[loci],#"Protected Area Network",
           col =   color_pal[pts$Optim+1])#scales::alpha(,0.1)
      text(43.5,-12,subplot_label[iter],cex=1.5)

    }
    if(loci == 5){
      legend("bottomright",
             legend=c(0,25,50,75,100),
             cex=0.8,
             fill= color_pal[c(1,25,50,75,100)],
             bg="white",
             title="Selection   \nFrequency",
             # title.adj = 0.5,
             # title.cex=0.9,
             inset=0.02)
    }
    if(loci %in% c(1)){
      mtext("Latitude",side = 2, line=4)
    }
    iter = iter + 1

  }
  colorz <- colorRampPalette(c("gold","chocolate", "red", "brown", "brown4"))
  pts_budget = read.csv(file.path(data_dir, loc[5],
                                  paste0("summary_PAs_",loc[5],"_",run[runi],".csv")))
  pts_budget$adm4_budget = pts_budget$Budget *100
  # cost map
  # iter=1
  for(loci in c(1,3,5)){


    # base = Planning_Units_ZONES[c("PUID","adm4")]
    pts = read.csv(file.path(data_dir, loc[1],
                             paste0("summary_PAs_",loc[1],"_",run[runi],".csv")))
    # colnames(pts)[colnames(pts) == "Optim"] = "Optimmm"
    pts_2 = read.csv(file.path(data_dir, loc[loci],
                               paste0("summary_PAs_",loc[loci],"_",run[runi],".csv")))

    pts = left_join(pts[c("PUID", "Cost", "Species_per_PU")],
                    pts_2[,!names(pts_2) %in% c("Cost", "Species_per_PU")],
                    by="PUID" )

    # #load the data
    # pts = read.csv(file.path(data_dir, loc[loci],
    #                          paste0("summary_PAs_",loc[loci],"_",run[runi],".csv")))
    pts$Optim[which(is.na(pts$Optim))] = 0


    # attach regional data
    pts = left_join(pts,
                    Planning_Units_ZONES[c("PUID","adm4")],
                    by="PUID" )

    pts = left_join(pts,
                    pts_budget[c("PUID","adm4_budget")],
                    by="PUID" )



    total_cost = sum(pts$Spending,na.rm=T)
    # summarise spending per administrative unit
    pts = pts %>%
      group_by(adm4) %>%
      mutate( adm4_spending = sum(Cost*Optim, na.rm=T)) %>% #total_cost) %>%
      ungroup

    pts = pts %>%
      group_by(adm4) %>%
      mutate( adm4_cost = sum(Cost, na.rm=T)) %>% #total_cost) %>%
      ungroup

    # pts = pts %>%
    #   group_by(adm4) %>%
    #   mutate( adm4_prop_budget = sum(Cost*Optim, na.rm=T)/adm4_budget*100) %>% #total_cost) %>%
    #   ungroup

    pts = pts %>%
      group_by(adm4) %>%
      mutate(adm4_area = length(Cost)) %>% #total_cost) %>%
      ungroup

    pts = pts %>%
      group_by(adm4) %>%
      mutate(adm4_biodiv = sum(Species_per_PU, na.rm=T)) %>% #total_cost) %>%
      ungroup


    pts$to_plot = normalise(log(pts$adm4_spending / pts$adm4_cost+0.001))*100#/ pts$adm4_cost#normalize_fixed(pts$adm4_spending, 0, 18451528)#/(pts$adm4_budget*100) #normalise(pts$adm4_spending) / normalise(pts$adm4_budget)#normalise(pts$adm4_spending) * 3/ normalise(pts$adm4_area) * normalise(pts$adm4_cost) * normalise(pts$adm4_biodiv)#pts$adm4_spending / (pts$adm4_spending/pts$adm4_budget)#/ budget)*100#+0.001#*1433#/sum(pts$Spending,na.rm=T))*1000 #(pts$adm4_spending / pts$Disturbance)
    pts$to_plot[pts$to_plot>100] = 200
    # pts$to_plot = pts$to_plot * 100
    pts$to_plot[which(is.na(pts$to_plot))] = 0
    pts$Cost[which(is.na(pts$Cost))] = 0#max(pts$Cost,na.rm=T)
    pts$Budget[which(is.na(pts$Budget))] =0

    pts$colors = ifelse(pts$Cost > pts$Budget, "grey",colorz(101)[round((pts$to_plot)) +1])
    pts$colors = ifelse(pts$to_plot == 200, "black", pts$colors)
    # pts$to_plot = log(pts$to_plot+0.001)
    # pts$to_plot = normalize_fixed( pts$to_plot, min_val = min( pts$to_plot), max_val = max( pts$to_plot)) *100
    # pts$to_plot[pts$to_plot>100] = 100

    # plot(pts$Coord_x, pts$Coord_y,
    #      col = colorz(101)[round(normalise(pts$to_plot)*100) +1],
    #      pch=15, cex=0.25)


    if(loci==1){

      plot(pts$Coord_x,
           pts$Coord_y,
           pch=15, cex=0.25,
           xlab= "",
           ylab = "Latitude",
           col = pts$colors)#ifelse(pts$Cost > pts$Budget, "grey",colorz(101)[round((pts$to_plot)) +1]))
      # colorz(101)[(round(pts$to_plot))+1])
      #scales::alpha(,0.1)
      text(43.5,-12,subplot_label[iter],cex=1.5)
    } else{

      plot(pts$Coord_x,
           pts$Coord_y,
           pch=15, cex=0.25,
           xlab= "",
           ylab = "",
           # main = locname[loci],#"Protected Area Network",
           col =  pts$colors)#ifelse(pts$Cost > pts$Budget, "grey",colorz(101)[round((pts$to_plot)) +1]))
      # colorz(101)[(round(pts$to_plot))+1]))#scales::alpha(,0.1)
      text(43.5,-12,subplot_label[iter],cex=1.5)

    }
    if(loci == 5){
      legend(x=44, y=-11.8,
             legend=c("0 (unaffordable)","0 (unspent)","25","50","75","100",">100"),
             cex=0.7,
             fill=c("grey",colorz(101)[c(1,25,50,75,100)],"black"),
             bg="white",
             title="Budget per capita (log)"
             # title.adj = 0.5,
             # title.cex=0.9,
             # inset=0.05
      )
    }
    if(loci %in% c(1)){
      mtext("Latitude",side = 2, line=4)
    }
    iter = iter + 1
  }
  colorz <- colorRampPalette(c("gold","chocolate", "red", "brown", "brown4"))
  pts_budget = read.csv(file.path(data_dir, loc[5],
                                  paste0("summary_PAs_",loc[5],"_",run[runi],".csv")))
  pts_budget$adm4_budget = pts_budget$Budget *100
  # cost map
  # iter=1
  for(loci in c(1,3,5)){


    # base = Planning_Units_ZONES[c("PUID","adm4")]
    pts = read.csv(file.path(data_dir, loc[1],
                             paste0("summary_PAs_",loc[1],"_",run[runi],".csv")))
    # colnames(pts)[colnames(pts) == "Optim"] = "Optimmm"
    pts_2 = read.csv(file.path(data_dir, loc[loci],
                               paste0("summary_PAs_",loc[loci],"_",run[runi],".csv")))

    pts = left_join(pts[c("PUID", "Cost", "Species_per_PU")],
                    pts_2[,!names(pts_2) %in% c("Cost", "Species_per_PU")],
                    by="PUID" )

    # #load the data
    # pts = read.csv(file.path(data_dir, loc[loci],
    #                          paste0("summary_PAs_",loc[loci],"_",run[runi],".csv")))
    pts$Optim[which(is.na(pts$Optim))] = 0


    # attach regional data
    pts = left_join(pts,
                    Planning_Units_ZONES[c("PUID","adm4")],
                    by="PUID" )

    pts = left_join(pts,
                    pts_budget[c("PUID","adm4_budget")],
                    by="PUID" )



    total_cost = sum(pts$Spending,na.rm=T)
    # summarise spending per administrative unit
    pts = pts %>%
      group_by(adm4) %>%
      mutate( adm4_spending = sum(Cost*Optim, na.rm=T)) %>% #total_cost) %>%
      ungroup

    pts = pts %>%
      group_by(adm4) %>%
      mutate( adm4_cost = sum(Cost, na.rm=T)) %>% #total_cost) %>%
      ungroup

    # pts = pts %>%
    #   group_by(adm4) %>%
    #   mutate( adm4_prop_budget = sum(Cost*Optim, na.rm=T)/adm4_budget*100) %>% #total_cost) %>%
    #   ungroup

    pts = pts %>%
      group_by(adm4) %>%
      mutate(adm4_area = length(Cost)) %>% #total_cost) %>%
      ungroup

    pts = pts %>%
      group_by(adm4) %>%
      mutate(adm4_biodiv = sum(Species_per_PU, na.rm=T)) %>% #total_cost) %>%
      ungroup


    pts$to_plot = normalise((pts$adm4_spending / pts$adm4_area+0.001))*100#/ pts$adm4_cost#normalize_fixed(pts$adm4_spending, 0, 18451528)#/(pts$adm4_budget*100) #normalise(pts$adm4_spending) / normalise(pts$adm4_budget)#normalise(pts$adm4_spending) * 3/ normalise(pts$adm4_area) * normalise(pts$adm4_cost) * normalise(pts$adm4_biodiv)#pts$adm4_spending / (pts$adm4_spending/pts$adm4_budget)#/ budget)*100#+0.001#*1433#/sum(pts$Spending,na.rm=T))*1000 #(pts$adm4_spending / pts$Disturbance)
    pts$to_plot[pts$to_plot>100] = 200
    # pts$to_plot = pts$to_plot * 100
    pts$to_plot[which(is.na(pts$to_plot))] = 0
    pts$Cost[which(is.na(pts$Cost))] = 0#max(pts$Cost,na.rm=T)
    pts$Budget[which(is.na(pts$Budget))] =0

    pts$colors = ifelse(pts$Cost > pts$Budget, "grey",colorz(101)[round((pts$to_plot)) +1])
    pts$colors = ifelse(pts$to_plot == 200, "black", pts$colors)
    # pts$to_plot = log(pts$to_plot+0.001)
    # pts$to_plot = normalize_fixed( pts$to_plot, min_val = min( pts$to_plot), max_val = max( pts$to_plot)) *100
    # pts$to_plot[pts$to_plot>100] = 100

    # plot(pts$Coord_x, pts$Coord_y,
    #      col = colorz(101)[round(normalise(pts$to_plot)*100) +1],
    #      pch=15, cex=0.25)


    if(loci==1){

      plot(pts$Coord_x,
           pts$Coord_y,
           pch=15, cex=0.25,
           xlab= "",
           ylab = "Latitude",
           col = pts$colors)#ifelse(pts$Cost > pts$Budget, "grey",colorz(101)[round((pts$to_plot)) +1]))
      # colorz(101)[(round(pts$to_plot))+1])
      #scales::alpha(,0.1)
      text(43.5,-12,subplot_label[iter],cex=1.5)
    } else{

      plot(pts$Coord_x,
           pts$Coord_y,
           pch=15, cex=0.25,
           xlab= "",
           ylab = "",
           # main = locname[loci],#"Protected Area Network",
           col =  pts$colors)#ifelse(pts$Cost > pts$Budget, "grey",colorz(101)[round((pts$to_plot)) +1]))
      # colorz(101)[(round(pts$to_plot))+1]))#scales::alpha(,0.1)
      text(43.5,-12,subplot_label[iter],cex=1.5)

    }
    if(loci == 5){
      legend(x=44, y=-11.8,
             legend=c("0 (unaffordable)","0 (unspent)","25","50","75","100",">100"),
             cex=0.7,
             fill=c("grey",colorz(101)[c(1,25,50,75,100)],"black"),
             bg="white",
             title="Budget per area"
             # title.adj = 0.5,
             # title.cex=0.9,
             # inset=0.05
      )
    }
    if(loci %in% c(1)){
      mtext("Latitude",side = 2, line=4)
    }
    iter = iter + 1
  }
  colorz <- colorRampPalette(c("gold","chocolate", "red", "brown", "brown4"))
  pts_budget = read.csv(file.path(data_dir, loc[5],
                                  paste0("summary_PAs_",loc[5],"_",run[runi],".csv")))
  pts_budget$adm4_budget = pts_budget$Budget *100
  # cost map
  # iter=1
  for(loci in c(1,3,5)){

    # base = Planning_Units_ZONES[c("PUID","adm4")]
    pts = read.csv(file.path(data_dir, loc[1],
                             paste0("summary_PAs_",loc[1],"_",run[runi],".csv")))
    # colnames(pts)[colnames(pts) == "Optim"] = "Optimmm"
    pts_2 = read.csv(file.path(data_dir, loc[loci],
                               paste0("summary_PAs_",loc[loci],"_",run[runi],".csv")))

    pts = left_join(pts[c("PUID", "Cost", "Species_per_PU")],
                    pts_2[,!names(pts_2) %in% c("Cost", "Species_per_PU")],
                    by="PUID" )

    # #load the data
    # pts = read.csv(file.path(data_dir, loc[loci],
    #                          paste0("summary_PAs_",loc[loci],"_",run[runi],".csv")))
    pts$Optim[which(is.na(pts$Optim))] = 0


    # attach regional data
    pts = left_join(pts,
                    Planning_Units_ZONES[c("PUID","adm4")],
                    by="PUID" )

    pts = left_join(pts,
                    pts_budget[c("PUID","adm4_budget")],
                    by="PUID" )



    total_cost = sum(pts$Spending,na.rm=T)
    # summarise spending per administrative unit
    pts = pts %>%
      group_by(adm4) %>%
      mutate( adm4_spending = sum(Cost*Optim, na.rm=T)) %>% #total_cost) %>%
      ungroup

    pts = pts %>%
      group_by(adm4) %>%
      mutate( adm4_cost = sum(Cost, na.rm=T)) %>% #total_cost) %>%
      ungroup

    # pts = pts %>%
    #   group_by(adm4) %>%
    #   mutate( adm4_prop_budget = sum(Cost*Optim, na.rm=T)/adm4_budget*100) %>% #total_cost) %>%
    #   ungroup

    pts = pts %>%
      group_by(adm4) %>%
      mutate(adm4_area = length(Cost)) %>% #total_cost) %>%
      ungroup

    pts = pts %>%
      group_by(adm4) %>%
      mutate(adm4_biodiv = sum(Species_per_PU, na.rm=T)) %>% #total_cost) %>%
      ungroup


    pts$to_plot = normalise((pts$adm4_spending / pts$adm4_biodiv+0.001))*100#/ pts$adm4_cost#normalize_fixed(pts$adm4_spending, 0, 18451528)#/(pts$adm4_budget*100) #normalise(pts$adm4_spending) / normalise(pts$adm4_budget)#normalise(pts$adm4_spending) * 3/ normalise(pts$adm4_area) * normalise(pts$adm4_cost) * normalise(pts$adm4_biodiv)#pts$adm4_spending / (pts$adm4_spending/pts$adm4_budget)#/ budget)*100#+0.001#*1433#/sum(pts$Spending,na.rm=T))*1000 #(pts$adm4_spending / pts$Disturbance)
    pts$to_plot[pts$to_plot>100] = 200
    # pts$to_plot = pts$to_plot * 100
    pts$to_plot[which(is.na(pts$to_plot))] = 0
    pts$Cost[which(is.na(pts$Cost))] = 0#max(pts$Cost,na.rm=T)
    pts$Budget[which(is.na(pts$Budget))] =0

    pts$colors = ifelse(pts$Cost > pts$Budget, "grey",colorz(101)[round((pts$to_plot)) +1])
    pts$colors = ifelse(pts$to_plot == 200, "black", pts$colors)
    # pts$to_plot = log(pts$to_plot+0.001)
    # pts$to_plot = normalize_fixed( pts$to_plot, min_val = min( pts$to_plot), max_val = max( pts$to_plot)) *100
    # pts$to_plot[pts$to_plot>100] = 100

    # plot(pts$Coord_x, pts$Coord_y,
    #      col = colorz(101)[round(normalise(pts$to_plot)*100) +1],
    #      pch=15, cex=0.25)


    if(loci==1){

      plot(pts$Coord_x,
           pts$Coord_y,
           pch=15, cex=0.25,
           xlab= "Longitude",
           ylab = "Latitude",
           col = pts$colors)#ifelse(pts$Cost > pts$Budget, "grey",colorz(101)[round((pts$to_plot)) +1]))
      # colorz(101)[(round(pts$to_plot))+1])
      #scales::alpha(,0.1)
      text(43.5,-12,subplot_label[iter],cex=1.5)
    } else{

      plot(pts$Coord_x,
           pts$Coord_y,
           pch=15, cex=0.25,
           xlab= "Longitude",
           ylab = "",
           # main = locname[loci],#"Protected Area Network",
           col =  pts$colors)#ifelse(pts$Cost > pts$Budget, "grey",colorz(101)[round((pts$to_plot)) +1]))
      # colorz(101)[(round(pts$to_plot))+1]))#scales::alpha(,0.1)
      text(43.5,-12,subplot_label[iter],cex=1.5)

    }
    if(loci == 5){
      legend(x=44, y=-11.8,
             legend=c("0 (unaffordable)","0 (unspent)","25","50","75","100",">100"),
             cex=0.7,
             fill=c("grey",colorz(101)[c(1,25,50,75,100)],"black"),
             bg="white",
             title="Budget per species"
             # title.adj = 0.5,
             # title.cex=0.9,
             # inset=0.05
      )
    }
    mtext("Longitude",side = 1, line=4)
    if(loci %in% c(1)){
      mtext("Latitude",side = 2, line=4)
    }
    iter = iter + 1
  }

  dev.off()
}



