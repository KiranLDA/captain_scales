# load libraries
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


#############################################
# connectivity metrics
#############################################

PC = data.frame(matrix(NA, nrow= 20, ncol = 6))
ECA = data.frame(matrix(NA, nrow= 20, ncol = 6))
IIC = data.frame(matrix(NA, nrow= 20, ncol = 6))
AUC = data.frame(matrix(NA, nrow= 20, ncol = 6))
total_area = data.frame(matrix(NA, nrow= 20, ncol = 6))
mean_area = data.frame(matrix(NA, nrow= 20, ncol = 6))
fragments = data.frame(matrix(NA, nrow= 20, ncol = 6))
buffer = data.frame(matrix(NA, nrow= 20, ncol = 6))
conservation_spending = data.frame(matrix(NA, nrow= 20, ncol = 6))
adm_area = data.frame(matrix(NA, nrow= 20, ncol = 6))
IIC_adj = data.frame(matrix(NA, nrow= 20, ncol = 6))

colnames(PC) = c("adm","run","LL","mean","median","UL")
colnames(ECA) = c("adm","run","LL","mean","median","UL")
colnames(IIC) = c("adm","run","LL","mean","median","UL")
colnames(AUC) = c("adm","run","LL","mean","median","UL")
colnames(total_area) = c("adm","run","LL","mean","median","UL")
colnames(mean_area) = c("adm","run","LL","mean","median","UL")
colnames(fragments) = c("adm","run","LL","mean","median","UL")
colnames(buffer) = c("adm","run","LL","mean","median","UL")
colnames(conservation_spending) = c("adm","run","LL","mean","median","UL")
colnames(adm_area) = c("adm","run","LL","mean","median","UL")
colnames(IIC_adj) = c("adm","run","LL","mean","median","UL")

rowi=1
for(runi in 1:length(run)){
  for (loci in 1:length(loc)){
    conn = read.csv(file.path(data_dir, loc[loci],
                                    paste0("summary_connectivity_",loc[loci],"_",run[runi],".csv")))

    PC[rowi,] = c(loc[loci],
                  run[runi],
                  summary(conn$PC)[2],
                  summary(conn$PC)[4],
                  summary(conn$PC)[3],
                  summary(conn$PC)[5])

    ECA[rowi,] = c(loc[loci],
                   run[runi],
                   summary(conn$ECA)[2],
                   summary(conn$ECA)[4],
                   summary(conn$ECA)[3],
                   summary(conn$ECA)[5])

    IIC[rowi,] = c(loc[loci],
                   run[runi],
                   summary(conn$IIC)[2],
                   summary(conn$IIC)[4],
                   summary(conn$IIC)[3],
                   summary(conn$IIC)[5])

    AUC[rowi,] = c(loc[loci],
                   run[runi],
                   summary(conn$AUC)[2],
                   summary(conn$AUC)[4],
                   summary(conn$AUC)[3],
                   summary(conn$AUC)[5])

    total_area[rowi,] = c(loc[loci],
                          run[runi],
                          summary(conn$total_area)[2]*1000000 / 581540,
                          summary(conn$total_area)[4]*1000000 / 581540,
                          summary(conn$total_area)[3]*1000000 / 581540,
                          summary(conn$total_area)[5]*1000000 / 581540)

    mean_area[rowi,] = c(loc[loci],
                        run[runi],
                        summary(conn$mean_area)[2]*1000000,
                        summary(conn$mean_area)[4]*1000000,
                        summary(conn$mean_area)[3]*1000000,
                        summary(conn$mean_area)[5]*1000000)

    fragments[rowi,] = c(loc[loci],
                         run[runi],
                         summary(conn$fragments)[2],
                         summary(conn$fragments)[4],
                         summary(conn$fragments)[3],
                         summary(conn$fragments)[5])

    buffer[rowi,] = c(loc[loci],
                         run[runi],
                         summary(conn$buffer)[2]*1000000,
                         summary(conn$buffer)[4]*1000000,
                         summary(conn$buffer)[3]*1000000,
                         summary(conn$buffer)[5]*1000000)

    conservation_spending[rowi,] = c(loc[loci],
                         run[runi],
                         summary(conn$conservation_spending)[2],
                         summary(conn$conservation_spending)[4],
                         summary(conn$conservation_spending)[3],
                         summary(conn$conservation_spending)[5])

    adm_area[rowi,] = c(loc[loci],
                        run[runi],
                        summary(conn$adm_area)[2],
                        summary(conn$adm_area)[4],
                        summary(conn$adm_area)[3],
                        summary(conn$adm_area)[5])

    IIC_adj[rowi,] = c(loc[loci],
                   run[runi],
                   summary(conn$IIC*conn$total_area / 581540 )[2],
                   summary(conn$IIC*conn$total_area / 581540 )[4],
                   summary(conn$IIC*conn$total_area / 581540 )[3],
                   summary(conn$IIC*conn$total_area / 581540)[5])
    rowi=rowi+1
  }
}


###############################################################################


colorz = rev(RColorBrewer::brewer.pal(11,"Spectral"))
colorz = c("black", "#D53E4F", "#3288BD","#FDAE61")

#   "#5E4FA2" "#3288BD" "#66C2A5" "#ABDDA4" "#E6F598" "#FFFFBF" "#FEE08B" "#FDAE61" "#F46D43" "#D53E4F"
# [11] "#9E0142"
grDevices::png(file = file.path(pathi, paste0("connectivity_metrics_panels.png")),
               width = 150, height = 210, units='mm', res = 300)

par(mfrow=c(3,2), mar=c(3,4,0.5,1),oma=c(0,0,0,0))

p = c("total_area","fragments","buffer","mean_area","IIC_adj", "conservation_spending")
pname = c("Total Area Protected (% country)",
          "Number of Fragments",
          "Total Area of PA Edges (km2)",
          "Mean size of PAs (km2)",
          "Integral index of connectivity",#"Probability of Connectivity",
          "Cost (human density proxy)") # "Area of administrative unit")
# "Node-cut rate of connectivity loss (AUC)")
subplot_label= c("(a)","(b)","(c)","(d)","(e)","(f)")
iter=1
for(i in 1:length(p)){ #"ECA",
  plot_data = get(p[i])
  # if (p[i] == "total_area"  ){
  #   for(j in 3:6){
  #     plot_data[,j] = as.numeric(plot_data[,j])*1000000 / 581540
  #   }
  # }
  # if (p[i] == "mean_area" | p[i] == "buffer"){
  #   for(j in 3:6){
  #     plot_data[,j] = as.numeric(plot_data[,j])*1000000
  #   }
  # }

  plot(c(1,2,3,4,5),
       c(-1000000000,-1000000000,-1000000000,-1000000000,-1000000000),
       xaxt="n",
       # main = p,
       xlab= "",
       ylab= pname[i],
       # type = "o",
       pch=19,
       ylim = c(0, (max(as.numeric(plot_data[,"UL"])) + 0.1*max(as.numeric(plot_data[,"UL"])))))
  axis(1, at=c(1,2,3,4,5), labels = locname)
  text(1.2, 1.05*(max(as.numeric(plot_data[,"UL"]))),
       subplot_label[iter],cex=1.5)

  for(runi in 1:length(run)){

    id = which(plot_data$run %in% run[runi])

    polygon(c(1:5,5:1),
            c(as.numeric(plot_data[id,"LL"]),
              rev(as.numeric(plot_data[id,"UL"]))),
            col = rgb(col2rgb(colorz[runi])[1],
                      col2rgb(colorz[runi])[2],
                      col2rgb(colorz[runi])[3],
                      alpha=30,
                      maxColorValue = 255),
            border = rgb(col2rgb(colorz[runi])[1],
                         col2rgb(colorz[runi])[2],
                         col2rgb(colorz[runi])[3],
                         alpha=30,
                         maxColorValue = 255))

    points(c(1,2,3,4,5),
           as.numeric(plot_data[id,"median"]),
           col = colorz[runi],
           type = "o",
           pch=19,
           lwd=2
    )
    if (p[i] == "fragments" ){
      legend("bottomright",
             legend=runname,
             cex=0.8,
             inset=0.02,
             fill=colorz[1:4],
             title="Budget split")
    }

  }
  iter = iter + 1
}

dev.off()



###############################################################################
#
#
# colorz = rev(RColorBrewer::brewer.pal(11,"Spectral"))
# colorz = c("black", "#D53E4F", "#3288BD","#FDAE61")
#
# #   "#5E4FA2" "#3288BD" "#66C2A5" "#ABDDA4" "#E6F598" "#FFFFBF" "#FEE08B" "#FDAE61" "#F46D43" "#D53E4F"
# # [11] "#9E0142"
# grDevices::png(file = file.path(pathi, paste0("connectivity_metrics_panels.png")),
#                width = 150, height = 210, units='mm', res = 300)
#
# par(mfrow=c(3,2), mar=c(3,4,0.5,1),oma=c(0,0,0,0))
#
# p = c("total_area","fragments","buffer","mean_area","IIC", "conservation_spending")
# pname = c("Total Area Protected (% country)",
#           "Number of Fragments",
#           "Total Area of PA Edges (km2)",
#           "Mean size of PAs (km2)",
#           "Integral index of connectivity",#"Probability of Connectivity",
#           "Cost (human density proxy)")
#           # "Node-cut rate of connectivity loss (AUC)")
# subplot_label= c("(a)","(b)","(c)","(d)","(e)","(f)")
# iter=1
# for(i in 1:length(p)){ #"ECA",
#   plot_data = get(p[i])
#   # if (p[i] == "total_area"  ){
#   #   for(j in 3:6){
#   #     plot_data[,j] = as.numeric(plot_data[,j])*1000000 / 581540
#   #   }
#   # }
#   # if (p[i] == "mean_area" | p[i] == "buffer"){
#   #   for(j in 3:6){
#   #     plot_data[,j] = as.numeric(plot_data[,j])*1000000
#   #   }
#   # }
#
#   plot(c(1,2,3,4,5),
#        c(-1000000000,-1000000000,-1000000000,-1000000000,-1000000000),
#        xaxt="n",
#        # main = p,
#        xlab= "",
#        ylab= pname[i],
#        # type = "o",
#        pch=19,
#        ylim = c(0, (max(as.numeric(plot_data[,"UL"])) + 0.1*max(as.numeric(plot_data[,"UL"])))))
#   axis(1, at=c(1,2,3,4,5), labels = locname)
#   text(1.2, 1.05*(max(as.numeric(plot_data[,"UL"]))),
#        subplot_label[iter],cex=1.5)
#
#   for(runi in 1:length(run)){
#
#     id = which(plot_data$run %in% run[runi])
#
#     polygon(c(1:5,5:1),
#             c(as.numeric(plot_data[id,"LL"]),
#               rev(as.numeric(plot_data[id,"UL"]))),
#             col = rgb(col2rgb(colorz[runi])[1],
#                       col2rgb(colorz[runi])[2],
#                       col2rgb(colorz[runi])[3],
#                       alpha=30,
#                       maxColorValue = 255),
#             border = rgb(col2rgb(colorz[runi])[1],
#                          col2rgb(colorz[runi])[2],
#                          col2rgb(colorz[runi])[3],
#                          alpha=30,
#                          maxColorValue = 255))
#
#     points(c(1,2,3,4,5),
#            as.numeric(plot_data[id,"median"]),
#            col = colorz[runi],
#            type = "o",
#            pch=19,
#            lwd=2
#     )
#     if (p[i] == "fragments" ){
#       legend("bottomright",
#              legend=runname,
#              cex=0.8,
#              inset=0.02,
#              fill=colorz[1:4],
#              title="Budget split")
#     }
#
#   }
#   iter = iter + 1
# }
#
# dev.off()
#

#--------------------------------------------------------------------------

colorz = rev(RColorBrewer::brewer.pal(11,"Spectral"))
grDevices::png(file = file.path(pathi, paste0("connectivity_metrics_x4.png")),
               width = 190, height = 190, units='mm', res = 300)

par(mfrow=c(2,2), mar=c(3,4,0.5,1),oma=c(0,0,0,0))#,mar=c(5,5,2,2), oma=c(0,0,0,0))

p = c("PC","total_area","mean_area","fragments")
pname = c("Probability of Connectivity",
          "Total Area Protected (% country)",
          "Mean size of Protected Area (km2)",
          "Number of fragments")
subplot_label= c("(a)","(b)","(c)","(d)")
iter=1
for(i in 1:length(p)){ #"ECA",
  plot_data = get(p[i])
  # if (p[i] == "total_area"  ){
  #   for(j in 3:6){
  #     plot_data[,j] = as.numeric(plot_data[,j])
  #   }
  # }
  # if (p[i] == "mean_area"){
  #   for(j in 3:6){
  #     plot_data[,j] = as.numeric(plot_data[,j])*1000000
  #   }
  # }

  plot(c(1,2,3,4,5),
       c(-100000,-100000,-100000,-100000,-100000),
       xaxt="n",
       # main = p,
       xlab= "",
       ylab= pname[i],
       # type = "o",
       pch=19,
       ylim = c(0, ((max(as.numeric(plot_data[,"UL"])) )
                    + 0.1*(max(as.numeric(plot_data[,"UL"]))))))
  axis(1, at=c(1,2,3,4,5), labels = locname)
  text(1.2, 1.05*(max(as.numeric(plot_data[,"UL"]))),
                      subplot_label[iter],cex=1.5)

  for(runi in 1:length(run)){

    id = which(plot_data$run %in% run[runi])

    polygon(c(1:5,5:1),
            c(as.numeric(plot_data[id,"LL"]),
              rev(as.numeric(plot_data[id,"UL"]))),
            col = rgb(col2rgb(colorz[runi])[1],
                      col2rgb(colorz[runi])[2],
                      col2rgb(colorz[runi])[3],
                      alpha=30,
                      maxColorValue = 255),
            border = rgb(col2rgb(colorz[runi])[1],
                         col2rgb(colorz[runi])[2],
                         col2rgb(colorz[runi])[3],
                         alpha=30,
                         maxColorValue = 255))

    points(c(1,2,3,4,5),
           as.numeric(plot_data[id,"median"]),
           col = colorz[runi],
           type = "o",
           pch=19,
           lwd=2
    )

    if (p[i] == "mean_area" ){
      legend("bottomleft",
             legend=runname,
             cex=0.8,
             inset=0.02,
             fill=colorz[1:4],
             title="Budget split")
    }



  }
  iter = iter + 1
}

dev.off()


####################################################################################
# get the stats for text
####################################################################################

table= total_area

for (p in c("total_area","fragments","buffer","mean_area","IIC_adj","conservation_spending")){
  table= get(p)
  out = ((as.numeric(table$median[table$adm == "MDG_adm3"])-
            as.numeric(table$median[table$adm == "MDG_adm0"])) /
           (as.numeric(table$median[table$adm == "MDG_adm0"])))*100
  print("--------------------------------------")
  print(p)
  print("--------------------------------------")
  print(table$run[table$adm == "MDG_adm3"][c(3,1,4,2)])
  print( out[c(3,1,4,2)])
  print("--------------------------------------")

}

