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
locname = c("national","province","region","district","commune")
# a. Read it into R as a data frame, with columns coord_x, Coord_y, Optim, yield.
filepath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/madagascar/data/captain/population_cost_data/5km"
files = list.files(path = data_dir, pattern = "all_results_MDG_.*\\.csv$", recursive = TRUE)


# grDevices::pdf(file = file.path(filepath, paste0("all_results_plotted_",dispersal_dist,".pdf")),
#                width = 14, height = 20)
# #width = 5, height = 6)

pathi = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/madagascar"

Planning_Units_ZONES = read.csv(file.path(filepath, "Planning_units_zones_pop_5km.csv"))


#############################################
# STAGGERED HISTOGRAMS
#############################################

grDevices::png(file = file.path(pathi, paste0("stacked_histograms.png")),
               width = 200, height = 200, units='mm', res = 300)
subplot_label= c("(a)","(b)","(c)","(d)")
par(mfrow=c(2,2), mar=c(2,1,0.5,0.5), oma=c(5,5,0,0))
# Loop over everything to create staggered histgrams
for(runi in 1:length(run)){

  loci = 1
  height = 2

  species_hist = read.csv(file.path(data_dir, loc[loci],
                                    paste0("summary_species_protected_",loc[loci],"_",run[runi],".csv")))

  dx = density(species_hist$protected_fraction,adjust=1)

  test = approx(dx$x, dx$y, xout =seq(0,1,0.001))
  test$x = ifelse(is.na(test$y), NA, test$x)
  test$y = test$y/max(test$y,na.rm=T) # normalise to 1
  test$x = c(0,test$x[!is.na(test$x)],1)
  test$y = c(0,test$y[!is.na(test$y)],0)
  plot(test$x,
       test$y+height,
       type="l",
       xlim=c(0,0.6),
       ylim=c(0,3),
       xlab = "Proportion of species ranges protected",
       ylab="Density",
       yaxt="n",
       col=colorz[loci])
  text(0.02,2.8,subplot_label[runi],cex=1.5) #add text

  # add p value
  p_value = length(which((species_hist$protected_fraction < 0.1)))/length(which(!is.na(species_hist$protected_fraction)))
  text(0.55,height,
       paste(round(100-(p_value*100),1),"%"),adj=0,pos=3,cex=1)

  abline(h=height)

  polygon(test$x,
          test$y+height,
          col = rgb(col2rgb(colorz[loci])[1],
                    col2rgb(colorz[loci])[2],
                    col2rgb(colorz[loci])[3],
                    alpha=140,
                    maxColorValue = 255),
          border = NULL)

  if (loci == 1 && runi == 4){
    text(0.37,height+0.1,
         paste(round(100-(p_value*100),1),"%"),adj=0,pos=3,cex=1)
  }



  for(loci in 2:length(loc)){
    height = height - 0.5
    species_hist = read.csv(file.path(data_dir, loc[loci],
                                      paste0("summary_species_protected_",loc[loci],"_",run[runi],".csv")))

    dx = density(species_hist$protected_fraction,adjust=1)

    test = approx(dx$x, dx$y, xout =seq(0,1,0.001))
    test$x = ifelse(is.na(test$y), NA, test$x)
    test$y = test$y/max(test$y,na.rm=T) # normalise to 1
    test$x = c(0,test$x[!is.na(test$x)],1)
    test$y = c(0,test$y[!is.na(test$y)],0)
    lines(test$x,
          test$y+height,
          type="l",
          col=colorz[loci])
    abline(h=height)
    p_value = length(which((species_hist$protected_fraction < 0.1)))/length(which(!is.na(species_hist$protected_fraction)))

    text(0.55,height,
         paste(round(100-(p_value*100),1),"%"),adj=0,pos=3,cex=1)



    polygon(test$x,
            test$y+height,
            col = rgb(col2rgb(colorz[loci])[1],
                      col2rgb(colorz[loci])[2],
                      col2rgb(colorz[loci])[3],
                      alpha=140,
                      maxColorValue = 255),
            border = NULL)

    if(runi %in% c(3,4)){
      mtext("Proportion of species range protected",side = 1, line=4)

    }
    if(runi %in% c(1,3)){
      mtext("Density",side = 2, line=4)

    }
    if (runi ==4 ){
      legend("topright",
             legend=locname,
             # legend=lapply(1:5, FUN= function(i) strsplit(loc[i], split="[_]")[[1]][2]),#loc,
             cex=0.92,
             fill=colorz[1:5],
             bg="white",#NA,
             # box.lwd = NA,
             # inset=0.02,
             title="Administration")


    }
  }

  abline(v=0.1, lwd=2,lty=2)
}
# add labels in the margin
dev.off()



#############################################################
#  t-test

data = matrix(NA,nrow=1,ncol=3)
colnames(data) = c("protected_fraction","adm", "run")

for(runi in 1:length(run)){

  loci = 1

  base_hist = read.csv(file.path(data_dir, loc[loci],
                                 paste0("summary_species_protected_",loc[loci],"_",run[runi],".csv")))

  to_add = cbind(base_hist$protected_fraction, loci,runi)
  colnames(to_add) = c("protected_fraction","adm", "run")

  data = rbind(data,to_add)
  remove = which(is.na(data[,"run"]))
  if(length(remove)>0){  data = data[-remove,]}


  p = NA
  for(loci in 2:length(loc)){
    species_hist = read.csv(file.path(data_dir, loc[loci],
                                      paste0("summary_species_protected_",loc[loci],"_",run[runi],".csv")))
    to_add = cbind(species_hist$protected_fraction, loci, runi)
    colnames(to_add) = c("protected_fraction","adm", "run")

    data = rbind(data,to_add)
    # data = cbind(base_hist$protected_fraction,species_hist$protected_fraction)
    # colnames(data) = c("base","comparison")


    subset = which((data[,"adm"] == 1 | data[,"adm"] == loci) & data[,"run"] == runi)
    # t.test(protected_fraction ~ adm,  data = data[subset,], paired= TRUE)
    out = t.test(protected_fraction ~ adm,  data = data[subset,],  paired = T)
    print(paste0("RUN: ",loc[loci],"_",run[runi]))
    print(out)
    p=c(p, out$p.value)

  }
  print(p)
  boxplot(protected_fraction ~ adm, data = data)
}



library(tidyverse)
library(rstatix)
library(ggpubr)

mydata = as_tibble(data)
mydata[,"run"] = run[data[,"run"]]
mydata[,"adm"] = loc[data[,"adm"]]


mydata.long <- mydata[,c(3,2,1)]


mydata.long %>%
  group_by(run,adm) %>%
  summarise(
    n = n(),
    mean = mean(protected_fraction),
    sd = sd(protected_fraction)
  ) %>%
  ungroup()


stat.test <- mydata.long %>%
  group_by(run) %>%
  t_test(protected_fraction ~ adm, p.adjust.method = "holm", alternative = "lower") #, paired=T, var.equal = T
#"holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none",  ref.group = "MDG_adm0"
# Remove unnecessary columns and display the outputs
stat.test %>% select(-.y., -statistic, -df)


# Create the plot
myplot <- ggboxplot(
  mydata.long, x = "adm", y = "protected_fraction",
  fill = "adm", palette = "npg", legend = "none",
  ggtheme = theme_pubr(border = TRUE)
) +
  facet_wrap(~run)
# Add statistical test p-values
stat.test <- stat.test %>% add_xy_position(x = "adm")
myplot + stat_pvalue_manual(stat.test, label = "p.adj.signif")




#############################################################
#  t-test for allll the data

data = matrix(NA, nrow=1, ncol=3)
colnames(data) = c("protected_fraction","adm", "run")

for(runi in 1:length(run)){
  p = NA
  for(loci in 1:length(loc)){
    listi= list.files(path= file.path(data_dir, loc[loci]),
                    pattern = paste0("species_protected_fraction_",loc[loci],"_",run[runi],".*\\.csv"))
    listi=listi[1:89]
    # print(length(listi))
    for(filei in listi){
      species_hist = read.csv(file.path(data_dir, loc[loci],filei))

      to_add = cbind(species_hist$protected_fraction, loci, runi)
      colnames(to_add) = c("protected_fraction","adm", "run")

      data = rbind(data, to_add)
      remove = which(is.na(data[,"run"]))
      if(length(remove)>0){  data = data[-remove,]}
    }

    # subset = which((data[,"adm"] == 1 | data[,"adm"] == loci) & data[,"run"] == runi)
    # out = t.test(protected_fraction ~ adm,  data = data[subset,],  paired = T)
    # print(paste0("RUN: ",loc[loci],"_",run[runi]))
    # print(out)
    # p=c(p, out$p.value)

  }
  # print(p)
  boxplot(protected_fraction ~ adm, data = data)
  # }
}





mydata = as_tibble(data)
mydata[,"run"] = run[data[,"run"]]
mydata[,"adm"] = loc[data[,"adm"]]


mydata.long <- mydata[,c(3,2,1)]


mydata.long %>%
  group_by(run,adm) %>%
  summarise(
    n = n(),
    mean = mean(protected_fraction),
    sd = sd(protected_fraction)
  ) %>%
  ungroup()

# stat.test <- mydata.long %>%
#   pairwise_t_test(
#     protected_fraction ~ adm, paired = TRUE,
#     p.adjust.method = "bonferroni"
#   )
#
# # Create the plot
# myplot <- ggboxplot(mydata.long, x = "adm", y = "protected_fraction", add = "point")
# # Add statistical test p-values
# stat.test <- stat.test %>% add_xy_position(x = "adm")
# myplot + stat_pvalue_manual(stat.test, label = "p.adj.signif")
#




# stat.test
stat.test <- mydata.long %>%
  group_by(run) %>%
  wilcox_test(protected_fraction ~ adm, p.adjust.method = "holm", paired =T)
  # t_test(protected_fraction ~ adm, p.adjust.method = "holm", alternative = "two.sided",paired=T)
  #t_test(protected_fraction ~ adm, p.adjust.method = "bonferroni") # ref.group = "MDG_adm4",
 #"holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"
# Remove unnecessary columns and display the outputs
# stat.test %>% select(-.y., -statistic, -df)


# Create the plot
myplot <- ggboxplot(
  mydata.long, x = "adm", y = "protected_fraction",
  fill = "adm", palette = "grey", legend = "none",
  ggtheme = theme_pubr(border = TRUE)) +
  facet_wrap(~run)

# Add statistical test p-values
stat.test <- stat.test %>% add_xy_position(x = "adm")
myplot + stat_pvalue_manual(stat.test, label = "p.adj.signif")


