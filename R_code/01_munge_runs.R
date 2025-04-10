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



Planning_Units_ZONES = read.csv(file.path(filepath, "Planning_units_zones_pop_5km.csv"))


#############################################
# STAGGERED HISTOGRAMS
#############################################

# grDevices::png(file = file.path(filepath, paste0("metrics_run_stacked_",dispersal_dist,"_zoom.png")),
#                width = 200, height = 200, units='mm', res = 300)
subplot_label= c("(a)","(b)","(c)","(d)")
par(mfrow=c(2,2), mar=c(2,1,0.5,0.5), oma=c(5,5,0,0))
# Loop over everything to create staggered histgrams
for(runi in 1:length(run)){
  for(loci in 1:length(loc)){
  # for(loci in 1){
    iter= 2
    # loci = 1
    height = 2
    # species data
    species_data = read.csv(file.path(data_dir, loc[loci],
                                      paste0("species_protected_fraction_",loc[loci],"_",run[runi],"_run_",iter,".csv")))

    # PA data
    protected_data = read.csv(file.path(data_dir, loc[loci],
                                        paste0("all_results_",loc[loci],"_",run[runi],"_pop_5km_run_",iter,".csv")))
    Optim = cbind(protected_data[,c("FID","PUID","Coord_x","Coord_y")],protected_data$Optim)
    Cost = cbind(protected_data[,c("FID","PUID","Coord_x","Coord_y")],protected_data$Cost)
    Disturbance = cbind(protected_data[,c("FID","PUID","Coord_x","Coord_y")],protected_data$Disturbance)
    Species_per_PU = cbind(protected_data[,c("FID","PUID","Coord_x","Coord_y")],protected_data$Species_per_PU)
    Budget = cbind(protected_data[,c("FID","PUID","Coord_x","Coord_y")],protected_data$Budget)
    Remaining_Budget = cbind(protected_data[,c("FID","PUID","Coord_x","Coord_y")],protected_data$Remaining_Budget)
    Species_number_below_target = cbind(protected_data[,c("FID","PUID","Coord_x","Coord_y")],protected_data$Species_number_below_target)
    Proportion_below_target = cbind(protected_data[,c("FID","PUID","Coord_x","Coord_y")],protected_data$Proportion_below_target)

    protected_data$Unaffordable = ifelse(protected_data$Budget < protected_data$Cost, 1, 0)
    Unaffordable = cbind(protected_data[,c("FID","PUID","Coord_x","Coord_y")],protected_data$Unaffordable)

    protected_data$Spending = protected_data$Optim * protected_data$Cost
    Spending = cbind(protected_data[,c("FID","PUID","Coord_x","Coord_y")],protected_data$Spending)

    percent_protect = sum(protected_data$Optim,na.rm=T)/length(protected_data$Optim)
    conservation_spending = sum(protected_data$Spending, na.rm = T)
    adm_area = length(!is.na(protected_data$Optim))


    # connectivity metrics
    connectivity =  read.table(file.path(data_dir, loc[loci],
                                         paste0("connectivity_",loc[loci],"_",run[runi],"_pop_5km_10_run_",iter,".txt")),
                               sep="\n" , header=T)
    # to_keep=c()
    for(i in 1:nrow(connectivity)){
      no_good = TRUE
      while(no_good){
        try(
          if(i == as.numeric(strsplit(paste0(as.character(connectivity[i,1])), split =",")[[1]][1])){
            no_good=FALSE
          } ,TRUE)
        try(
          if(i != as.numeric(strsplit(paste0(as.character(connectivity[i,1])), split =",")[[1]][1])){
            connectivity = as.data.frame(connectivity[-i,1])
            if(i == as.numeric(strsplit(paste0(as.character(connectivity[i,1])), split =",")[[1]][1])){
              no_good=FALSE
            }
          },TRUE)
        try(
          if(is.na(i != as.numeric(strsplit(paste0(as.character(connectivity[i,1])), split =",")[[1]][1]))){
            connectivity = as.data.frame(connectivity[-i,1])
            if(i == as.numeric(strsplit(paste0(as.character(connectivity[i,1])), split =",")[[1]][1])){
              no_good=FALSE
            }
          },TRUE)
        # print(i)
        if (i > nrow(connectivity)){
          break
        }
      }
    }

    test = stringr::str_split_fixed(connectivity[,1], "\\([^()]*\\)", 2)
    test1 = stringr::str_split_fixed(test[,1], ",", 8)[,1:7]
    test2 = stringr::str_split_fixed(test[,2], ",", 5)[,2:5]

    connectivity$area = as.numeric(test1[,2])
    connectivity$buffr_r = as.numeric(test1[,3])
    PC = as.numeric(test2[1,1])
    ECA = as.numeric(test2[1,2])
    IIC = as.numeric(test2[1,3])
    AUC = as.numeric(test2[1,4])
    total_area = sum(as.numeric(connectivity$area),na.rm=T)
    buffer = sum(as.numeric(connectivity$buffr_r),na.rm=T)
    mean_area = mean(as.numeric(connectivity$area),na.rm=T)
    fragments = length(which(!is.na(connectivity$area)))


    for(iter in 3:100){
      # try in case run didn't work
      try(
        {temp = read.csv(file.path(data_dir, loc[loci],
                                   paste0("species_protected_fraction_",loc[loci],"_",run[runi],"_run_",iter,".csv")))

        species_data$Protect_Ind_per_spp = species_data$Protect_Ind_per_spp + temp$Protect_Ind_per_spp
        species_data$Ind_per_spp = species_data$Ind_per_spp + temp$Ind_per_spp
        species_data$protected_fraction = species_data$Protect_Ind_per_spp/species_data$Ind_per_spp

        # PAs
        temp = read.csv(file.path(data_dir, loc[loci],
                                  paste0("all_results_",loc[loci],"_",run[runi],"_pop_5km_run_",iter,".csv")))

        protected_data$Optim = protected_data$Optim + temp$Optim
        protected_data$Cost = protected_data$Cost + temp$Cost
        protected_data$Disturbance = protected_data$Disturbance + temp$Disturbance
        protected_data$Species_per_PU = protected_data$Species_per_PU + temp$Species_per_PU
        protected_data$Budget = protected_data$Budget + temp$Budget
        protected_data$Remaining_Budget = protected_data$Remaining_Budget + temp$Remaining_Budget
        protected_data$Species_number_below_target = protected_data$Species_number_below_target + temp$Species_number_below_target
        protected_data$Proportion_below_target = protected_data$Proportion_below_target + temp$Proportion_below_target

        temp$Unaffordable = ifelse(temp$Budget > temp$Cost, 1, 0)
        protected_data$Unaffordable = protected_data$Unaffordable + temp$Unaffordable

        temp$Spending = temp$Optim * temp$Cost
        protected_data$Spending = protected_data$Spending + temp$Spending


        Optim = cbind(Optim, temp$Optim)
        Disturbance = cbind(Disturbance, temp$Disturbance)
        Species_per_PU = cbind(Species_per_PU, temp$Species_per_PU)
        Species_number_below_target = cbind(Species_number_below_target, temp$Species_number_below_target)
        Proportion_below_target = cbind(Proportion_below_target, temp$Proportion_below_target)

        Unaffordable = cbind(Unaffordable, temp$Unaffordable)
        Spending = cbind(Spending, temp$Spending)





        # connectivity metrics
        connectivity =  read.table(file.path(data_dir, loc[loci],
                                             paste0("connectivity_",loc[loci],"_",run[runi],"_pop_5km_10_run_",iter,".txt")),
                                   sep="\n" , header=T)

        # to_keep=c()
        for(i in 1:nrow(connectivity)){
          no_good = TRUE
          while(no_good){
            try(
              if(i == as.numeric(strsplit(paste0(as.character(connectivity[i,1])), split =",")[[1]][1])){
                no_good=FALSE
              } ,TRUE)
            try(
              if(i != as.numeric(strsplit(paste0(as.character(connectivity[i,1])), split =",")[[1]][1])){
                connectivity = as.data.frame(connectivity[-i,1])
                if(i == as.numeric(strsplit(paste0(as.character(connectivity[i,1])), split =",")[[1]][1])){
                  no_good=FALSE
                }
              },TRUE)
            try(
              if(is.na(i != as.numeric(strsplit(paste0(as.character(connectivity[i,1])), split =",")[[1]][1]))){
                connectivity = as.data.frame(connectivity[-i,1])
                if(i == as.numeric(strsplit(paste0(as.character(connectivity[i,1])), split =",")[[1]][1])){
                  no_good=FALSE
                }
              },TRUE)
            # print(i)
            if (i > nrow(connectivity)){
              break
            }
          }
        }

        test = stringr::str_split_fixed(connectivity[,1], "\\([^()]*\\)", 2)
        test1 = stringr::str_split_fixed(test[,1], ",", 8)[,1:7]
        test2 = stringr::str_split_fixed(test[,2], ",", 5)[,2:5]

        connectivity$area = as.numeric(test1[,2])
        connectivity$buffr_r = as.numeric(test1[,3])

        PC = c(PC, max(as.numeric(test2[1:4,1]),na.rm=T))
        ECA = c(ECA, max(as.numeric(test2[1:4,2]),na.rm=T))
        IIC = c(IIC, max(as.numeric(test2[1:4,3]),na.rm=T))
        conn = data.table::fread(file.path(data_dir, loc[loci],
                                           paste0("connectivity_",loc[loci],"_",run[runi],
                                                  "_pop_5km_10_run_",iter,".txt")),
                                 nrows = 1,skip=1)

        coli = ncol(conn)
        AUC = c(AUC,as.numeric(conn[[coli]]))
        # AUC = c(AUC, max(as.numeric(test2[1:114,4]),na.rm=T))
        total_area = c(total_area, sum(as.numeric(connectivity$area),na.rm=T))
        buffer = c(buffer, sum(as.numeric(connectivity$buffr_r),na.rm=T))
        mean_area = c(mean_area, mean(as.numeric(connectivity$area),na.rm=T))
        fragments = c(fragments, length(which(!is.na(connectivity$area))))
        conservation_spending = c(conservation_spending, sum(protected_data$Spending, na.rm = T))


        }, TRUE )
    }
    conn_stats = cbind(PC, ECA, IIC, AUC,
                       total_area, buffer, mean_area, fragments, conservation_spending, adm_area)

    write.csv(conn_stats,
              file.path(data_dir, loc[loci],
                        paste0("summary_connectivity_",loc[loci],"_",run[runi],".csv")))


    write.csv(species_data,
              file.path(data_dir, loc[loci],
                        paste0("summary_species_protected_",loc[loci],"_",run[runi],".csv")))

    write.csv(protected_data,
              file.path(data_dir, loc[loci],
                        paste0("summary_PAs_",loc[loci],"_",run[runi],".csv")))
    #add for plots
    write.csv( Unaffordable,
               file.path(data_dir, loc[loci],
                         paste0("summary_Unaffordable_",loc[loci],"_",run[runi],".csv")))
    write.csv( Spending,
               file.path(data_dir, loc[loci],
                         paste0("summary_Spending_",loc[loci],"_",run[runi],".csv")))





  }
}

