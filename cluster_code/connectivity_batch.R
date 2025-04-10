# get the input passed from the shell script
args <- commandArgs(trailingOnly = TRUE)
str(args)
cat(args, sep = "\n")
repetition = args[1]
print("TASK ID:")
print(repetition)


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


# a. Read it into R as a data frame, with columns coord_x, Coord_y, Optim, yield.
#filepath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/madagascar/data/captain/population_cost_data/5km"
filepath = "/mnt/shared/scratch/kdhanjal/madagascar/"
# files = list.files(path = filepath, pattern = "all_results_MDG_", recursive = TRUE)

# loc = c("MDG_adm0","MDG_adm1","MDG_adm2","MDG_adm3","MDG_adm4")
loc = c("0_level", "1st_level", "2nd_level", "3rd_level", "4th_level")

run = c("equal_cost","biodiversity_cost", "pop_cost","area_cost") #"pop_cost","area_cost"

costype - c("hf_cost", "no_cost")

dispersal_dist = 10#100#10000 #300


pts_PU= rgdal::readOGR(filepath,
                       "Planing_Units_5x5")


for (costi in 1:length(costype)){
  for(runi in 1:length(run)){
    for(loci in 1:length(loc)){
      print("                                               ")
      print("***********************************************")
      print("                                               ")
      print(paste(costype[costi],loc[loci],run[runi]))
      print("                                               ")
      print("***********************************************")
      print("                                               ")
      
      par(mfrow=c(1,1))
      
      # load data
      pts = read.csv(file.path(filepath, costype[costi], loc[loci] ,"summary",
                               paste0("all_results_",costype[costi],"_",loc[loci],"_",run[runi],"_run_", repetition,".csv")))
      
      # extract lat lon
      coordinates(pts)=~Coord_x+Coord_y
      
      # add model run output data to spatial data
      pts2 <- pts_PU
      pts2@data = dplyr::left_join(pts2@data, pts[c("PUID","Optim", "Cost", "Disturbance", "Budget", "Region")],
                                   by=c("Id" = "PUID"),
                                   copy=TRUE)
      
      # make sure projection and extents are correct
      pgeo <- spTransform(pts2, CRS('+proj=longlat +datum=WGS84'))
      ext <- floor(extent(pgeo))
      rr <- raster(ext, res=0.05)
      rr <- rasterize(pgeo, rr, field="Optim")
      
      # convert landscape grid to shp by summarising raster cell information
      grain_poly = rasterToPolygons(rr) %>% st_as_sf()
      grain_poly2 = grain_poly %>%
        group_by(layer) %>%
        summarize()
      
      # combine protected cells into PAs i.e. one big polygon instead of several small
      PAs = grain_poly2[grain_poly2$layer %in% 1]
      unique_PAs = st_cast(PAs,"MULTIPOLYGON")
      unique_PAs = st_cast(unique_PAs,"POLYGON")
      
      # extract area of PAs (for connectivity stats)
      unique_PAs$area <- st_area(unique_PAs)
      unique_PAs = unique_PAs[as.numeric(st_area(unique_PAs)) < max(st_area(unique_PAs)),]#unique_PAs$geometry[st_area(unique_PAs) < max(st_area(unique_PAs))]#unique_PAs$geometry[unique_PAs$area < 44.33]
      unique_PAs$area <- st_area(unique_PAs)
      
      # buffer the PAs
      PA_buffer <-  st_buffer(unique_PAs, dist= -0.001) # roughly 111m
      unique_PAs$buffer_area <- unique_PAs$area - st_area(PA_buffer)

      # extract centroids of PAs (for connectivity stats)
      centroid_coords = st_coordinates(st_centroid(unique_PAs))
      unique_PAs$x = centroid_coords[,1]
      unique_PAs$y = centroid_coords[,2]
      unique_PAs$ID = 1:nrow(centroid_coords) 
      unique_PAs$spp = 1
      st_crs(unique_PAs) = CRS('+proj=longlat +datum=WGS84')
      
      # save the polygon
      st_write(unique_PAs, file.path(filepath, costype[costi], loc[loci] ,"summary",
                                     paste0("all_results_",costype[costi],"_",loc[loci],"_",run[runi],"_run_", repetition,".shp")),
               append=FALSE)
      
      unique_PAs = sf::read_sf(
        dsn = file.path(filepath, costype[costi], loc[loci] ,"summary"),
        layer = paste0("all_results_",costype[costi],"_",loc[loci],"_",run[runi],"_run_", repetition)
      )
      
      rl1 = import.shape(filename = paste0("all_results_",costype[costi],"_",loc[loci],"_",run[runi],"_run_", repetition,".shp"),
                         path = file.path(filepath, costype[costi], loc[loci] ,"summary"),
                         species.col = "spp",
                         ID.col = "ID",
                         area.col = "area", dispersal = dispersal_dist,
                         class.landscape=TRUE)
      
      file.remove(file.path(filepath, costype[costi], loc[loci] ,"summary",
                            paste0("all_results_",costype[costi],"_",loc[loci],"_",run[runi],"_run_", repetition,".shp")))
      file.remove(file.path(filepath, costype[costi], loc[loci] ,"summary",
                            paste0("all_results_",costype[costi],"_",loc[loci],"_",run[runi],"_run_", repetition,".shx")))
      file.remove(file.path(filepath, costype[costi], loc[loci] ,"summary",
                            paste0("all_results_",costype[costi],"_",loc[loci],"_",run[runi],"_run_", repetition,".prj")))
      file.remove(file.path(filepath, costype[costi], loc[loci] ,"summary",
                            paste0("all_results_",costype[costi],"_",loc[loci],"_",run[runi],"_run_", repetition,".dbf")))
      
      start_time <- Sys.time()
      PC = metrics.graph(rl1, metric = 'PC', dispersal.dist = dispersal_dist)
      end_time <- Sys.time()
      print("                                   ")
      print(paste0("PC: ",PC))
      print(end_time - start_time)
      
      start_time <- Sys.time()
      ECA = metrics.graph(rl1, metric = 'ECA', dispersal.dist = dispersal_dist)
      end_time <- Sys.time()
      print("                                   ")
      print(paste0("ECA: ",ECA))
      print(end_time - start_time)
      
      start_time <- Sys.time()
      IIC = metrics.graph(rl1, metric = 'IIC', dispersal.dist = dispersal_dist)
      end_time <- Sys.time()
      print("                                   ")
      print(paste0("IIC: ",IIC))
      print(end_time - start_time)
      
      unique_PAs$PC = PC
      unique_PAs$ECA = ECA
      unique_PAs$IIC = IIC
      
      pts = data.frame(unique_PAs)#[,1:6]
      pts$PC = PC
      pts$ECA = ECA
      pts$IIC = IIC
      
      ##############################################
      #### migflow connectivity
      start_time <- Sys.time()
      
      pop = 40000000000000
      par(mfrow=c(1,1))
      # Simulate 10 fake tracks with a mean distance of 500km
      tracks <- rnorm(dispersal_dist/3, 0, 100)
      # tracks = tracks[tracks>=0] # to make it a distance decay
      hist(tracks)
      # rgamma(10, 500, 200)
      
      # Create a fake list of sites where animals were seen at, with latitude, longitude and number of anumals seen there
      dta <- data.frame(Site = 1:length(unique_PAs$geometry) ,
                        Lat = unique_PAs$y,#centroid_coords[,2],
                        Lon = unique_PAs$x,#centroid_coords[,1],
                        Pop = unique_PAs$area*100*10000)
      
      dta = dta[order(dta$Lat,decreasing = T),]
      
      # create a distance matrix based on these data
      dist <- point2DIST(dta)
      
      # calculate the probability of going between these sites given the distance the animal can travel
      Dist_P <- distPROB(tracks, dist, adjust=2, plot=) #+ 0.000000000000001
      
      # Calculate proportion of population using a site
      Pop_P <- nodePopPROP(dta, pop)
      
      # make birds/animals prefer sites which a larger proportion of the population has been seen and where the distance is better
      network <- Dist_P * Pop_P #* Azi_P
      
      # Make the network directed
      network <- directedNET(network, include_diagonal = TRUE)
      
      #Add supersource and sink nodes
      network <- addSUPERNODE(network, sources=dta$Site[dta$Lat > (max(dta$Lat)-0.1)],
                              sinks= dta$Site[dta$Lat < (min(dta$Lat)+0.1)])
      
      
      # proportion
      prop = network
      for(i in 2:nrow(network)){
        prop[i,i:(ncol(prop)-1)] = network[i,i:(ncol(network)-1)]/sum(network[i,i:(ncol(network)-1)])
      }
      
      
      prop[is.na(prop)]=0
      network = pop * prop
      network["supersource",network["supersource",]==Inf] = pop / length(network["supersource",network["supersource",]==Inf])
      
      network = round(network)
      
      #estimate number of birds entering and exiting sites based on distance, population count and azimuth
      # network <- network * pop#popPROP(network, pop)
      
      # network
      sites<- rbind(
        c("supersource",max(dta$Lat)+1,mean(dta$Lon),pop),
        dta,
        c("supersink", min(dta$Lat)-1,mean(dta$Lon),pop))
      
      full_site_list = sites
      
      
      full_net = network
      network = full_net
      toplot=T
      
      library(igraph)
      ##test
      weight <- igraph::graph_from_adjacency_matrix(network,  mode="directed", weighted = TRUE)
      
      # run the population through the network a forst time
      flow = max_flow(weight, source = V(weight)["supersource"],
                      target = V(weight)["supersink"], capacity = E(weight)$weight)
      
      #######################################################################
      # plot flow network
      nodes = get.edgelist(weight, names=TRUE)
      nodes = as.data.frame(nodes)
      nodes$flow = flow$flow
      # nodes$V1 <- substring(nodes$V1, 2)
      # nodes$V2 <- substring(nodes$V2, 2)
      
      nodes = nodes[nodes$V1 != "supersource" & nodes$V2 != "supersink" ,]
      
      nodes$Lat_from = unlist(lapply(1:nrow(nodes), function(i) as.numeric(sites$Lat[sites$Site %in% nodes[i,1]])))
      nodes$Lon_from = unlist(lapply(1:nrow(nodes), function(i) as.numeric(sites$Lon[sites$Site %in% nodes[i,1]])))
      nodes$Lat_to   = unlist(lapply(1:nrow(nodes), function(i) as.numeric(sites$Lat[sites$Site %in% nodes[i,2]])))
      nodes$Lon_to   = unlist(lapply(1:nrow(nodes), function(i) as.numeric(sites$Lon[sites$Site %in% nodes[i,2]])))
      
      # library(shape)
      #par(mfrow=c(1,3))
      #par(mar=c(4,4,4,4))
      index=2:(nrow(sites)-1)
      #plot(sites$Lon[index], sites$Lat[index], pch=16,
      #     cex=0, xlab="", ylab="", xaxt="n", yaxt = "n",
      #     frame.plot=FALSE)
      
      index=1:nrow(nodes)#2:(nrow(nodes)-1)#
      
      #segments(x0 = nodes$Lon_from[index],
      #         y0 = nodes$Lat_from[index],
      #         x1 = nodes$Lon_to[index],
      #         y1 = nodes$Lat_to[index],
      #         col= "black",#adjustcolor("royalblue3", alpha.f = 0.9),
      #         lwd=(nodes$flow[index]/(max(nodes$flow, na.rm = T)))*2)
      
      
      
      nodeflow = merge(aggregate(nodes$flow, by=list(Category=as.character(nodes$V1)), FUN=sum, na.rm=TRUE),
                       aggregate(nodes$flow, by=list(Category=as.character(nodes$V2)), FUN=sum, na.rm=TRUE), all=T)
      nodeflow$x = as.numeric(nodeflow$x)
      
      nodeflow = data.frame( unique(as.matrix(nodeflow[ , 1:2 ]) ))
      nodeflow$x = as.numeric(as.character(nodeflow$x))
      
      nodeflow = nodeflow[nodeflow$Category != "supersource" & nodeflow$Category != "supersink",]
      
      nodeflow = nodeflow[order(nodeflow$x),]
      to_remove = which(nodeflow$x %in% min(nodeflow$x, na.rm=TRUE))
      
      iter = 1
      
      # empty dataset to store output
      prioritisation <- data.frame(Site=as.character(nodeflow$Category[to_remove]),
                                   Pop_Flow =   flow$value,
                                   Site_Flow =   nodeflow$x[to_remove],
                                   Iteration = iter)
      
      net_remove = which(colnames(network) %in% nodeflow$Category[to_remove])
      dim(network[-net_remove,-net_remove ])
      network = network[-net_remove,-net_remove ]
      
      while(dim(network)[1] > 2 & sum(network,na.rm=T) > 0){
        
        
        ###############################
        #  start to reallocate flow...
        # test
        weight <- igraph::graph_from_adjacency_matrix(network,  mode="directed", weighted = TRUE)
        # if(any(is.na(E(weight)$weight))){
        # E(weight)$weight[which(is.na(E(weight)$weight))]=0
        # }
        
        # run the population through the network a forst time
        flow = max_flow(weight, source = V(weight)["supersource"],
                        target = V(weight)["supersink"], capacity = E(weight)$weight)
        
        nodes = get.edgelist(weight, names=TRUE)
        nodes = as.data.frame(nodes)
        nodes$flow = flow$flow
        if (length(nodes$flow) == 1){
          nodes$flow[is.na(nodes$flow)] = 0
          iter = iter + 1
          
          to_remove = get.edgelist(weight, names=TRUE)[!(get.edgelist(weight, names=TRUE) %in% c("supersink","supersource"))]
          
          prioritisation <- rbind(prioritisation,
                                  data.frame(Site = to_remove,
                                             Pop_Flow = flow$value,
                                             Site_Flow = nodes$flow,
                                             Iteration = iter))
          break
        }
        
        
        
        
        nodes = nodes[nodes$V1 != "supersource" & nodes$V2 != "supersink" ,]
        
        nodes$Lat_from = unlist(lapply(1:nrow(nodes), function(i) as.numeric(sites$Lat[sites$Site %in% nodes[i,1]])))
        nodes$Lon_from = unlist(lapply(1:nrow(nodes), function(i) as.numeric(sites$Lon[sites$Site %in% nodes[i,1]])))
        nodes$Lat_to   = unlist(lapply(1:nrow(nodes), function(i) as.numeric(sites$Lat[sites$Site %in% nodes[i,2]])))
        nodes$Lon_to   = unlist(lapply(1:nrow(nodes), function(i) as.numeric(sites$Lon[sites$Site %in% nodes[i,2]])))
        
        nodes$flow[is.na(nodes$flow)] = 0
        
        
        #  end to reallocate flow...
        ###############################
        
        
        nodeflow = merge(aggregate(nodes$flow, by=list(Category=as.character(nodes$V1)), FUN=sum, na.rm=TRUE),
                         aggregate(nodes$flow, by=list(Category=as.character(nodes$V2)), FUN=sum, na.rm=TRUE), all=T)
        nodeflow$x = as.numeric(nodeflow$x)
        
        nodeflow = data.frame( unique(as.matrix(nodeflow[ , 1:2 ]) ))
        nodeflow$x = as.numeric(as.character(nodeflow$x))
        
        nodeflow = nodeflow[nodeflow$Category != "supersource" & nodeflow$Category != "supersink",]
        
        # nodeflow = nodeflow[order(nodeflow$x),]
        to_remove = which(nodeflow$x %in% min(nodeflow$x, na.rm=TRUE))
        
        
        iter = iter + 1
        
        prioritisation <- rbind(prioritisation,
                                data.frame(Site=as.character(nodeflow$Category[to_remove]),
                                           Pop_Flow =   flow$value,
                                           Site_Flow =   nodeflow$x[to_remove],
                                           Iteration = iter))
        
        net_remove = which(colnames(network) %in% nodeflow$Category[to_remove])
        #dim(network[-net_remove,-net_remove ])
        network = network[-net_remove,-net_remove ]
        
      }
      
      y = prioritisation$Pop_Flow/prioritisation$Pop_Flow[1]
      x = (1:length(prioritisation$Pop_Flow))/length(prioritisation$Pop_Flow)
      AUC = curvCALC(x,y)
      
      prioritisation$AUC = AUC
      
      # if(toplot == TRUE){
      #   par(mar=c(4,4,1,1))
      #   plot(prioritisation$Pop_Flow, type="o",pch=16,
      #        xlab="# Sites Removed",
      #        ylab="Population size",
      #        main = AUC)
      #
      #   par(mar=c(4,4,1,1))
      #   plot(prioritisation$Site_Flow, c(0,abs(diff(prioritisation$Pop_Flow))), pch=16,
      #        col = colorRampPalette(c("royalblue", "orange"))(nrow(full_site_list)-1)[1:(nrow(full_site_list)-1)],
      #        xlab="Site carrying capacity",
      #        ylab="Change in population size when site lost")
      # }
      end_time <- Sys.time()
      print("                                   ")
      print(paste0("AUC: ",AUC))
      print(end_time - start_time)
      
      pts$AUC = AUC
      
      write.csv(pts, file.path(filepath, costype[costi],loc[loci] ,"summary",
                               paste0("connectivity_",costype[costi],"_",loc[loci],"_",run[runi],"_run_", repetition,".txt")))
                               # paste0("connectivity_",loc[loci],"_",run[runi],"_pop_5km_",dispersal_dist,"_run_", repetition,".txt")))
      
      write.csv(prioritisation,
                file.path(filepath, costype[costi], loc[loci] ,"summary",
                          paste0("migflow_",costype[costi],"_",loc[loci],"_",run[runi],"_run_", repetition,".txt")))
                          # paste0("migflow_",loc[loci],"_",run[runi],"_pop_5km_",dispersal_dist,"_run_", repetition,".txt")))
    }
  }
}



