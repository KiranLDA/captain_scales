# get the arguments from the slurm input
import sys
repetition = sys.argv[1] 
print("-------------------------------------------------------" )
print("This is iteration "+ str(repetition))
print("-------------------------------------------------------" )


#import libraries
import os
github_dir = "/mnt/shared/scratch/kdhanjal/captain-project/"
os.chdir(github_dir)
sys.path.append(github_dir)
import captain as cn 
import glob
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt


#parameters for simulation
total_budget = 48139
total_pop = 1274821.413097077
total_pus = 24465
total_spp = 1517

# get the trained model
trained_model_file = '/mnt/shared/scratch/kdhanjal/captain-project/trained_models/full_monitor_protect_at_once_d4_n8-0r.log'


for region_i in ["MDG_adm0", "MDG_adm1","MDG_adm2", "MDG_adm3", "MDG_adm4"]: # 
    
    for simtype in [ "equal_cost", "area_cost","pop_cost","biodiversity_cost"]:       

        
        # specify where the data are
        data_dir = "/mnt/shared/scratch/kdhanjal/madagascar/" + region_i
        files = glob.glob(os.path.join(data_dir,'puvsp_ID_*_pop_5km.csv'))
         
        for i in range(len(files)):
            
            id = i+1
            
            # define the run so that we can keep track of different simulations
            run = region_i + "_" + simtype + "_ID_" + str(i+1) + "_run_" + str(repetition)
            print("******** run "+ run +" *************")
        
            # the input files
            puvsp_file = 'puvsp_ID_' + str(i+1) + '_pop_5km.csv'
            pu_file = 'pu_ID_' + str(i+1) + '_pop_5km.csv'
            pu_info_file = 'Planning_units_ID_' + str(i+1) + '_pop_5km.csv'
            
            # the generated empirical env files
            hist_out_file = 'hist_'+run+'.npy'
            pu_id_out_file = 'pu_id_'+run+'.npy'
            sp_id_out_file = 'sp_id_'+run+'.npy'
        
            #unique output file name to save
            output_file = 'output_'+ run
            
            #define some values to generate species sensitivities (so that runs can be comparable with same species sensitvities reused
            occs = np.loadtxt(os.path.join(data_dir,puvsp_file), skiprows=1, delimiter=",")
            if occs.shape == (0,):
                continue   
            if len(occs.shape) == 1:
                continue
            _species_id = np.unique(occs[:, 0]).astype(int)
            _n_species = len(_species_id)
            if _n_species == 1:
                continue
            if len(np.unique(occs[:, 1])) == 1:
                continue
            
            species_sensitivities = np.random.random(_n_species)
            
            pu_region = np.loadtxt(os.path.join(data_dir,pu_file), skiprows=1, delimiter=",")
            budget = total_budget
            if(simtype == "area_cost"):
                if len(pu_region.shape) == 1:
                    budget = total_budget * 1 / total_pus
                else:
                    budget = total_budget * (pu_region.shape[0] / total_pus)
            if(simtype == "pop_cost"):
                if len(pu_region.shape) == 1:
                    budget = total_budget * (pu_region[1] / total_pop)
                else:
                    budget = total_budget * (pu_region[:,1].sum() / total_pop)
            if(simtype == "equal_cost"):
                 if len(pu_region.shape) == 1:
                     budget = total_budget
                 else:
                     budget = total_budget / len(files)
            if(simtype == "biodiversity_cost"):
                if len(pu_region.shape) == 1:
                    budget = total_budget * (_n_species / total_spp / len(files))
                else:
                    budget = total_budget * (_n_species / total_spp / len(files))
        
            # build env
            env1 = cn.build_empirical_env(wd=data_dir,
                                          puvsp_file=puvsp_file,
                                          pu_file=pu_file,
                                          pu_info_file=pu_info_file,
                                          hist_out_file="results/" + hist_out_file,
                                          pu_id_out_file="results/" + pu_id_out_file,
                                          sp_id_out_file="results/" + sp_id_out_file,
                                          budget=budget,
                                          rescale_cost=False)
            print("")
            print("")
            print("******** env built *************")
            
            env2, out_file = cn.run_policy_empirical(env1,
                                                    trained_model=trained_model_file,
                                                    obsMode=1, # full species monitoring
                                                    observePolicy=2, # recurrent monitoring, at-once protection
                                                    n_nodes=[8, 0],
                                                    budget=budget, #  for 10% of median #115595 for 10% of mean  #70000 looks nice
                                                    relative_budget=False,
                                                    #conservation_target=conservation_target,
                                                    protection_target=0.1,
                                                    stop_at_end_budget=True,
                                                    stop_at_target_met=False,
                                                    seed=int(str(repetition)),
                                                    update_features=10,
                                                    wd=data_dir,
                                                    result_file="results/" + output_file)
            print("")
            print("")
            print("******** prioritisation done *************")
            print("")
            print("")
            print("------------------------------------------------------------------------------")

