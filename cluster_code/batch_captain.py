# get the arguments from the slurm input
import sys

repetition = sys.argv[1]  # cluster input
print("-------------------------------------------------------")
print("This is iteration " + str(repetition))
print("-------------------------------------------------------")

# import libraries
import os

github_dir = "/mnt/shared/scratch/kdhanjal/captain-project/"
os.chdir(github_dir)
sys.path.append(github_dir)
import captain as cn
import glob
import numpy as np
import pandas as pd

# get the trained model
trained_model_file = '/mnt/shared/scratch/kdhanjal/captain-project/trained_models/full_monitor_protect_at_once_d4_n8-0r.log'

for costype in ["hf_cost", "no_cost"]:

    if costype == "hf_cost":
        # parameters for simulation
        total_budget = 100940873 * 0.3  # 48139 # # 30% of population
        total_pop = 100940873  # 1274821.413097077
        total_pus = 24465
        total_spp = 1517
    else:
        total_budget = 24465 * 0.3  # 48139 # # 30% of area
        total_pop = 100940873  # 1274821.413097077
        total_pus = 24465
        total_spp = 1517

    occs2 = np.loadtxt('/mnt/shared/scratch/kdhanjal/madagascar/' + costype + '/0_level/puvsp_ID_1.csv', skiprows=1,
                       delimiter=",")
    total_spp_occ = len(occs2[:, 0].astype(int))

    for region_i in ["0_level", "1st_level", "2nd_level", "3rd_level", "4th_level"]:

        directory1 = '/mnt/shared/scratch/kdhanjal/madagascar/' + costype + '/' + region_i + '/results/'
        directory2 = '/mnt/shared/scratch/kdhanjal/madagascar/' + costype + '/' + region_i + '/summary/'
        if not os.path.exists(directory1):
            os.makedirs(directory1)
        if not os.path.exists(directory2):
            os.makedirs(directory2)

        for simtype in ["pop_cost", "biodiversity_cost", "equal_cost", "area_cost"]:

            all_results = pd.DataFrame()

            species_table = pd.DataFrame()
            species_table["Species_ID"] = np.unique(occs2[:, 0]).astype(int)
            species_table["Protect_Ind_per_spp"] = 0
            species_table["Ind_per_spp"] = 0
            species_table["geoRangePerSpecies"] = 0

            # specify where the data are
            data_dir = "/mnt/shared/scratch/kdhanjal/madagascar/" + costype + "/" + region_i
            files = glob.glob(os.path.join(data_dir, 'puvsp_ID_*.csv'))

            for i in range(len(files)):

                id = i + 1

                # define the run so that we can keep track of different simulations
                run = costype + "_" + region_i + "_" + simtype + "_ID_" + str(i + 1) + "_run_" + str(repetition)
                print("******** run " + run + " *************")

                # the input files
                puvsp_file = 'puvsp_ID_' + str(i + 1) + '.csv'
                pu_file = 'pu_ID_' + str(i + 1) + '.csv'
                pu_info_file = 'Planning_units_ID_' + str(i + 1) + '.csv'

                # the generated empirical env files
                hist_out_file = 'hist_' + run + '.npy'
                pu_id_out_file = 'pu_id_' + run + '.npy'
                sp_id_out_file = 'sp_id_' + run + '.npy'

                # unique output file name to save
                output_file = 'output_' + run

                # define some values to generate species sensitivities (so that runs can be comparable with same species sensitvities reused
                occs = np.loadtxt(os.path.join(data_dir, puvsp_file), skiprows=1, delimiter=",")
                if occs.shape == (0,):
                    continue
                if len(occs.shape) == 1:
                    continue
                n_spp_occ = len(occs[:, 0].astype(int))
                _species_id = np.unique(occs[:, 0]).astype(int)
                _n_species = len(_species_id)
                if _n_species == 1:
                    continue
                if len(np.unique(occs[:, 1])) == 1:
                    continue

                species_sensitivities = np.random.random(_n_species)

                # pu_region = np.loadtxt(os.path.join(data_dir,pu_file), skiprows=1, delimiter=",")
                pu_region = np.genfromtxt(os.path.join(data_dir, pu_file), skip_header=1, delimiter=",",
                                          filling_values=500000.00)

                budget = total_budget
                if (simtype == "area_cost"):
                    if len(pu_region.shape) == 1:
                        budget = total_budget * 1 / total_pus
                    else:
                        budget = total_budget * (pu_region.shape[0] / total_pus)
                if (simtype == "pop_cost"):
                    if len(pu_region.shape) == 1:
                        budget = total_budget * (pu_region[1] / total_pop)
                    else:
                        budget = total_budget * (pu_region[:, 1].sum() / total_pop)
                if (simtype == "equal_cost"):
                    if len(pu_region.shape) == 1:
                        budget = total_budget
                    else:
                        budget = total_budget / len(files)
                if (simtype == "biodiversity_cost"):
                    if len(pu_region.shape) == 1:
                        budget = total_budget * (n_spp_occ / total_spp_occ)  # (_n_species / total_spp / len(files))
                    else:
                        budget = total_budget * (n_spp_occ / total_spp_occ)  # (_n_species / total_spp / len(files))

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

                try:
                    env2, out_file = cn.run_policy_empirical(
                        env1,
                        trained_model=trained_model_file,
                        obsMode=1,  # full species monitoring
                        observePolicy=2,  # recurrent monitoring, at-once protection
                        n_nodes=[8, 0],
                        budget=budget,
                        relative_budget=False,
                        protection_target=0.3,
                        stop_at_end_budget=True,
                        stop_at_target_met=False,
                        seed=int(str(repetition)),
                        update_features=10,
                        wd=data_dir,
                        result_file="results/" + output_file
                    )

                except ValueError as e:
                    if str(e) == 'zero-size array to reduction operation minimum which has no identity':
                        print("Not enough species in cells - go locally extinct during environment building")
                        continue  # skip this iteration
                    else:
                        raise  # re-raise any other unexpected ValueError

                print("")
                print("")
                print("******** prioritisation done *************")
                print("")
                print("")
                print("------------------------------------------------------------------------------")

                pkl_files = [os.path.join(data_dir, "results/" + output_file + '_' + str(
                    repetition) + '.pkl')]  ### "results/" + output_file +'.pkl')]  ###

                # load all environments so that they can later be stitched together
                output_envs = [cn.load_pickle_file(f) for f in pkl_files]

                # region = output_envs[0]
                for region in output_envs:
                    results_regional = pd.DataFrame(region.bioDivGrid.coords)
                    for pu_i in range(results_regional.shape[0]):
                        indx = np.where(region.bioDivGrid._pus_id == results_regional["PUID"][pu_i])[0]
                        if len(indx):  # if cell in data
                            results_regional.loc[pu_i, 'Optim'] = region.bioDivGrid.protection_matrix[indx][0][
                                0]  # results_regional.index == i
                            results_regional.loc[pu_i, "Cost"] = region.protection_cost[indx][0]
                            results_regional.loc[pu_i, "Disturbance"] = region.bioDivGrid.disturbance_matrix[indx][
                                0]  # [0]
                            results_regional.loc[pu_i, "Species_per_PU"] = region.bioDivGrid.speciesPerCell()[indx][0][
                                0]

                    results_regional["Budget"] = region._initialBudget
                    results_regional["Remaining_Budget"] = region.budget
                    results_regional["Species_number_below_target"] = len(region.protect_fraction) - len(
                        region.get_species_met_target())
                    results_regional["Proportion_below_target"] = (len(region.protect_fraction) - len(
                        region.get_species_met_target())) / len(region.protect_fraction)
                    results_regional["Region"] = id

                    # add the species to the table
                    spp_i = region.bioDivGrid._species_id
                    species_table.loc[species_table["Species_ID"].isin(
                        spp_i), "Protect_Ind_per_spp"] += region.bioDivGrid.protectedIndPerSpecies()
                    species_table.loc[species_table["Species_ID"].isin(
                        spp_i), "Ind_per_spp"] += region.bioDivGrid.individualsPerSpecies()
                    species_table.loc[species_table["Species_ID"].isin(
                        spp_i), "geoRangePerSpecies"] += region.bioDivGrid.geoRangePerSpecies()

                    id += 1

                    all_results = pd.concat([all_results, results_regional], axis=0, ignore_index=True)

                    #### PLot each region too
                    target_fraction = 0.1  # region.protect_fraction
                    bins = np.linspace(0, 1, 51)  # range(11) #[0,1, 2, 5, 10]
                    col = ["#ef8a62", "#67a9cf"]  # ["#bd0026", "#bdc9e1"]
                    colors = [col[0]] * len(bins[bins < target_fraction]) + [col[1]] * len(
                        bins[bins > target_fraction]
                    )
                    edgecolor = colors  # "#252525"#
                    Ylim = 55563
                    protected_fraction = (
                            region.bioDivGrid.protectedIndPerSpecies() / region.bioDivGrid.individualsPerSpecies()
                    )

            # give the data columns a name
            all_results.columns = ['FID', 'PUID', 'Coord_x', 'Coord_y', 'Optim', "Cost", "Disturbance",
                                   "Species_per_PU", "Budget",
                                   "Remaining_Budget", "Species_number_below_target", "Proportion_below_target",
                                   "Region"]  # new columns added
            # # save as csv
            all_results.to_csv(os.path.join(data_dir,
                                            "summary/all_results_" + costype + "_" + region_i + "_" + simtype + "_run_" + str(
                                                repetition) + '.csv'), index=False)

            species_table["protected_fraction"] = species_table["Protect_Ind_per_spp"] / species_table["Ind_per_spp"]

            # save as csv
            species_table.to_csv(os.path.join(data_dir,
                                              "summary/species_protected_fraction_" + costype + "_" + region_i + "_" + simtype + "_run_" + str(
                                                  repetition) + ".csv"), index=False)






