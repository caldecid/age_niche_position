# Age-related Niche Position and Specialization in Neotropical Vertebrates

This repository contains **R scripts, functions, and data** used to replicate the analyses from the manuscript *“Age-Dependent niche Position and specialization in Neotropical vertebrates.”*

> ⚠️ Some scripts are intended to be run on a **server with multiple cores** due to memory limitations. These are marked with `#server`.

---

## Repository Structure

The repository is organized into **four methodological sections**, each with its own folder:

### 1. Spatial Data and Manipulation  
**Folder:** `results/scripts/ENFA`  
This section includes functions and scripts for processing spatial data and performing ecological niche factor analysis (ENFA):

- `dsm.enfa.function.R`: Processes biome, climate, and species distribution data.  
- `ENFA.function.R`: Runs ENFA using the outputs from the previous function.  
- `weight.enfa.function.R`: Executes Weighted ENFA based on the prior results.  
- `execution_enfa.R`: Wrapper script to run the above functions sequentially.

---

### 2. Phylogenetic Data and Manipulation  
**Folder:** `results/scripts/phylo_analyses/phylo_manipulation`  
Scripts to prepare phylogenetic data for comparative analyses:

- `multiphylo_data_enfa.R`: Assembles 100 random trees for each tetrapod group.  
- `comparative_data.R`: Generates 100 `comparative.data` objects (using the `caper` package) per tetrapod group.

---

### 3. Phylogenetic and Statistical Analyses  
**Folder:** `results/scripts/phylo_analyses/pgls`  
R scripts to assess phylogenetic signal, test hypotheses, and generate plots:

- `phylo_signal.R`: Measures the phylogenetic signal of ENFA metrics across groups. `#server`  
- `anova_kruskal.R`: Performs Kruskal-Wallis tests on ENFA measures across tetrapod classes.  
- `weighted_pgls.R`: Runs PGLS models testing the effect of species age on weighted ENFA measures. `#server`  
- `plots_weighted.R`: Plots the results of `weighted_pgls.R`.  
- `biomes_pgls.R`: Performs PGLS models per biome to test the effect of species age. `#server`  
- `plots_biomes.R`: Plots the results of `biomes_pgls.R`.  
- `final_tables.R`: Summarizes and formats the results from `weighted_pgls.R` and `biomes_pgls.R`.

---

### 4. Sensitivity analyses  
**Folder:** `results/scripts/sensitivity_analyses`  
R scripts to perform the sensitivity analyses:

(i) Spatial scale: `results/scripts/sensitivity_analyses/spatial_scale`
- `whole_neotropic_analyses.R`: Runs Neotropical ENFA (without biome stratification) for the 4 tetrapod groups (VIF < 10). `#server`  
- `correlation_neotropica_10th_3th.R`: Performs correlation analyses among weighted ENFA, Neotropical ENFA (VIF < 10), and Neotropical ENFA (VIF < 3).  
- `whole_neotropical_plots_results.R`: Correlation assessment plots. (Most are not included in the MS).  

(ii) Biovariable selection: `results/scripts/sensitivity_analyses/biovariable_selection`
- `sensitivity_biome_climate_3th.R`: Climatic data manipulation for obtaining Biomes environmental conditions (VIF < 3)  
- `neotropic_ENFA_3th.R`: Runs Neotropical ENFA (without biome stratification) for the 4 tetrapod groups (VIF < 3). 
- `biome_ENFA_3th.R`: Runs biome-ENFA for the 4 tetrapod groups (VIF < 3). `#server`
- `correlation_biome_3th.R`: Performs correlation analyses among biome-ENFA metrics (VIF < 10) and biome-ENFA metrics (VIF < 3)
- `plots_biomes_3th.R`: Plots the results of pgls models form ENFA metrics with a VIF < 3.  


## Auxiliary Functions

Additional helper functions for:

- Correcting species age estimates  
- Phylogenetic tree manipulation  
- Estimating diversification rates  

can be found in:  
**`results/scripts/functions`**

---

## Contact

For questions or contributions, feel free to contact:

**Carlos Calderón del Cid**  
**caldecid@gmail.com**  
