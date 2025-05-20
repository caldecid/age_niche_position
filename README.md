#### Repository of "Age-Dependent niche position and specializaiton in Neotropical vertebrates"


R scripts, functions and data to replicate the analysis in the manuscript. Note that, due to memory limitations, some Rscripts are intended to be run on a server with multiple cores, we signal them #server.  

The manuscripts possesses three metodological sections associated to three folders:

i) Spatial data and manipulation ----- correspond to the "results/scripts/ENFA" file:
    i.1) "dsm.enfa.function.R" ---- Function to process Biomes, climate and species' distribution.
    i.2) "ENFA.function.R" -------  Function to run ENFA with the outcomes of the previous script.
    i.3) "weight.enfa.function.R" ------ Funciton to execute the Weighted ENFA with the outcome of the previous scripts.
    i.4) "execution_enfa.R" ---------- Rscript to run the functions described previously. 

 ii) Phylogenetic data and manipulation ---------- correspond to the "results/scripts/phylo_analyses/phylo_manipulation" file:
     ii.1) "multiphylo_data_enfa.R"-------------- ensamble 100 random trees for each tetrapod group.
     ii.2) "comparative_data.R" ---------------- creating 100 comparative.data (caper package) for the 100 trees from each tetrapod group.
     
iii) Phylogenetic and statistical analyses ----------- correspond to the "results/scripts/phylo_analyses/pgls" file:
     iii.1) "phylo_signal.R" --------------------- Rscript for measuring phylogenetic signal of ENFA measures across groups. (# server)
     iii.2) "anova_kruskal.R" --------------- Rscript for performing Kruskal-Wallis tests to evaluate differences in ENFA measures across tetrapod classes.
     iii.3) "weighted_pgls.R" -------------- Rscript for performing phylogenetic generalized least squares (PGLS) models to evaluate the effect of species ages on the weighted ENFA measures of the four tetrapod classes (#server)
     iii.4) "plots_weighted.R" ------------- Rscript for plotting the results of the "weighted_pgls.R" Rscript. 
     iii.5) "biomes_pgls.R" ----------------- Rscript for performing PGLS models to evaluate the effect of species ages on the ENFA measures of the four tetrapod classes for each Neotropical biome (#server)
     iii.6) "plots_biomes.R" --------------- Rscript for plotting the results of the "biomes_pgls.R" Rscript.
     iii.7) "final_tables.R" -------------- Rscript for summarizing the table results from "weighted_pgls.R" and "biomes_pgls.R". 

The auxiliary functions for correcting species ages, perform some phylogenetic manipulations, and estimating diversification rates are inside the "results/scripts/functions" file. 
