install.packages("BioGeoBEARS")
# Load the package (after installation, see above).
library(optimx)   # optimx seems better than R's default optim()
library(GenSA)    # GenSA seems better than optimx (but slower) on 5+ parameters, 
# seems to sometimes fail on simple problems (2-3 parameters)
library(FD)       # for FD::maxent() (make sure this is up-to-date)
library(snow)     # (if you want to use multicore functionality; some systems/R versions prefer library(parallel), try either)
library(parallel)
library(rexpokit)
library(cladoRcpp)
library(BioGeoBEARS)
#Getting extada directory with BioGeoBEARS scripts
#extdata_dir = np(system.file("extdata", package="BioGeoBEARS"))
#extdata_dir
#list.files(extdata_dir)

library(ape)
library(phytools)
library(ggtree)
library(ggplot2)
library(ggnewscale)
library(dplyr)
library("ape")
library(RColorBrewer)
library(dplyr)
library('ggplot2')
library('ggtree')
library(tidytree)
library(ggnewscale)


## Lire un arbre
read.nexus("Desktop/Stage_M2/Données_R/Arbre_final.tree")
tree <- read.nexus("Desktop/Stage_M2/Données_R/Arbre_final.tree")
## Afficher le numero du nœud d’un ancetre commun 
node_orecto <- getMRCA(phy = tree,tip = c("Parascyllium_collare","Orectolobus_floridus"))
## Extraire une partie d’un arbre a partir d’un ancetre commun
tree_orecto <- extract.clade(phy = tree,node = (node_orecto))
## Dessiner un arbre
plot(tree_orecto)
write.tree(tree_orecto, file = "Desktop/Stage_M2/Données_R/tree_orecto.tree")

tree_orecto$tip.label

tableau <- read.table("Desktop/Stage_M2/Données_R/Distribution_BioGeoBEARS_2.txt",sep="\t",header = TRUE)


## Remplacer l'espace dans espèce par un _
tableau$Species<- gsub(" ","_", tableau$Species)
## Afficher les nom d'especes présente dans le tableau mais pas sur l'arbre
diff <- setdiff(tableau$Species,tree_orecto$tip.label)
## Conserver que les nom d'espèces en commun
tableau_cleaned<-tableau[!tableau$Species%in%diff,]
tableau_cleaned <- as.data.frame(tableau_cleaned)
str(tableau_cleaned)
write.table(tableau_cleaned, file = "Desktop/Stage_M2/Données_R/tableau_cleaned.txt", sep = "\t",row.names = FALSE)

#######################################################
#Phylogeny file
trfn <- "Desktop/Stage_M2/Données_R/tree_orecto.tree"
tr <- read.tree(trfn)
tr$root.edge=NULL
ladderize(tr, right = FALSE)

#######################################################
# Geography file
geogfn <- "Desktop/Stage_M2/Données_R/tableau_cleaned3.txt"

# Loading geographical range data
tipranges = getranges_from_LagrangePHYLIP(lgdata_fn=geogfn)

# Set the maximum number of areas any species may occupy.
max_range_size = 7

#######################################################
# Run DEC
#######################################################

# Intitialize a default model (DEC model)
BioGeoBEARS_run_object = define_BioGeoBEARS_run()

# Give BioGeoBEARS the location of the phylogeny Newick file
BioGeoBEARS_run_object$trfn = trfn

# Give BioGeoBEARS the location of the geography text file
BioGeoBEARS_run_object$geogfn = geogfn

# Input the maximum range size
BioGeoBEARS_run_object$max_range_size = max_range_size

#### Manual modification of state list ####

# Get your states list (assuming, say, 4-area analysis, with max. rangesize=4)
areas = getareas_from_tipranges_object(tipranges)
#areas = c("A", "B", "C", "D")

# This is the list of states/ranges, where each state/range
# is a list of areas, counting from 0
states_list_0based = rcpp_areas_list_to_states_list(areas=areas, maxareas=max_range_size, include_null_range=TRUE)

# How many states/ranges, by default: 163
length(states_list_0based)

# Make the list of ranges
ranges_list = NULL
for (i in 1:length(states_list_0based))
{    
  if ( (length(states_list_0based[[i]]) == 1) && (is.na(states_list_0based[[i]])) )
  {
    tmprange = "_"
  } else {
    tmprange = paste(areas[states_list_0based[[i]]+1], collapse="")
  }
  ranges_list = c(ranges_list, tmprange)
}

# Look at the ranges list
ranges_list

# How many states/ranges, by default: 163
#length(ranges_list)

# Let's remove some non-adjacent ranges
#nonadjacent=c("AC","AD","AF","BF","BG","CG","DE","DG","ABF","ACD","ACF","ACG","ADG","ADE","ADF","BCG","BDG","BDF","BFG","CDG","DEG")
#nonadjacent=c("AC","AD","BE","BF","CF","DF","ACD","ACF","ADF","BCF","BDF","BEF","CDF")
#keepTF = ranges_list %in% nonadjacent == FALSE

#ranges_list_NEW = ranges_list[keepTF]
#length(ranges_list_NEW)     # now 148

#states_list_0based_NEW = states_list_0based[keepTF]
#length(states_list_0based_NEW)     # now 148

# INPUT the NEW states list into the BioGeoBEARS_run_object
#BioGeoBEARS_run_object$states_list = states_list_0based_NEW
BioGeoBEARS_run_object$states_list = states_list_0based
#BioGeoBEARS_run_object$states_list=states_list_0based

BioGeoBEARS_run_object$min_branchlength = 0.000001    # Min to treat tip as a direct ancestor (no speciation event)
BioGeoBEARS_run_object$include_null_range = TRUE    # set to FALSE for e.g. DEC* model, DEC*+J, etc.
# (For DEC* and other "*" models, please cite: Massana, Kathryn A.; Beaulieu, 
#  Jeremy M.; Matzke, Nicholas J.; O’Meara, Brian C. (2015). Non-null Effects of 
#  the Null Range in Biogeographic Models: Exploring Parameter Estimation in the 
#  DEC Model. bioRxiv,  http://biorxiv.org/content/early/2015/09/16/026914 )
# Also: search script on "include_null_range" for other places to change

# Set up a time-stratified analysis using area adjacency matrix
#Time bin file
##BioGeoBEARS_run_object$timesfn = c(0, 247.55120453)

#Area adjacency matrix
BioGeoBEARS_run_object$areas_adjacency_fn <- read.table("Desktop/Stage_M2/Données_R/Matrice_aires_adjacentes.txt",sep="\t",header = TRUE)
#dispersal rates
#BioGeoBEARS_run_object$dispersal_multipliers_fn = "./new_model/Parnassiinae_connectivity.txt"
#Allowing for unknown characters for fossil tips
BioGeoBEARS_run_object$useAmbiguities = TRUE

# Speed options and multicore processing if desired
BioGeoBEARS_run_object$on_NaN_error = -1e50    # returns very low lnL if parameters produce NaN error (underflow check)
BioGeoBEARS_run_object$speedup = FALSE          # shorcuts to speed ML search; use FALSE if worried (e.g. >3 params)
BioGeoBEARS_run_object$use_optimx = TRUE    # if FALSE, use optim() instead of optimx();

# if "GenSA", use Generalized Simulated Annealing, which seems better on high-dimensional
# problems (5+ parameters), but seems to sometimes fail to optimize on simple problems
BioGeoBEARS_run_object$num_cores_to_use = 2
BioGeoBEARS_run_object$force_sparse = FALSE    # force_sparse=TRUE causes pathology & isn't much faster at this scale

# This function loads the dispersal multiplier matrix etc. from the text files into the model object. Required for these to work!
# (It also runs some checks on these inputs for certain errors.)
BioGeoBEARS_run_object = readfiles_BioGeoBEARS_run(BioGeoBEARS_run_object)

# Divide the tree up by timeperiods/strata 
##BioGeoBEARS_run_object =  section_the_tree(inputs=BioGeoBEARS_run_object,make_master_table=TRUE, plot_pieces=FALSE,cut_fossils=FALSE) 
# The stratified tree is described in this table:
##BioGeoBEARS_run_object$master_table

# Good default settings to get ancestral states
BioGeoBEARS_run_object$return_condlikes_table = TRUE
BioGeoBEARS_run_object$calc_TTL_loglike_from_condlikes_table = TRUE
BioGeoBEARS_run_object$calc_ancprobs = TRUE    # get ancestral states from optim run


# Look at the BioGeoBEARS_run_object; it's just a list of settings etc.
BioGeoBEARS_run_object

# This contains the model object
BioGeoBEARS_run_object$BioGeoBEARS_model_object

# This table contains the parameters of the model 
BioGeoBEARS_run_object$BioGeoBEARS_model_object@params_table

# Run this to check inputs. Read the error messages if you get them!
check_BioGeoBEARS_run(BioGeoBEARS_run_object)

resfn = "Orecto_DEC_2018_model_2018.Rdata"
res = bears_optim_run(BioGeoBEARS_run_object)
res
res$total_loglikelihood
save(res, file=resfn)
resDEC = res
# For a slow analysis, run once, then set runslow=FALSE to just 
# load the saved result.


#######################################################
# PDF plots
#######################################################
pdffn = "Orecto_DEC_2018_model_2018.pdf"
pdf(pdffn)
# plot(resfn)
dev.off()

#######################################################
# Plot ancestral states - DEC
#######################################################
analysis_titletxt ="Desktop/Stage_M2/Données_R/Orecto_DEC_2018_model_2018.Rdata"

# Setup
results_object = resDEC
scriptdir = np(system.file("extdata/a_scripts", package="BioGeoBEARS"))

# States
res2 = plot_BioGeoBEARS_results(results_object, analysis_titletxt,  plotwhat="text", label.offset=0.45, tipcex=0.7, statecex=0.7, splitcex=0.6, titlecex=0.8, plotsplits=TRUE, cornercoords_loc=scriptdir, include_null_range=TRUE, tr=tr, tipranges=tipranges)


# Pie chart

plot_BioGeoBEARS_results(results_object, analysis_titletxt, addl_params=list("j"), plotwhat="pie", label.offset=0.45, tipcex=0.7, statecex=0.7, splitcex=0.6, titlecex=0.8, plotsplits=FALSE, cornercoords_loc=scriptdir, include_null_range=TRUE, tr=tr, tipranges=tipranges)


dev.off()  # Turn off PDF

cmdstr = paste("open ", pdffn, sep="")
system(cmdstr) # Plot it

