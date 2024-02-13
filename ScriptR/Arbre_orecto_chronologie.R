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
library(strap)
library(ggtree)

## Lire un arbre
read.nexus("Desktop/Stage_M2/Données_R/Arbre_final.tree")
tree <- read.nexus("Desktop/Stage_M2/Données_R/Arbre_final.tree")
## Afficher le numero du nœud d’un ancetre commun 
node_orecto <- getMRCA(phy = tree,tip = c("Parascyllium_collare","Orectolobus_floridus"))
## Extraire une partie d’un arbre a partir d’un ancetre commun
tree_orecto <- extract.clade(phy = tree,node = (node_orecto))
ggtree(tree_orecto) + geom_tiplab()+ theme_tree2()
## Ajouter age de la racine
tree_orecto$root.time = 247.5512

## Ajouter frise chronologique
pdf("arbre_orecto_chronologie.pdf", width = 12, height = 8)
geoscalePhylo(tree=ladderize(tree_orecto,right=FALSE), units=c("Period", "Epoch", "Age"), boxes="Epoch", cex.tip=0.5, cex.age=0.7, cex.ts=0.7, label.offset=2, x.lim=c(-15,240), lwd=3, width=2)
dev.off()

