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
read.nexus("Desktop/Stage_M2/Données_R/369sp_16FC_15C.tree")
tree <- read.nexus("Desktop/Stage_M2/Données_R/369sp_16FC_15C.tree")
## Afficher le numero du nœud d’un ancetre commun 
node_orecto <- getMRCA(phy = tree,tip = c("Parascyllium_collare","Orectolobus_floridus"))
## Extraire une partie d’un arbre a partir d’un ancetre commun
tree_orecto <- extract.clade(phy = tree,node = (node_orecto))
## Dessiner un arbre
plot(tree_orecto)


tableau <- read.csv("Desktop/Stage_M2/Données_R/Distribution_orecto2.tsv",sep="\t",header = TRUE)

## Remplacer l'espace dans espèce par un _
tableau$Species<- gsub(" ","_", tableau$Species)
## Afficher les nom d'especes présente dans le tableau mais pas sur l'arbre
diff <- setdiff(tableau$Species,tree_orecto$tip.label)
## Conserver que les nom d'espèces en commun
tableau_cleaned<-tableau[!tableau$Species%in%diff,]

#Enlever la colonne Species
tableau_cleaned_v2<-tableau_cleaned[,-1]
# Transformer en dataframe
tableau_cleaned_v2 <- as.data.frame(tableau_cleaned_v2)
# Remplacer les points des noms de régions par des enderscores
colnames(tableau_cleaned_v2)<- gsub("\\.","_", colnames(tableau_cleaned_v2))
# Transformer les valeurs binaires en charachter
tableau_cleaned_v2<- as.data.frame(lapply(tableau_cleaned_v2, as.character), stringsAsFactors = FALSE)
# Remettre les nom d'espèces en nom de lignes
rownames(tableau_cleaned_v2)<-tableau_cleaned[,1]
str(tableau_cleaned_v2)
# Créer une colonne de "1" (pour point noir en bout de branches)
nouvelle_colonne <- rep("1", nrow(tableau_cleaned_v2))
tableau_cleaned_v2<- cbind(Rien = nouvelle_colonne, tableau_cleaned_v2)


# Créer un tableau de palette de couleurs
couleur <- data.frame(c("#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF"),c("#000000","#B4573D","#F17922","#DEB764","#F6D394","#482A5A","#9F1957","#E288B1","#cdccf1","#800000","#FBE870","#0279A8","#02B2D7","#B8DCE3","#618E3C","#B5CE88","#707173","#C63637","#FF6961","#FFBFB0"))
colnames(couleur)<-c("0","1")


## Fonction pour faire la figure 

gettreedata <- function(tree, meta){
  #get treedata object
  d<-meta[row.names(meta) %in% tree$tip.label,]
  d$label <- row.names(d)
  y <- full_join(as_tibble(tree), d, by='label')
  y <- as.treedata(y)
  return(y)
}

ggplottree <- function(tree, meta, cols=NULL, cmaps=NULL, layout="rectangular",
                       offset=20, tiplabel=FALSE, tipsize=3) {
  
  y <- gettreedata(tree, meta)
  p <- ggtree(y, layout=layout)
  p <- p + theme_tree(guide = FALSE)
  if (is.null(cols)){
    return (p)
  }
  
  col <- cols[1]
  cmap <- cmaps[1]
  df<-meta[tree$tip.label,][col]
  colors <- couleur[1,]
  
  #tip formatting    
  p1 <- p + new_scale_fill() +    
    geom_tippoint(mapping=aes(fill=.data[[col]]),size=tipsize,shape=21,stroke=0) +
    scale_fill_manual(values=colors, na.value="white")+
    geom_tiplab(size=2, align=TRUE, linesize=.5, hjust = -0.1)
  
  p2 <- p1
  if (length(cols)>1){
    for (i in 2:length(cols)){
      col <- cols[i]
      cmap <- cmaps[i]
      df <- meta[tree$tip.label,][col]
      type <- class(df[col,])            
      p2 <- p2 + new_scale_fill()
      p2 <- gheatmap(p2, df, offset=i*offset, width=.08,
                     colnames_angle=70, colnames_offset_y = 3,color  =  "black",colnames_position  =  "top",font.size  =  2)  
      #deal with continuous values
      if (type == 'numeric'){               
        p2 <- p2 + scale_color_brewer(type="div", palette=cmap)
      }
      else {
        colors <- couleur[i,]
        p2 <- p2 + scale_fill_manual(values=colors, name=col)
      }          
    }
  }
  
  p2 <- p2 + theme_tree2(legend.text = element_text(size=20), legend.key.size = unit(1, 'cm'),
                         legend.position="left", plot.title = element_text(size=40)) + theme(legend.position="none")    
  guides(color = guide_legend(override.aes = list(size=10)))
  
  return(p2)
}

# Réalisation de la figure

tree <- tree_orecto
options(repr.plot.width=7, repr.plot.height=5)
df <- tableau_cleaned_v2
ggplottree(tree, df, cols=c("Rien","Australie_côte_est","Australie_côte_nord","Australie_côte_ouest","Australie_côte_sud","Indonésie_Ouest","Indonésie_Centrale","Indonésie_Est","Mer_de_Chine","Inde","Iles_Pacifiques","Côte_ouest_Amérique_de_Nord","Côte_est_Amérique_de_Nord","Caraïbe","Côte_ouest_Amérique_du_Sud","Côte_est_Amérique_du_Sud","Côte_ouest_Europe","Côte_ouest_Afrique","Côte_est_Afrique","Péninsule_arabique"),
           cmaps=c('Greys','Purples','Greens','Reds','Greys','Oranges','Blues','Purples','Greens','Reds','Greys','Oranges','Blues','Purples','Greens','Reds','Greys','Oranges','Blues','Purples'), tipsize=3, offset=20 ,layout='rect')

####



dev.off()

pdf("tree_orecto.pdf")

