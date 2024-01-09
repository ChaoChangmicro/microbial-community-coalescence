library(phyloseq)
library(tidyverse)
metadata = read.delim("./metadata.txt",row.names = 1)
metadata$ID = row.names(metadata)

#After the data was rarefied
otutab = read.delim("./bacteria_rare.txt", row.names=1)
head(otutab)

taxonomy = read.table("./taxonomy.txt", row.names=1,header = T)
head(taxonomy)

library(Biostrings)
rep = readDNAStringSet("./dna-sequences.fasta")
seq <- rep[names(rep) %in% row.names(otutab)]
head(seq)
ps = phyloseq(
  sample_data(metadata),
  otu_table(as.matrix(otutab), taxa_are_rows=TRUE),
  tax_table(as.matrix(taxonomy)),
  refseq(rep)
)


#Change the ASVs name
taxa_names(ps) <- 1:ntaxa(ps)
taxa_names(ps) <- paste("ASV", taxa_names(ps), sep="")
taxa_names(ps)

a <- as.data.frame(otu_table(ps))
write.csv(a ,file = "asv_rarefied.csv")

b <- as.data.frame(tax_table(ps))
write.csv(b ,file = "tax_rarefied.csv")

c <- refseq(ps)
writeXStringSet(c , "./asv.fasta", format="fasta")



######constructing phylogenetic trees
# Starting files are otus.fa (sequence), annotation.txt (species and relative abundance) files in result/tree directory
# Muscle software for sequence alignment, 3s
#cd to research directory
/mnt/c/bin/muscle.exe -in asv.fasta -out asv_aligned.fasta

### Method 1. Rapid construction of ML phylogenetic tree using IQ-TREE, 2m
# rm -rf iqtree
# mkdir -p iqtree
# iqtree -s otus_aligned.fas \
# -bb 1000 -redo -alrt 1000 -nt AUTO \
# -pre iqtree/otus

## Method 2. FastTree (Linux)
## Note that the input file to the FastTree software is a file in fasta format, not the commonly used Phylip format. The output file is in Newick format.
# This method is suitable for large data, such as phylogenetic trees of several hundred ASV!
# To install fasttree on Ubuntu you can use `apt install fasttree`.
fasttree -gtr -nt asv_aligned.fasta > asv.nwk

#### Crop the original tree to the desired tree ####
# library(picante)
# tree <- read.tree("rooted_tree.nwk")
# otu <- read.table("asv_rare.txt",sep = "\t",row.names = 1,header = T)
# otu <- t(otu)
# #裁剪时otu的名字必须为列名
# phy.tree <- prune.sample(otu,tree)
# plot(phy.tree)
# write.tree(phy.tree,file = "pruned_tree.nwk")


# library(seqinr)
# all_fasta <- read.fasta("dna-sequences.fasta")

# sub_fasta <- all_fasta[names(all_fasta) %in% colnames(otu)]

# write.fasta(sequences = sub_fasta, names =names(sub_fasta), file.out = 'target.fasta')

# Build phyloseq objects  ------------------------------------------------------------
library(phyloseq)
library(tidyverse)
metadata = read.delim("./metadata.txt",row.names = 1)
metadata$ID = row.names(metadata)


otutab = read.delim("./asv_rarefied.txt", row.names=1)
head(otutab)

taxonomy = read.table("./tax_rarefied.txt", row.names=1,header = T)
head(taxonomy)

tree  = read_tree("./asv.nwk")


library(Biostrings)
rep = readDNAStringSet("./asv.fasta")

ps = phyloseq(
  sample_data(metadata),
  otu_table(as.matrix(otutab), taxa_are_rows=TRUE),
  tax_table(as.matrix(taxonomy)), 
  phy_tree(tree),
  refseq(rep)
)

saveRDS(ps,"ps.rds")

