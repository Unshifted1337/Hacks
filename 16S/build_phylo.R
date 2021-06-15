require("dada2"); packageVersion("dada2") # 1.6.0

# Pipeline for seqtab_stringent
seqtab <- readRDS("./16S_seqtab.rds")
seqtab.t = as.data.frame(t(seqtab))
seqs = row.names(seqtab.t)
row.names(seqtab.t) = paste0("SV", 1:nrow(seqtab.t))
seqs = as.list(seqs)
seqinr::write.fasta(seqs, row.names(seqtab.t), "16S_seqtab.fasta")


# Remove sequences assigned to Archaea and Eukaryota
tax <- readRDS("./16S_silva128_dada2_tax.rds")
rownames(tax) <- paste0("SV", 1:nrow(tax))
eukarya <- tax[tax[,1]=="Eukaryota",]
eukarya <- rownames(eukarya)[!is.na(rownames(eukarya))]
archaea <- tax[tax[,1]=="Archaea",]
archaea <- rownames(archaea)[!is.na(rownames(archaea))]

# Removing Mitochontria and Chloroplast sequences
mitos <- tax[tax[,5]=="Mitochondria",]
mitos <- rownames(mitos)[!is.na(rownames(mitos))]
chloros <- tax[tax[,3]=="Chloroplast",]
chloros <- rownames(chloros)[!is.na(rownames(chloros))]

# Removing ASVs assigned to Eukarya and Archaea
seqtab <- readRDS("./16S_seqtab.rds")
seqtab.t = as.data.frame(t(seqtab))
seqs = row.names(seqtab.t)
row.names(seqtab.t) = paste0("SV", 1:nrow(seqtab.t))
to_remove <- rownames(seqtab.t)%in%c(eukarya,archaea,mitos,chloros)
seqtab.t <- seqtab.t[!to_remove,]
seqs <- seqs[!to_remove]
seqs = as.list(seqs)
seqinr::write.fasta(seqs, row.names(seqtab.t), "16S_seqtab_bacteria.fasta")

# Align to SILVA
system("align_seqs.py -i 16S_seqtab_bacteria.fasta -t /data/kruger/silva/128_qiime/SILVA_128_QIIME_release/core_alignment/core_alignment_SILVA128.fna -o pynast_aligned/")

# Some sequences had no search result in pynast
# These lines were ommited from the aligned fasta file

# pynast adds to the sequence names " 1..404" or similar, indicating length
# strip the whitespace and trailing characters with the following (to match seqtab and so as to work with RAxML)
system("cat pynast_aligned/16S_seqtab_bacteria_aligned.fasta | sed 's/[ \t].*//' > pynast_aligned/16S_seqtab_bacteria_aligned_clean.fasta")

# Gap and entropy filtering
system("filter_alignment.py -i pynast_aligned/16S_seqtab_bacteria_aligned_clean.fasta -o pynast_aligned/ -e 0.05 -g 0.9")

# Build GTRCAT phylogeny with FastTree
system("FastTree -nt -gtr  < pynast_aligned/16S_seqtab_bacteria_aligned_clean_pfiltered.fasta > FastTree_GTRCAT_16S_bacteria_filtered_005_09.tre")

# Plot tree as visual check
require(ape);packageVersion("ape") # 5.0
tree <- read.tree("FastTree_GTRCAT_16S_bacteria_filtered_005_09.tre")
tree_ladder <- ladderize(tree, right = TRUE)
plot(tree_ladder, show.tip.label=TRUE)
