require("dada2"); packageVersion("dada2") # 1.6.0

path <- "/data/kruger/illumina/16S_fastq" # directory containing the fastq files

# Set random seed (for reproducibility)
set.seed(4534)

### Filter & Trim ###
# Forward and reverse fastq filenames have format: SAMPLENAME_R1_001.fastq and SAMPLENAME_R2_001.fastq
fnFs <- sort(list.files(path, pattern="_R1_001.pick.trim.fastq", full.names = TRUE))
fnRs <- sort(list.files(path, pattern="_R2_001.pick.trim.fastq", full.names = TRUE))

# Extract and format sample names
sample.names <- sapply(strsplit(basename(fnFs), "_"), `[`, 1)
kruger.sample.names <- gsub("Max[-]","", sample.names)
kruger.sample.names <- gsub("[-]","_", kruger.sample.names)

### Examine quality profiles of forward and reverse reads ###
# Sunquist et al 2007 v3-v4 primers amplify the v3 region (197bp)
# and the v4 region (288bp), totalling 485bp. 
# Forward primer is 20bp, reverse primer is 19bp.
# After removing primers, amplicon should be 446bp.
# With 20bp overlap we need about 466 total length (F + R)

plotQualityProfile(fnFs, aggregate=TRUE) 
plotQualityProfile(fnRs, aggregate=TRUE) 

### Perform filtering & trimming ###
# Assign the filenames for the filtered fastq.gz files.
filt_path <- file.path(path, "filtered") # Place filtered files in filtered/ subdirectory
filtFs <- file.path(filt_path, paste0(sample.names, "_F_filt_stringent.fastq.gz"))
filtRs <- file.path(filt_path, paste0(sample.names, "_R_filt_stringent.fastq.gz"))

# Filter the forward and reverse reads
out <- filterAndTrim(fnFs, filtFs, fnRs, filtRs, truncLen=c(230, 220),
              maxN=0, truncQ=6, rm.phix=TRUE, maxEE=c(6,6),
              compress=TRUE, multithread=TRUE) # On Windows set multithread=FALSE
out

### Infer Sequence Variants ###
# Infer sequence variants in each sample
# NOTE: This portion of the workflow should be performed on a run-by-run basis, as error rates can differ between runs.
# See - https://benjjneb.github.io/dada2/bigdata_paired.html

# Runs - Batch12, Batch15, Batch54, Batch7
# Get names for each batch and match to pick.trim.fastq files in 16S_fastq/
Batch12Fs <- sort(list.files("/data/kruger/illumina/Batch12", pattern="_R1_001.fastq", full.names = TRUE))
Batch15Fs <- sort(list.files("/data/kruger/illumina/Batch15", pattern="_R1_001.fastq", full.names = TRUE))
Batch54Fs <- sort(list.files("/data/kruger/illumina/Batch54", pattern="_R1_001.fastq", full.names = TRUE))
Batch7Fs <- sort(list.files("/data/kruger/illumina/Batch7", pattern="_R1_001.fastq", full.names = TRUE))

# Extract and format sample names
Batch12.sample.names <- sapply(strsplit(basename(Batch12Fs), "_"), `[`, 1)
Batch15.sample.names <- sapply(strsplit(basename(Batch15Fs), "_"), `[`, 1)
Batch54.sample.names <- sapply(strsplit(basename(Batch54Fs), "_"), `[`, 1)
Batch7.sample.names <- sapply(strsplit(basename(Batch7Fs), "_"), `[`, 1)

# Workflow for Big Data #
# Subsetting filtFs and filtRs per Batch
filtFs.sample.names <- sapply(strsplit(basename(filtFs), "_"), `[`, 1)

Batch12.filtFs <- filtFs[filtFs.sample.names %in% Batch12.sample.names]
Batch12.filtRs <- filtRs[filtFs.sample.names %in% Batch12.sample.names]
names(Batch12.filtFs) <- Batch12.sample.names
names(Batch12.filtRs) <- Batch12.sample.names
Batch15.filtFs <- filtFs[filtFs.sample.names %in% Batch15.sample.names]
Batch15.filtRs <- filtRs[filtFs.sample.names %in% Batch15.sample.names]
names(Batch15.filtFs) <- Batch15.sample.names
names(Batch15.filtRs) <- Batch15.sample.names
Batch54.filtFs <- filtFs[filtFs.sample.names %in% Batch54.sample.names]
Batch54.filtRs <- filtRs[filtFs.sample.names %in% Batch54.sample.names]
names(Batch54.filtFs) <- Batch54.sample.names
names(Batch54.filtRs) <- Batch54.sample.names
Batch7.filtFs <- filtFs[filtFs.sample.names %in% Batch7.sample.names]
Batch7.filtRs <- filtRs[filtFs.sample.names %in% Batch7.sample.names]
names(Batch7.filtFs) <- Batch7.sample.names
names(Batch7.filtRs) <- Batch7.sample.names

Batches <- c("Batch12","Batch15","Batch54","Batch7")

learn_derep_merge_save <- function(filt_Fs, filt_Rs, Batch.sample.names, output){

	# Learn forward error rates
	errF <- learnErrors(filt_Fs, nread=1e6, multithread=TRUE)
	# Learn reverse error rates
	errR <- learnErrors(filt_Rs, nread=1e6, multithread=TRUE)

	# objects to store output (for tracking read numbers)
	mergers <- vector("list", length(Batch.sample.names))
	names(mergers) <- Batch.sample.names
	ddFs <- vector("list", length(Batch.sample.names))
	names(ddFs) <- Batch.sample.names
	
	# Sample inference and merger of paired-end reads
	for(sam in Batch.sample.names) {
	  cat("Processing:", sam, "\n")
	    derepF <- derepFastq(filt_Fs[[sam]])
	    ddF <- dada(derepF, err=errF, multithread=TRUE, pool=TRUE)
	    ddFs[[sam]] <- ddF
	    derepR <- derepFastq(filt_Rs[[sam]])
	    ddR <- dada(derepR, err=errR, multithread=TRUE)
	    merger <- mergePairs(ddF, derepF, ddR, derepR)
	    mergers[[sam]] <- merger
	}
	rm(derepF); rm(derepR)

	# Construct and save sequence table
	seqtab <- makeSequenceTable(mergers)
	saveRDS(seqtab, output)

	# Save ddFs and mergers for tracking read numbers
	saveRDS(ddFs, paste("Batch",length(Batch.sample.names),"ddFs","stringent.rds",sep="_"))
	saveRDS(mergers, paste("Batch",length(Batch.sample.names),"mergers","stringent.rds",sep="_"))

}

learn_derep_merge_save(Batch12.filtFs, Batch12.filtRs, Batch12.sample.names, "./Batch12_seqtab_stringent.rds")
learn_derep_merge_save(Batch15.filtFs, Batch15.filtRs, Batch15.sample.names, "./Batch15_seqtab_stringent.rds")
learn_derep_merge_save(Batch54.filtFs, Batch54.filtRs, Batch54.sample.names, "./Batch54_seqtab_stringent.rds")
learn_derep_merge_save(Batch7.filtFs, Batch7.filtRs, Batch7.sample.names, "./Batch7_seqtab_stringent.rds")

# The final result, the count matrix of samples (rows) by non-chimeric sequence variants (columns).

### Merge Runs & Remove Chimeras ###
# This part is the same as in the single-read version of this workflow.

# Merge multiple runs 
st1 <- readRDS("./Batch12_seqtab_stringent.rds")
st2 <- readRDS("./Batch15_seqtab_stringent.rds")
st3 <- readRDS("./Batch54_seqtab_stringent.rds")
st4 <- readRDS("./Batch7_seqtab_stringent.rds")
st.all <- mergeSequenceTables(st1, st2, st3, st4)

# Remove chimeras
seqtab.nochim.consensus <- removeBimeraDenovo(st.all, method="consensus", multithread=TRUE)
# Once the full-study sequence table is created, chimeras can be identified 
# and removed, and taxonomy assigned. For chimera removal, we have found 
# that the "consensus" chimera removal method works better on large studies, 
# but the "pooled" method is also an option.
seqtab.nochim.pooled <- removeBimeraDenovo(st.all, method="pooled", multithread=TRUE)


## Track reads
# Bringing in dada and merger read counts for tracking
ddFs1 <- readRDS("./Batch_12_ddFs_stringent.rds")
ddFs2 <- readRDS("./Batch_15_ddFs_stringent.rds")
ddFs3 <- readRDS("./Batch_54_ddFs_stringent.rds")
ddFs4 <- readRDS("./Batch_7_ddFs_stringent.rds")
ddFs <- c(ddFs1, ddFs2, ddFs3, ddFs4)

mergers1 <- readRDS("./Batch_12_mergers_stringent.rds")
mergers2 <- readRDS("./Batch_15_mergers_stringent.rds")
mergers3 <- readRDS("./Batch_54_mergers_stringent.rds")
mergers4 <- readRDS("./Batch_7_mergers_stringent.rds")
mergers <- c(mergers1, mergers2, mergers3, mergers4)

# Must re-order ddFs and mergers by out to properly track reads
# Due to batch processing of samples
ddFs <- ddFs[rownames(out)]
mergers <- mergers[rownames(out)]
# seqtab.nochim.pooled is reordered when calling track

getN <- function(x) sum(getUniques(x))
track <- cbind(out, sapply(ddFs, getN), sapply(mergers, getN), rowSums(seqtab.nochim.pooled[rownames(out),]))
# If processing a single sample, remove the sapply calls: e.g. replace sapply(dadaFs, getN) with getN(dadaFs)
colnames(track) <- c("Input", "Filtered", "Denoised", "Merged", "NoChim.pool")
rownames(track) <- kruger.sample.names

require(xtable)
print(xtable(track),file="./16S_track_reads.tex")

# Write to disk
saveRDS(seqtab.nochim.pooled, "./16S_seqtab.rds")


## Taxonomy Assignment 
# To assign taxonomy with SILVA, must format for dada2
# https://zenodo.org/record/801832#.WmC_sB2YXmE
seqtab <- readRDS("./16S_seqtab.rds")
tax <- assignTaxonomy(seqtab, "/data/kruger/silva/128_dada2/silva_nr_v128_train_set.fa.gz", multithread=TRUE)
# add species by exact sequence matching
tax <- addSpecies(tax, "/data/kruger/silva/128_dada2/silva_species_assignment_v128.fa.gz")
saveRDS(tax, "./16S_silva128_dada2_tax.rds")
