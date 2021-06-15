require(dada2); packageVersion("dada2") # 1.6.0
require(phyloseq); packageVersion("phyloseq") # 1.22.3
require(ggplot2); packageVersion("ggplot2") # 2.2.1
require(ape); packageVersion("ape")# 5.0
require(dplyr); packageVersion("dplyr")# 0.7.4
require(vegan); packageVersion("vegan")# 2.4.6

seqtab <- readRDS("./16S_seqtab.rds")
tax <- readRDS("./16S_silva128_dada2_tax.rds")
genus_species <- paste(unname(tax)[,6], unname(tax)[,7], sep="_")
tree <- ladderize(read_tree("FastTree_GTRCAT_16S_bacteria_filtered_005_09.tre"))
sampdf <- read.csv("sample_metadata.csv", as.is=T)


# Identifying BLANKS
blanks <- seqtab[grep("BLANK",rownames(seqtab)),]
samples.out <- rownames(seqtab)
sample <- gsub("Max-","", samples.out)
sample <- gsub("-","_", sample)
site <- sapply(strsplit(samples.out, "-"), `[`, 2)
num <- as.numeric(gsub("[A-z]","",sample))
AB <- sapply(strsplit(samples.out, "[0-9]"), `[`, 2)
AB <- gsub("-.*$","",AB)
S_XS <- sapply(strsplit(sample, "_"), `[`, 3)

sample_data <- data.frame(sample=as.character(sample), site=site, num=num, AB=AB, S_XS=S_XS, stringsAsFactors=FALSE)
sampdf <- left_join(sampdf, sample_data)
rownames(sampdf) <- samples.out

# Merge into phyloseq object
colnames(seqtab) <- paste0("SV", 1:ncol(seqtab))
rownames(tax) <- paste0("SV", 1:nrow(tax))

ps <- phyloseq(otu_table(seqtab, taxa_are_rows=FALSE), 
               sample_data(sampdf), 
               tax_table(tax),
               phy_tree(tree))

sample_names(ps) <- sample_data(ps)$sample

dim(seqtab)# 3533
ps # 3393 taxa - automatically dropped ASVs missing from the tree 


env_table <- sample_data(ps) %>% select(c(sample_num, site, AB, date, temp_c, 
                      ms_cm, do_percent, do_ch, ph)) %>%
                  unique() %>% unclass() %>% as.data.frame()

# require(xtable)
# print(xtable(env_table),file="./env_table.tex")

#######################
## Data descriptions ##                  
#######################

# Proportion of ASVs by read
sum(colSums(otu_table(ps))==1)/dim(otu_table(ps))[2]
# 15.44% (n=524) of ASVs are represented by a single read

# What proportion of reads are singletons?
sum(colSums(otu_table(ps))==1)/sum(otu_table(ps))*100
# 0.0476 % of all reads

## Composition of BLANKS
ps_blank <- subset_samples(ps, site=="BLANK")
# remove zero-abundance taxa
ps_blank <- filter_taxa(ps_blank, function(x) sum(x) > 0, TRUE)
sum(otu_table(ps_blank))# 38,598
dim(otu_table(ps_blank))# 43 taxa
blank_taxa <- colnames(otu_table(ps_blank))
# otu_table(ps_blank)[,blank_taxa]

# Reads in Blanks
ps_nonblank <- subset_samples(ps, site!="BLANK")
# total reads for taxa found in blanks
sum(otu_table(ps_nonblank)[,blank_taxa])
# None of the ASVs found in the Blanks were found in the other samples

# ASVs in Blanks
blank1 <- otu_table(ps_blank)[1,]
blank2 <- otu_table(ps_blank)[2,]

# both dominated by a single ASV (SV7)
blank1[,"SV7"]/sum(blank1)# 0.53
blank2[,"SV7"]/sum(blank2)# 0.857
sum(otu_table(ps_blank)[,"SV7"])/sum(otu_table(ps_blank))#0.667

# which ASVs are found in both
blank1_non0 <- blank1[,blank1>0]
blank2_non0 <- blank2[,blank2>0]
union(colnames(blank1_non0), colnames(blank2_non0))#43 ASVs
intersect(colnames(blank1_non0), colnames(blank2_non0))#9 ASVs

# Read counts and ASVs per sample
sample_summary <- data.frame(totReads = rowSums(otu_table(ps)))
sample_summary$sample <- rownames(sample_summary)

rich <- estimate_richness(ps, split = TRUE, measures = NULL)
rich$sample <- row.names(sample_summary)

reads_asvs <- merge(sample_summary, rich, by="sample")[,1:3]
head(reads_asvs)
# require(xtable)
# print(xtable(reads_asvs),file="./16S_reads_asvs.tex")


## Creating "core" sample set (removing S_XS, and BLANK samples)
ps_core <- subset_samples(ps, is.na(S_XS))
ps_core <- subset_samples(ps_core, site!="BLANK")
# Removing daily NWA samples from core
ps_core <- prune_samples(!sample_data(ps_core)$sample_num%in%c("NWA_3","NWA_4","NWA_5","NWA_6"), ps_core)

# Make a data frame with a column for the read counts of each sample
sample_sum_df <- data.frame(sum = sample_sums(ps_core))

# Histogram of sample read counts
depth_plot <- ggplot(sample_sum_df, aes(x = sum)) + 
  geom_histogram(color = "black", fill = "lightgrey", binwidth = 2500) +
  xlab("Read counts") + ylab("Number of samples") + 
  theme_bw() + theme(panel.border = element_blank(),
    plot.title = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) + scale_y_continuous(expand = c(0, 0))
# depth_plot
# ggsave("depth_plot.pdf", depth_plot, width=3, height=3)

# Number of ASVs in ps_core
sum(colSums(otu_table(ps_core))>0)

# Boxplot of reads per site (core samples)
ps.site <- merge_samples(ps_core, "site", fun=mean)
reads_site <- cbind(sample_sums(ps_core), sample_data(ps_core)[,1:5])
names(reads_site)[1] <- "totReads"

# pdf("reads_per_site_core.pdf", width=7, height=5)
# with(reads_site, plot(totReads ~ as.factor(site), ylab="Reads",xlab="Site"))
# dev.off()

# Re-doing for S_XS
smalls <- unique(sample_data(ps)$sample_num[!is.na(S_XS)])
ps_small <- subset_samples(ps, sample_data(ps)$sample_num%in%smalls)
sample_data(ps_small)$site_size <- with(sample_data(ps_small), paste(site,S_XS, sep="_"))
sample_data(ps_small)$site_size <- gsub("_NA","",sample_data(ps_small)$site_size)

sample_data(ps_small)$S_XS[is.na(sample_data(ps_small)$S_XS)] <- "FULL"

sample_data(ps_small)[sample_data(ps_small)=="FULL"] <- "150"
sample_data(ps_small)[sample_data(ps_small)=="S"] <- "50"
sample_data(ps_small)[sample_data(ps_small)=="XS"] <- "15"
sample_data(ps_small)$S_XS <- as.factor(sample_data(ps_small)$S_XS)
levels(sample_data(ps_small)$S_XS) <- c("15","50","150")

reads_site_small <- cbind(sample_sums(ps_small), sample_data(ps_small))
names(reads_site_small)[1] <- "totReads"

# pdf("reads_per_sample_S_XS.pdf", width=17, height=10)
# par(mfrow=c(2,1),oma = c(5,4,0,0) + 0.1,mar = c(0,0,1,1) + 0.1)
# with(reads_site_small[reads_site_small$AB=="A",], plot(totReads ~ as.factor(sample), ylab="Reads",xlab=NULL, xaxt="n"))
# axis(1, at=1:(length(reads_site_small$sample)/2), labels=rep("",18))
# legend("topright",bty="n","A samples")
# with(reads_site_small[reads_site_small$AB=="B",], plot(totReads ~ as.factor(sample), ylab="Reads",xlab="Sample", xaxt="n"))
# axis(1, at=1:(length(reads_site_small$sample)/2), labels=unique(sort(reads_site_small$site_size)))
# legend("topright",bty="n","B samples")
# mtext("Reads", side = 2, outer = TRUE, cex = 1, line = 2.2)
# mtext("Sample", side = 1, outer = TRUE, cex = 1, line = 2.2)
# dev.off()

# reorder by section
sites_by_section <- ps_core %>% psmelt() %>% select(site, section)%>% unique()
sites_by_section <- sites_by_section$site[rev(order(sites_by_section$section))]
ps_core2 <- ps_core
sample_data(ps_core2)$site <- factor(sample_data(ps_core2)$site, levels=sites_by_section)

section_manual_scale <- c(  "#a52626",
						              	"#548b54",
						              	"#27408b",
						              	"#cbc563",
						              	"#61b9ca"
						              )

rich = plot_richness(ps_core2, "site", color="section", shape="type", 
			measures=c("Observed","Shannon")) + 
			scale_colour_manual(values=section_manual_scale) + 
			theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
			geom_point(size=3)
# rich
# ggsave(rich, file="alpha_div.pdf", height=4, width=6)

lm1 <- summary(lm(rowSums(otu_table(ps))~rowSums(otu_table(ps)>1)))

# pdf("reads_vs_asv.pdf",width=4.5, height=4.5)
plot((rowSums(otu_table(ps))),rowSums(otu_table(ps)>1), pch=19, xlab="Reads per sample", ylab="ASV richness")
legend("topright", bty="n", legend=paste("r2 adj =", format(lm1$adj.r.squared, digits=2),"\n",
										"p =", format(lm1$coefficients[2,4], digits=3)))
# dev.off()


############################################
## Comparison of full with S & XS samples ##
############################################
site_manual_scale <- c( "#a52626",#DLP
            						"#1056b8",#HOY
            						"#61b9ca",#KWA
            						"#672262",#NGO
            						"#009200",#NWA
            						"#d581b8"#NYA
            						)

# Alpha diversity across small samples
smalls <- unique(sample_data(ps)$sample_num[!is.na(S_XS)])
ps_small <- subset_samples(ps, sample_data(ps)$sample_num%in%smalls)

sample_data(ps_small)$S_XS[is.na(sample_data(ps_small)$S_XS)] <- "FULL"

sample_data(ps_small)[sample_data(ps_small)=="FULL"] <- "150"
sample_data(ps_small)[sample_data(ps_small)=="S"] <- "50"
sample_data(ps_small)[sample_data(ps_small)=="XS"] <- "15"
sample_data(ps_small)$S_XS <- as.factor(sample_data(ps_small)$S_XS)
levels(sample_data(ps_small)$S_XS) <- c("15","50","150")

rich_small = plot_richness(ps_small, x="S_XS",measures=c("Observed","Shannon"))
rich_small <- rich_small + geom_boxplot(alpha=0.1)
rich_small <- rich_small + theme(panel.background = element_blank(), 
				panel.border = element_rect(colour = "black", fill=NA, size=0.5),
				axis.text.x = element_text(angle = 90)) 
rich_small <- rich_small + labs(x ="Sample Volume (mL)")
rich_small <- rich_small + geom_point(aes(colour = factor(site))) + scale_colour_manual(values=site_manual_scale)
# rich_small
# ggsave(rich_small, file="alpha_div_S_XS_colour.pdf", height=4, width=6)


# Ordination
ps_small_rel <- transform_sample_counts(ps_small, function(OTU) OTU/sum(OTU))
small.bray.ord <- ordinate(ps_small_rel, method="NMDS", distance="bray", try=40)
# plot_ordination(ps_small, small.bray.ord, color="site", title="Bray NMDS")
# plot_ordination(ps_small, small.bray.ord, color="site", title="Bray NMDS")

site_manual_scale <- c( "#a52626",#DLP
						"#1056b8",#HOY
						"#61b9ca",#KWA
						"#672262",#NGO
						"#009200",#NWA
						"#d581b8"#NYA
						)

p_small <- plot_ordination(ps_small, small.bray.ord, color="site", shape="S_XS") 
p_small <- p_small + scale_colour_manual(values=site_manual_scale) + geom_point(size=4)
p_small <- p_small + theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5)) 
# p_small
# ggsave(p_small, file="NDMS_bray_S_XS_site_rel.pdf", width=6, height=5)


# Taxonomic composition bar plot:
ps.small.trans <- transform_sample_counts(ps_small, function(OTU) OTU/sum(OTU))
sample_data(ps.small.trans)$site <- as.factor(rownames(sample_data(ps.small.trans)))

# Plot for top X phyla, making the rest "Other"
rev(sort(table(tax_table(ps.small.trans)[, "Phylum"], exclude = NULL)))
top7 <- names(rev(sort(table(tax_table(ps.small.trans)[, "Phylum"], exclude = NULL))))[1:7]
# Make those not in top 7 "Other"
tax_table(ps.small.trans)[,2][(!tax_table(ps.small.trans)[,2]%in%top7)] <- "Other"

mdf <- psmelt(ps.small.trans)
mdf$Sample <- as.character(mdf$Sample)
mdf$Sample[grep("[0-9]A",mdf$Sample)] <- "A"
mdf$Sample[grep("[0-9]B",mdf$Sample)] <- "B"

mdf$S_XS[mdf$S_XS=="FULL"] <- "150"
mdf$S_XS[mdf$S_XS=="S"] <- "50"
mdf$S_XS[mdf$S_XS=="XS"] <- "15"
mdf$S_XS <- as.factor(mdf$S_XS)
levels(mdf$S_XS) <- c("150","50","15")

mdf$sample_num <- sub("_.","",mdf$sample_num)

p = ggplot(mdf, aes_string(x="Sample", y="Abundance", fill="Phylum"))
p = p + geom_bar(stat="identity", position="stack")
p = p + theme(axis.text.x=element_text(angle=90, hjust=0),
	panel.background = element_blank()) 
p = p + scale_y_continuous(labels = scales::percent, expand = c(0, 0))
p = p + labs(x ="", y = "Relative abundance (%)")

phyla_manual_scale <- c("#8d312f",
            						"#2d886e",
            						"#3569a0",
            						"#cbc563",
            						"#61b9ca",
            						"#A9A9A9",#other
            						"#7e629c",
            						"#672262",
            						"#a0a136",
            						"#225667")
p <- p + scale_fill_manual(values = phyla_manual_scale)
p <- p + facet_grid(sample_num~S_XS, shrink=TRUE, drop=TRUE, scales="free", space="free")
p <- p + theme(panel.spacing = unit(1, "lines"))
# p
# ggsave(p, file="phyla_by_S_XS_5x10.pdf", width=5, height=10)


#####################################################
## Phylogenetic Diversity (Ecophyogenetic metrics) ##
#####################################################

require(picante); packageVersion("picante")# 1.6.2
ps_pd <- pd(samp=as(otu_table(ps_core2), "matrix"), tree=phy_tree(ps_core2), include.root=FALSE)
ps_pd$sample <- as.factor(rownames(ps_pd))
ps_pd <- left_join(ps_pd, sample_data(ps_core2))
names(ps_pd)

boxplot(ps_pd$PD ~ ps_pd$SR)
boxplot(ps_pd$PD ~ as.factor(ps_pd$site), xlab = "Site", ylab = "Faith's PD")
boxplot(ps_pd$PD ~ as.factor(ps_pd$section), xlab = "Section", ylab = "Faith's PD")
boxplot(ps_pd$PD ~ as.factor(ps_pd$type), xlab = "Hole Type", ylab = "Faith's PD")
boxplot(ps_pd$PD ~ as.factor(ps_pd$geology), xlab = "Geology", ylab = "Faith's PD")

# SES.MPD
phy.dist <- cophenetic(phy_tree(ps_core))

# calculate ses.mpd
comm.sesmpd <- ses.mpd(as(otu_table(ps_core), "matrix"), phy.dist, null.model = "richness", abundance.weighted = TRUE, 
    runs = 999)

sig <- rownames(comm.sesmpd[abs(comm.sesmpd$mpd.obs.z) > 1.96,])
comm.sesmpd[sig,]
# all clustered

# calculate ses.mntd
comm.sesmntd <- ses.mntd(as(otu_table(ps_core), "matrix"), phy.dist, null.model = "richness", abundance.weighted = TRUE, 
    runs = 999)

sig.mntd <- rownames(comm.sesmntd[abs(comm.sesmntd$mntd.obs.z) > 1.96,])
comm.sesmntd[sig.mntd,]

# pdf("ses_metrics.pdf", height = 4, width = 7)
# par(mfrow=c(1,2))
# with(comm.sesmpd, plot(mpd.obs.z , mpd.obs, pch=20, xlab="MPD z-score", ylab="Observed MPD", col="#4E8B94"))
# abline(v = 0, col = "gray")
# abline(v = -1.96, col = "#B22222", lty=2, lwd=2)
# abline(v = 1.96, col = "#B22222", lty=2, lwd=2 )

# with(comm.sesmntd, plot(mntd.obs.z , mntd.obs, pch=20, xlab="MNTD z-score", ylab="Observed MNTD", xlim=c(-5.5, 0.2), col="#4E8B94"))
# abline(v = 0, col = "gray")
# abline(v = -1.96, col = "#B22222", lty=2, lwd=2 )
# abline(v = 1.96, col = "#B22222", lty=2, lwd=2 )
# dev.off()
# par(mfrow=c(1,1))


################################################
## ASV accumulation curve (on "core" samples) ##
################################################

exact <- specaccum(otu_table(ps_core), method="exact")

# pdf("16s_specaccum.pdf", width=4, height=4)
plot(exact, ylab="Number of Bacterial ASVs", xlab="Number of 150mL samples")
# dev.off()

# Total ASV estimation
richness_estimate <- specpool(otu_table(ps_core))
richness_estimate


#######################################################
## Tree with Relative abundance of ASVs across sites ##
#######################################################

ps.site <- merge_samples(ps_core, "site", fun=mean)
ps.site.trans <- transform_sample_counts(ps.site, function(OTU) OTU/sum(OTU))
sample_data(ps.site.trans)$site <- as.factor(rownames(sample_data(ps.site.trans)))

site_manual_scale <- c( "#85c248",#NHL
            						"#a52626",#DLP
            						"#009200",#NWA
            						"#61b9ca",#KWA
            						"#6c44e5",#GIR
            						"#b87100",#WIT
            						"#cbc563",#IMB
            						"#1056b8",#HOY
            						"#d581b8",#NYA
            						"#672262"#NGO
            						)

# reorder by section
sites_by_section <- ps_core %>% psmelt() %>% select(site, section)%>% unique()
sites_by_section <- sites_by_section$site[rev(order(sites_by_section$section))]
sample_data(ps.site.trans)$site <- factor(rownames(sample_data(ps.site.trans)), levels=sites_by_section)

tree_sites <- plot_tree(ps.site.trans,
                            size = "Abundance",
    				          	    color = "site",
        		  	            justify = "yes jagged", 
          			            ladderize = "left",
          			            base.spacing = 0.05,
          			            plot.margin = 0.01) +
                            scale_size_continuous(range = c(0,8)) +
                            scale_colour_manual(values=site_manual_scale)
# tree_sites
# ggsave(tree_sites, file="fastTree_ASV_abund_sites.pdf", width=7, height=6)


#########################
## Taxonomic Diversity ##
#########################

# % ASVs assigned per taxonomic level
tax.tab <- tax_table(ps)

# pdf("tax_assign_prop.pdf", width=5, height=5)
# barplot(apply(tax.tab, 2, function(x) sum(!is.na(x))/nrow(tax.tab))[2:6], ylim=c(0,1), col="#4E8B94", 
# 	ylab="Classified sequences up to taxonomic level (%)")
# dev.off()

# Create table, number of features for each phyla
phyla_count <- table(tax_table(ps)[, "Phylum"], exclude = NULL)
phyla_count <- phyla_count[rev(order(phyla_count))]
# (phyla_count/sum(phyla_count))[1:5]

# Create table, number of features for each class
class_count <- table(tax_table(ps)[, "Class"], exclude = NULL)
class_count <- class_count[rev(order(class_count))]
# (class_count/sum(class_count))[1:5]


# Taxonomic composition bar plot
ps.site <- merge_samples(ps_core, "site", fun=mean)
ps.site.trans <- transform_sample_counts(ps.site, function(OTU) OTU/sum(OTU))
sample_data(ps.site.trans)$site <- as.factor(rownames(sample_data(ps.site.trans)))
# Plot for top X phyla, making the rest "Other"
rev(sort(table(tax_table(ps.site.trans)[, "Phylum"], exclude = NULL)))
top7 <- names(rev(sort(table(tax_table(ps.site.trans)[, "Phylum"], exclude = NULL))))[1:7]
# Make those not in top 7 "Other"
tax_table(ps.site.trans)[,2][(!tax_table(ps.site.trans)[,2]%in%top7)] <- "Other"

# Modify plot_bar to remove OTU delineations
plot_bar_blank = function(physeq, x="Sample", y="Abundance", fill=NULL,
	title=NULL, facet_grid=NULL){
		
	mdf = psmelt(physeq)
	p = ggplot(mdf, aes_string(x=x, y=y, fill=fill))
	p = p + geom_bar(stat="identity", position="stack")
	p = p + theme(axis.text.x=element_text(angle=-90, hjust=0))
	if( !is.null(facet_grid) ){	
		p <- p + facet_grid(facet_grid)
	}
	if( !is.null(title) ){
		p <- p + ggtitle(title)
	}
	return(p)
}

# reorder by percent proteobacteria
require(dplyr)
prop_proteo <- ps.site.trans %>% psmelt() %>% group_by(site, Phylum) %>% 
summarise(sum(Abundance)) %>% filter(Phylum=="Proteobacteria")
names(prop_proteo)[3] <- "prop"
prop_proteo <- prop_proteo$site[rev(order(prop_proteo$prop))]

mdf <- psmelt(ps.site.trans)
mdf$Sample <- factor(mdf$Sample, levels=prop_proteo)
p = ggplot(mdf, aes_string(x="Sample", y="Abundance", fill="Phylum"))
p = p + geom_bar(stat="identity", position="stack")
p = p + theme(axis.text.x=element_text(angle=90, hjust=0),
	panel.background = element_blank()) 
p = p + scale_y_continuous(labels = scales::percent, expand = c(0, 0))
p = p + labs(x ="Site", y = "Relative abundance (%)")

phyla_manual_scale <- c("#8d312f",
            						"#2d886e",
            						"#cbc563",
            						"#3569a0",
            						"#61b9ca",
            						"#A9A9A9",#other
            						"#7e629c",
            						"#672262",
            						"#a0a136",
            						"#225667")
p <- p + scale_fill_manual(values = phyla_manual_scale)
# p
# ggsave(p, file="phyla_by_site.pdf", width=6, height=4.5)


# Plot class
ps.site <- merge_samples(ps_core, "site", fun=mean)
ps.site.trans <- transform_sample_counts(ps.site, function(OTU) OTU/sum(OTU))
sample_data(ps.site.trans)$site <- as.factor(rownames(sample_data(ps.site.trans)))
rev(sort(table(tax_table(ps.site.trans)[, "Class"], exclude = NULL)))
top12 <- names(rev(sort(table(tax_table(ps.site.trans)[, "Class"], exclude = NULL))))[1:12]
# Make NA and those not in top 12 "Other"
tax_table(ps.site.trans)[,3][(!tax_table(ps.site.trans)[,3]%in%top12)] <- "Other"
tax_table(ps.site.trans)[,3][is.na(tax_table(ps.site.trans)[,3])] <- "Other"

# reorder by prop_proteo
mdf <- psmelt(ps.site.trans)
mdf$Sample <- factor(mdf$Sample, levels=prop_proteo)
p_class = ggplot(mdf, aes_string(x="Sample", y="Abundance", fill="Class"))
p_class = p_class + geom_bar(stat="identity", position="stack")
p_class = p_class + theme(axis.text.x=element_text(angle=90, hjust=0),
	panel.background = element_blank()) 
p_class = p_class + scale_y_continuous(labels = scales::percent, expand = c(0, 0))
p_class = p_class + labs(x ="Site", y = "Relative abundance (%)")

class_manual_scale <- c("#729370",
            						"#bd5a12",
            						"#c6759e",
            						"#7585e8",
            						"#c2af20",
            						"#399f65",
            						"#0863c1",
            						"#401516",
            						"#8b2f65",
            						"#20695e",
            						"#A9A9A9",#other
            						"#6386c4",
            						"#feff7a")

p_class <- p_class + scale_fill_manual(values = class_manual_scale) 
# p_class
# ggsave(p_class, file="class_by_site.pdf", width=6, height=4.5)

# Plot order
ps.site <- merge_samples(ps_core, "site", fun=mean)
ps.site.trans <- transform_sample_counts(ps.site, function(OTU) OTU/sum(OTU))
sample_data(ps.site.trans)$site <- as.factor(rownames(sample_data(ps.site.trans)))
rev(sort(table(tax_table(ps.site.trans)[, "Order"], exclude = NULL)))
top17 <- names(rev(sort(table(tax_table(ps.site.trans)[, "Order"], exclude = NULL))))[1:17]
# Make NA and those not in top 17 "Other"
tax_table(ps.site.trans)[,4][(!tax_table(ps.site.trans)[,4]%in%top17)] <- "Other"
tax_table(ps.site.trans)[,4][is.na(tax_table(ps.site.trans)[,4])] <- "Other"

# reorder by prop_proteo
mdf <- psmelt(ps.site.trans)
mdf$Sample <- factor(mdf$Sample, levels=prop_proteo)
p_order = ggplot(mdf, aes_string(x="Sample", y="Abundance", fill="Order"))
p_order = p_order + geom_bar(stat="identity", position="stack")
p_order = p_order + theme(axis.text.x=element_text(angle=90, hjust=0),
	panel.background = element_blank()) 
p_order = p_order + scale_y_continuous(labels = scales::percent, expand = c(0, 0))
p_order = p_order + labs(x ="Site", y = "Relative abundance (%)")

order_manual_scale <- c("#87cefa",
            						"#f08080",
            						"#eead0e",
            						"#eedc82",
            						"#bdb76b",
            						"#68838b",
            						"#cd3278",
            						"#20b2aa",
            						"#9acd32",
            						"#8fbc8f",
            						"#A9A9A9",#other
            						"#db7093",
            						"#ab82ff",
            						"#53868b",
            						"#eedd82",
            						"#009acd",
            						"#bc8f8f",
            						"#caff70",
            						"#6959cd")

p_order <- p_order + scale_fill_manual(values = order_manual_scale) 
# p_order
# ggsave(p_order, file="order_by_site.pdf", width=6, height=4.5)

# Plot family
ps.site <- merge_samples(ps_core, "site", fun=mean)
ps.site.trans <- transform_sample_counts(ps.site, function(OTU) OTU/sum(OTU))
sample_data(ps.site.trans)$site <- as.factor(rownames(sample_data(ps.site.trans)))
rev(sort(table(tax_table(ps.site.trans)[, "Family"], exclude = NULL)))
top16 <- names(rev(sort(table(tax_table(ps.site.trans)[, "Family"], exclude = NULL))))[1:16]
# Make NA and those not in top 16 "Other"
tax_table(ps.site.trans)[,5][(!tax_table(ps.site.trans)[,5]%in%top16)] <- "Other"
tax_table(ps.site.trans)[,5][is.na(tax_table(ps.site.trans)[,5])] <- "Other"

# reorder by prop_proteo
mdf <- psmelt(ps.site.trans)
mdf$Sample <- factor(mdf$Sample, levels=prop_proteo)
p_family = ggplot(mdf, aes_string(x="Sample", y="Abundance", fill="Family"))
p_family = p_family + geom_bar(stat="identity", position="stack")
p_family = p_family + theme(axis.text.x=element_text(angle=90, hjust=0),
	panel.background = element_blank()) 
p_family = p_family + scale_y_continuous(labels = scales::percent, expand = c(0, 0))
p_family = p_family + labs(x ="Site", y = "Relative abundance (%)")
# p_family


#######################################
### Taxonomic Composition over Time ###
#######################################

ps.s.num <- merge_samples(ps_core, "sample_num", fun=mean)
sample_data(ps.s.num)$sample_num <- rownames(sample_data(ps.s.num))
ps.s.num <- subset_samples(ps.s.num, sample_num!="DLP_8")
sample_data(ps.s.num)$site <- gsub("_.","",rownames(sample_data(ps.s.num)))

ps.s.num.trans <- transform_sample_counts(ps.s.num, function(OTU) OTU/sum(OTU))

# Plot for top X phyla, making the rest "Other"
rev(sort(table(tax_table(ps.s.num.trans)[, "Phylum"], exclude = NULL)))
top6 <- names(rev(sort(table(tax_table(ps.s.num.trans)[, "Phylum"], exclude = NULL))))[1:6]
# Make those not in top 6 "Other"
tax_table(ps.s.num.trans)[,2][(!tax_table(ps.s.num.trans)[,2]%in%top6)] <- "Other"

NWA_phyla_manual_scale <- c("#8d312f",
              							"#2d886e",
              							"#cbc563",
              							"#61b9ca",
              							"#A9A9A9",#other
              							"#7e629c",
              							"#672262",
              							"#a0a136",
              							"#225667")

sites_by_section <- ps_core %>% psmelt() %>% select(site, section)%>% unique()
sites_by_section <- sites_by_section$site[rev(order(sites_by_section$section))]
sample_data(ps.s.num.trans)$site <- factor(sample_data(ps.s.num.trans)$site, levels=sites_by_section)

p_weekly <- plot_bar_blank(ps.s.num.trans, fill="Phylum") + scale_fill_manual(values = NWA_phyla_manual_scale)
p_weekly <- p_weekly + theme(axis.text.x=element_text(angle=90, hjust=1),
			panel.background = element_blank()) 
p_weekly <- p_weekly + scale_y_continuous(labels = scales::percent, expand = c(0, 0))
p_weekly <- p_weekly + labs(x ="", y = "Relative abundance (%)")
p_weekly <- p_weekly + facet_grid(~site, shrink=TRUE, drop=TRUE, scales="free", space="free")
# p_weekly
# ggsave(p_weekly, file="phyla_by_week_7x4.pdf", width=7, height=4)


# Plot for top X classes, making the rest "Other"
rev(sort(table(tax_table(ps.s.num.trans)[, "Class"], exclude = NULL)))
top12 <- names(rev(sort(table(tax_table(ps.s.num.trans)[, "Class"], exclude = NULL))))[1:12]
# Make those not in top 12 "Other"
tax_table(ps.s.num.trans)[,3][(!tax_table(ps.s.num.trans)[,3]%in%top12)] <- "Other"
tax_table(ps.s.num.trans)[,3][is.na(tax_table(ps.s.num.trans)[,3])] <- "Other"

class_manual_scale <- c("#729370",
            						"#bd5a12",
            						"#c6759e",
            						"#7585e8",
            						"#c2af20",
            						"#399f65",
            						"#0863c1",
            						"#401516",
            						"#8b2f65",
            						"#20695e",
            						"#A9A9A9",#other
            						"#6386c4",
            						"#feff7a")

p_weekly <- plot_bar_blank(ps.s.num.trans, fill="Class") + scale_fill_manual(values = class_manual_scale)
p_weekly <- p_weekly + theme(axis.text.x=element_text(angle=90, hjust=1),
			panel.background = element_blank()) 
p_weekly <- p_weekly + scale_y_continuous(labels = scales::percent, expand = c(0, 0))
p_weekly <- p_weekly + labs(x ="", y = "Relative abundance (%)")
p_weekly <- p_weekly + facet_grid(~site, shrink=TRUE, drop=TRUE, scales="free", space="free")
# p_weekly
# ggsave(p_weekly, file="class_by_week_7x4.pdf", width=7, height=4)

# Temporal change across one week (NWA)
# NWA_2 - June26
# NWA_3 - June29
# NWA_4 - June30
# NWA_5 - July1
# NWA_6 - July2
# NWA_7 - July3
# NWA_8 - July10

# NWA_2_7_8 should be in ps_core, 3_4_5_6_7 should be in daily
ps_NWA <- subset_samples(ps, is.na(S_XS))
ps_NWA <- subset_samples(ps_NWA, site!="BLANK")
ps_NWA <- prune_samples(sample_data(ps_NWA)$site=="NWA", ps_NWA)
ps_NWA <- prune_samples(sample_data(ps_NWA)$num%in%c(3,4,5,6,7), ps_NWA)
ps.NWA.trans <- transform_sample_counts(ps_NWA, function(OTU) OTU/sum(OTU))
sample_data(ps.NWA.trans)$site <- as.factor(rownames(sample_data(ps.NWA.trans)))

# Plot for top X phyla, making the rest "Other"
rev(sort(table(tax_table(ps.NWA.trans)[, "Phylum"], exclude = NULL)))
top6 <- names(rev(sort(table(tax_table(ps.NWA.trans)[, "Phylum"], exclude = NULL))))[1:6]
# Make those not in top 6 "Other"
tax_table(ps.NWA.trans)[,2][(!tax_table(ps.NWA.trans)[,2]%in%top6)] <- "Other"

# Change sample names to dates
sample_names(ps.NWA.trans) <- c("4A - June 30", "3A - June 29", "3B - June 29",
								"4B - June 30", "5A - July 1", "5B - July 1",
								"6A - July 2", "6B - July 2", "7A - July 3", "7B - July 3")

NWA_phyla_manual_scale <- c("#8d312f",
              							"#2d886e",
              							"#cbc563",
              							"#61b9ca",
              							"#A9A9A9",#other
              							"#7e629c",
              							"#672262",
              							"#a0a136",
              							"#225667")

p_daily <- plot_bar_blank(ps.NWA.trans, fill="Phylum") + scale_fill_manual(values = NWA_phyla_manual_scale)
p_daily <- p_daily + theme(axis.text.x=element_text(angle=90, hjust=1),
			panel.background = element_blank()) 
p_daily <- p_daily + scale_y_continuous(labels = scales::percent, expand = c(0, 0))
p_daily <- p_daily + labs(x ="", y = "Relative abundance (%)")
# p_daily
# ggsave(p_daily, file="NWA_phyla_by_day.pdf", width=6, height=4.5)

# Plot Class Daily
rev(sort(table(tax_table(ps.NWA.trans)[, "Class"], exclude = NULL)))
top12 <- names(rev(sort(table(tax_table(ps.NWA.trans)[, "Class"], exclude = NULL))))[1:12]
# Make those not in top 12 "Other"
tax_table(ps.NWA.trans)[,3][(!tax_table(ps.NWA.trans)[,3]%in%top12)] <- "Other"
tax_table(ps.NWA.trans)[,3][is.na(tax_table(ps.NWA.trans)[,3])] <- "Other"

p_daily_class <- plot_bar_blank(ps.NWA.trans, fill="Class") + scale_fill_manual(values = class_manual_scale)
p_daily_class <- p_daily_class + theme(axis.text.x=element_text(angle=90, hjust=1),
			panel.background = element_blank()) 
p_daily_class <- p_daily_class + scale_y_continuous(labels = scales::percent, expand = c(0, 0))
p_daily_class <- p_daily_class + labs(x ="", y = "Relative abundance (%)")
# p_daily_class
# ggsave(p_daily_class, file="NWA_class_by_day.pdf", width=6, height=4.5)

# Plot Order Daily
rev(sort(table(tax_table(ps.NWA.trans)[, "Order"], exclude = NULL)))
top17 <- names(rev(sort(table(tax_table(ps.NWA.trans)[, "Order"], exclude = NULL))))[1:17]
# Make those not in top 17 "Other"
tax_table(ps.NWA.trans)[,4][(!tax_table(ps.NWA.trans)[,4]%in%top17)] <- "Other"
tax_table(ps.NWA.trans)[,4][is.na(tax_table(ps.NWA.trans)[,4])] <- "Other"

p_daily_order <- plot_bar_blank(ps.NWA.trans, fill="Order") + scale_fill_manual(values = order_manual_scale)
p_daily_order <- p_daily_order + theme(axis.text.x=element_text(angle=90, hjust=1),
			panel.background = element_blank()) 
p_daily_order <- p_daily_order + scale_y_continuous(labels = scales::percent, expand = c(0, 0))
p_daily_order <- p_daily_order + labs(x ="", y = "Relative abundance (%)")
# p_daily_order
# ggsave(p_daily_order, file="NWA_order_by_day.pdf", width=6, height=4.5)


###################
### Ordinations ###
###################

## Raw abundances

# Bray-Curtis
ord.nmds.bray <- ordinate(ps_core, method="NMDS", distance="bray", try=40)

# Abundance-Weighted UniFrac
# Must root tree to this outgroup in order to have convergence in 20 iterations (try=___ does not work with unifrac distance... post issue on github)
phy_tree(ps_core) <- root(phy=phy_tree(ps_core), outgroup="SV1842", resolve.root=TRUE, interactive=FALSE)
ord.nmds.wunif <- ordinate(ps_core, "NMDS", distance="unifrac", weighted=TRUE)

## Relative abundances
psrel <- transform_sample_counts(ps_core, function(OTU) OTU/sum(OTU))

ord.nmds.bray.rel <- ordinate(psrel, method="NMDS", distance="bray", try=40)
ord.nmds.wunif.rel <- ordinate(psrel, "NMDS", distance="unifrac", weighted=TRUE)

# pdf("nmds_wunif_rel_stressplot.pdf", width=5, height=5)
stressplot(ord.nmds.wunif.rel)
# dev.off()
# Non-metric fit r2 = 0.981
# Linear fit r2 = 0.948
# ord.nmds.wunif.rel # stress = 0.13859

# pdf("nmds_bray_rel_stressplot.pdf", width=5, height=5)
stressplot(ord.nmds.bray.rel)
# dev.off()
# Non-metric fit r2 = 0.969
# Linear fit r2 = 0.863
# ord.nmds.bray.rel # stress = 0.1769


site_manual_scale <- c( "#a52626",
						            "#6c44e5",
						            "#1056b8",
						            "#cbc563",
						            "#61b9ca",
						            "#672262",
						            "#85c248",
						            "#009200",
						            "#d581b8",
						            "#b87100"
						            )

type_manual_scale <- c( "#9bcd9b",
						            "#8b4c39",
						            "#00688b"
						            )

section_manual_scale <- c(  "#a52626",
						              	"#548b54",
						              	"#27408b",
						              	"#cbc563",
						              	"#61b9ca"
						              )


# Weighted UniFrac NMDS
p_unif <- plot_ordination(ps_core, ord.nmds.wunif, color="site") 
p_unif <- p_unif + scale_colour_manual(values=site_manual_scale) + geom_point(size=4)
p_unif <- p_unif + theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5)) 
# p_unif
# ggsave(p_unif, file="NDMS_wunif_site_core_5x3.pdf", width=5, height=3)

p_unif <- plot_ordination(ps_core, ord.nmds.wunif.rel, color="section", shape="type") 
p_unif <- p_unif + scale_colour_manual(values=section_manual_scale) + geom_point(size=4)
p_unif <- p_unif + theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5)) 
# p_unif
# ggsave(p_unif, file="NDMS_wunif_rel_section_type_core_5x3.pdf", width=5, height=3)

# p_unif by waterhole type and geology
p_unif <- plot_ordination(ps_core, ord.nmds.wunif.rel, color="geology", shape="type") 
p_unif <- p_unif + scale_colour_manual(values=type_manual_scale) + geom_point(size=4)
p_unif <- p_unif + theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5)) 
# p_unif
# ggsave(p_unif, file="NDMS_wunif_rel_geology_type_core_5x3.pdf", width=5, height=3)


# BRAY NDMS
p_bray <- plot_ordination(ps_core, ord.nmds.bray.rel, color="site") 
p_bray <- p_bray + scale_colour_manual(values=site_manual_scale) + geom_point(size=4)
p_bray <- p_bray + theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5)) 
# p_bray
# ggsave(p_bray, file="NDMS_wbray_rel_site_core_5x3.pdf", width=5, height=3)

p_bray <- plot_ordination(ps_core, ord.nmds.bray.rel, color="section", shape="type") 
p_bray <- p_bray + scale_colour_manual(values=section_manual_scale) + geom_point(size=4)
p_bray <- p_bray + theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5)) 
# p_bray
# ggsave(p_bray, file="NDMS_bray_rel_section_type_core_5x3.pdf", width=5, height=3)

# p_bray by waterhole type and geology
p_bray <- plot_ordination(ps_core, ord.nmds.bray.rel, color="geology", shape="type") 
p_bray <- p_bray + scale_colour_manual(values=type_manual_scale) + geom_point(size=4)
p_bray <- p_bray + theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5)) 
# p_bray
# ggsave(p_bray, file="NDMS_bray_rel_geology_type_core_5x3.pdf", width=5, height=3)


### Plotting vectors

# subset and rename enviro data
env_dat <- sample_data(ps_core)[,c("temp_c","ms_cm","do_ch","ph")]
names(env_dat) <- c("Temperature", "Conductivity","Dissolved Oxygen","pH")

unif.envfit <- envfit(ord.nmds.wunif.rel, env_dat)
bray.envfit.sites <- envfit(ord.nmds.bray.rel, env_dat, display="sites")

# Adding relative abundances of bacterial classes as vectors
ps.rel.abund <- transform_sample_counts(ps_core, function(OTU) OTU/sum(OTU))
top11 <- names(rev(sort(table(tax_table(ps.rel.abund)[, "Class"], exclude = NULL))))[1:11]
# Make NA and those not in top 11 "Other"
tax_table(ps.rel.abund)[,3][(!tax_table(ps.rel.abund)[,3]%in%top11)] <- "Other"
tax_table(ps.rel.abund)[,3][is.na(tax_table(ps.rel.abund)[,3])] <- "Other"

# subset to these classes
top10 <- top11[!is.na(top11)]
top_classes <- subset_taxa(ps.rel.abund, Class %in% top10)

class_df <- psmelt(top_classes)
names(class_df)
require(tidyr);packageVersion("tidyr")# 0.8.0
cdf <- class_df %>% group_by(Sample, Class) %>% 
				mutate(class_Abundance=sum(Abundance)) %>%
				select(Sample, Class, class_Abundance) %>%
				unique()%>%
				spread(Class, class_Abundance)

env_dat2 <- env_dat
env_dat2$Sample <- rownames(env_dat2)
env_tax_dat <- left_join(env_dat2, cdf)
env_tax_dat <- select(env_tax_dat, -Sample) 

tax_dat <- env_tax_dat[,-c(1:4)]

tax.unif.envfit <- envfit(ord.nmds.wunif.rel, tax_dat)
tax.bray.envfit.sites <- envfit(ord.nmds.bray.rel, tax_dat, display="sites")

# pdf("nmds_wunif_rel_vectors_class_core.pdf",width=5, height=5)
tax.nmds.wunif <- ordiplot(ord.nmds.wunif.rel, type="none",display="sites")#, 
				# xlim=c(-0.1,0.07), ylim=c(-0.002,0.02))

# points(tax.nmds.wunif, "sites", col=NULL, bg=rgb(78/256,139/256,148/256,0.5), pch=21)
# plot(tax.unif.envfit, col="black")
plot(tax.unif.envfit, col="black", p.max=0.05)
plot(unif.envfit, col="blue", p.max=0.05)
# dev.off()

# pdf("nmds_bray_rel_vectors_class_core.pdf",width=5, height=5)
tax.nmds.bray <- ordiplot(ord.nmds.bray.rel, type="none", display="sites")#,
				# ylim=c(-4.5,4))
# points(tax.nmds.bray, "sites", col=NULL, bg=rgb(78/256,139/256,148/256,0.5), pch=21)
# plot(tax.bray.envfit.sites, col="black")
plot(tax.bray.envfit.sites, col="black", p.max=0.05)
plot(bray.envfit.sites, col="blue", p.max=0.05)
# dev.off()


# Adding relative abundances of bacterial orders as vectors
ps.rel.abund <- transform_sample_counts(ps_core, function(OTU) OTU/sum(OTU))
rev(sort(table(tax_table(ps.rel.abund)[, "Order"], exclude = NULL)))
top16 <- names(rev(sort(table(tax_table(ps.rel.abund)[, "Order"], exclude = NULL))))[1:16]
# Make NA and those not in top 16 "Other"
tax_table(ps.rel.abund)[,4][(!tax_table(ps.rel.abund)[,4]%in%top16)] <- "Other"
tax_table(ps.rel.abund)[,4][is.na(tax_table(ps.rel.abund)[,4])] <- "Other"

# subset to these classes
top <- top16[!is.na(top16)]
top_orders <- subset_taxa(ps.rel.abund, Order %in% top16)

order_df <- psmelt(top_orders)
cdf <- order_df %>% group_by(Sample, Order) %>% 
				mutate(order_Abundance=sum(Abundance)) %>%
				select(Sample, Order, order_Abundance) %>%
				unique()%>%
				spread(Order, order_Abundance)

env_dat2 <- env_dat
env_dat2$Sample <- rownames(env_dat2)
env_tax_dat <- left_join(env_dat2, cdf)
env_tax_dat <- select(env_tax_dat, -Sample) 

tax_dat <- env_tax_dat[,-c(1:4)]

tax.unif.envfit <- envfit(ord.nmds.wunif.rel, tax_dat)
tax.bray.envfit.sites <- envfit(ord.nmds.bray.rel, tax_dat, display="sites")

# pdf("nmds_wunif_rel_vectors_order_core.pdf",width=5, height=5)
tax.nmds.wunif <- ordiplot(ord.nmds.wunif.rel, type="none",display="sites")#, 
				# xlim=c(-0.1,0.07), ylim=c(-0.002,0.02))

# points(tax.nmds.wunif, "sites", col=NULL, bg=rgb(78/256,139/256,148/256,0.5), pch=21)
# plot(tax.unif.envfit, col="black")
plot(tax.unif.envfit, col="black", p.max=0.05)
plot(unif.envfit, col="blue", p.max=0.05)
# dev.off()

# pdf("nmds_bray_rel_vectors_order_core.pdf",width=5, height=5)
tax.nmds.bray <- ordiplot(ord.nmds.bray.rel, type="none", display="sites")#,
				# ylim=c(-4.5,4))
# points(tax.nmds.bray, "sites", col=NULL, bg=rgb(78/256,139/256,148/256,0.5), pch=21)
# plot(tax.bray.envfit.sites, col="black")
plot(tax.bray.envfit.sites, col="black", p.max=0.05)
plot(bray.envfit.sites, col="blue", p.max=0.05)
# dev.off()


###################################################################
## Beta diversity partitioining for NWA daily and weekly samples ##
###################################################################

require(betapart); packageVersion("betapart")#1.5.0
ps_NWA <- subset_samples(ps, site=="NWA")
ps_NWA <- subset_samples(ps_NWA, is.na(S_XS))
ps_NWA <- prune_taxa(taxa_sums(ps_NWA) > 1, ps_NWA)

# converting num to days based on day of sampling, starting at 1
# NWA_2 is 1, NWA_8 is 15, NWA_7 is 8, 
sample_data(ps_NWA)$num
sample_data(ps_NWA)$num[sample_data(ps_NWA)$num==8] <- 15
sample_data(ps_NWA)$num[sample_data(ps_NWA)$num==7] <- 8
sample_data(ps_NWA)$num[sample_data(ps_NWA)$num==6] <- 7
sample_data(ps_NWA)$num[sample_data(ps_NWA)$num==5] <- 6
sample_data(ps_NWA)$num[sample_data(ps_NWA)$num==4] <- 5
sample_data(ps_NWA)$num[sample_data(ps_NWA)$num==3] <- 4
sample_data(ps_NWA)$num[sample_data(ps_NWA)$num==2] <- 1

com_pa <- otu_table(ps_NWA)
com.dist <- vegdist(com_pa)
com_pa[com_pa>0] <-1
pair.s <- beta.pair(com_pa)

# pdf("beta_sor_NWA.pdf")
plot(hclust(pair.s$beta.sor, method="average"), main="", sub="", xlab="")
title(xlab=expression(beta[sor]), line=0.3)
# dev.off()

# adonis for NWA daily samples
ps_daily <- subset_samples(ps_NWA, sample_data(ps_NWA)$sample_num%in%c("NWA_3","NWA_4","NWA_5","NWA_6"))
ps_daily <- prune_taxa(taxa_sums(ps_daily) > 1, ps_daily)
ps_daily <- transform_sample_counts(ps_daily, function(OTU) OTU/sum(OTU))
com_pa <- otu_table(ps_daily)
com.dist <- vegdist(com_pa)
com_pa[com_pa>0] <-1
pair.s <- beta.pair(com_pa)
daily_df <- as.data.frame(unclass(sample_data(ps_daily)))
adonis(pair.s$beta.sor ~ num, data = daily_df, perm=999)
# Pr(>F)=0.525; R2 among sample times = 0.136

# adonis for NWA weekly samples
ps_weekly <- subset_samples(ps_NWA, sample_data(ps_NWA)$sample_num%in%c("NWA_2", "NWA_7", "NWA_8"))
ps_weekly <- prune_taxa(taxa_sums(ps_weekly) > 1, ps_weekly)
ps_weekly <- transform_sample_counts(ps_weekly, function(OTU) OTU/sum(OTU))
com_pa <- otu_table(ps_weekly)
com.dist <- vegdist(com_pa)
com_pa[com_pa>0] <-1
pair.s <- beta.pair(com_pa)
weekly_df <- as.data.frame(unclass(sample_data(ps_weekly)))
adonis(pair.s$beta.sor ~ num, data = weekly_df, perm=999)
# Pr(>F)=0.022; R2 is 0.49075


################################################
## Additive partitioning of Shannon diversity ##
################################################

# Creating "core" sample set (removing S_XS, and BLANK samples)
ps_core <- subset_samples(ps, is.na(S_XS))
ps_core <- subset_samples(ps_core, site!="BLANK")
# Removing daily NWA samples from core
ps_core <- prune_samples(!sample_data(ps_core)$sample_num%in%c("NWA_3","NWA_4","NWA_5","NWA_6"), ps_core)
com <- otu_table(ps_core)

# With section
hier_samp <- sample_data(ps_core) %>% select(sample, sample_num, site, section)
hier_samp <- data.frame(unclass(hier_samp), park="Kruger", stringsAsFactors=TRUE)
adi_shan_prop <- adipart(com, hier_samp, index="shannon", weights="prop", relative = FALSE, nsimul=99, method = "r2dtable")
