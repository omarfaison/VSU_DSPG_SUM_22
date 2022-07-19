##Created by Kenyaita M. Hodge 
##Emory University
##Introduction to publicly available data from GEO Accession "GSE137495"
##For The Data Science for the Public Good Young Scholars program
##July 19, 2022

###Install required packages
install.packages("car")
install.packages("data.table")
install.packages("dplyr")

#Load packages
library(car)
library(data.table)
library(dplyr)

###Set working directory where output files will go
setwd("C:/Users/khodge4/OneDrive - Emory University/Faison/Output")

###Load beta matrix and phenotype files
load("C:/Users/khodge4/OneDrive - Emory University/Faison/GSE137495_betas_phen.RData")
#load("C:/Users/kenya/Desktop/Everson Lab/GSE137495/b.m_04042020.csv")

###485K CpG probes: remove missing and zero values

load(file="C:/Users/khodge4/OneDrive - Emory University/Faison/ChenProbeIDs.RData")
annot2$SNPs = annot2[,"Global"] #### Use "Global" if population is very diverse
kept.probes = subset(annot2, as.character(Name) %in% rownames(b.m) & sex %in% "Keep" & CRSH %in% "Keep" & SNPs %in% "Keep")

table(ifelse(colnames(b.m)==as.character(pd.sub$ID2),"Matched","--NOT MATCHED--"))
# Drop problematic probes:
b.m = b.m[which(rownames(b.m) %in% as.character(kept.probes$Name)),]
dim(b.m)

## Drop non-CG probes:
b.m = b.m[which(substr(rownames(b.m),1,2) %in% "cg"),]

## Drop probes with missing values:
b.m = b.m[complete.cases(b.m), ]

###Physically removed samples w/ age >0.35 - kept getting error when I used script.
pheno = fread("C:/Users/khodge4/OneDrive - Emory University/Faison/pd.sub_04042020_new.csv", header = TRUE,data.table = F)

###Check age distribution (age is in months where most infants are around 3 months old or 3/12=0.25)
summary(pheno$Age)


###Annotation file: removed "chr" from variables chr and Islands_Name. This helped CpGAssoc run smoother.
annot3 = fread("C:/Users/khodge4/OneDrive - Emory University/Faison/Annotation_04042020_2.csv", header = TRUE)

###Make sure same subjects are in both methylation and phenotype datasets

b.m2 <- b.m[,colnames(b.m)%in%pheno$ID2]
pheno2 <- pheno[pheno$ID2%in%colnames(b.m2),]

pheno3 <- pheno2[order(pheno2$ID2),]
b.m3 <- b.m2[,order(colnames(b.m2))]
table(pheno3$ID2==colnames(b.m3))

###Get cell proportions
BiocManager::install("EpiDISH")
library("EpiDISH")
cellmix1 <- epidish(beta.m = b.m3, centEpiFibIC.m, method = 'RPC')$estF

cellmix2 = fread("C:/Users/khodge4/OneDrive - Emory University/Faison/CellMix_EpiPercent.csv")

###Package re-images dataframes
install.packages('tibble')
library(tibble)
cellmix3 <- cellmix2 %>%
  `row.names<-`(., NULL) %>% 
  column_to_rownames(var = "V1")

b.m4 <- b.m3[,colnames(b.m3)%in%pheno3$ID2]
pheno4<- pheno3[pheno3$ID2%in%colnames(b.m4),]

#### b.m4= methylation file, pheno4= phenotype file, annot3= new annotation file

####Install packages from Bioconductor needed for CpGassoc and surrogate variable analysis
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("CpGassoc")
library(CpGassoc)

BiocManager::install("sva")

lib = c("MASS","sva","limma");
lapply(lib, require, character.only = TRUE);

mod1<-model.matrix(~Age, data=pheno4)
mod01<-model.matrix(~1, data=pheno4)

###SKIP: This step takes long
#n.sv = num.sv(b.m4,mod1,method="be") ###calculating the number of significant surrogate variables
#svobj1= sva(as.matrix(b.m4),mod1,mod01,n.sv=n.sv) ###this step runs iterations of the data

###IMPORT SVs dataset
load(file="C:/Users/khodge4/OneDrive - Emory University/Faison/SVs.Rdata")

dim(SVs)

####Selected 5 surrogate variables for analysis
SVs_5 <- SVs[1:136,1:5]

###SKIP: Package was updated and generates errors due to R software update
#ewas<-cpg.assoc(b.m4, as.numeric(pheno4$Age),covariates=data.frame(SVs_5, pheno4$Sex, cellmix3$Epi))

###LOAD EWAS results
load(file="C:/Users/khodge4/OneDrive - Emory University/Faison/ExpressionQCtogetherAge_05052020_SVA_Sex_Celltype.Rdata")
ewas

#Extract results and coefficient objects from ewas list
results<-ewas$results
coef<-ewas$coefficients

###Combine datasets with annotation file
both<-merge(results,coef, by.x="CPG.Labels", by.y="row.names")
both1<-merge(both, annot3, by.x="CPG.Labels", by.y="Name")

###Generate and export QQ plot
pdf(width=8, height=8,family = "Times", file = "C:/Users/khodge4/OneDrive - Emory University/Faison/Output/QQplot_Age_SVA_Sex_Cell_annot.pdf")
op <- par(family = "Times")
plot(ewas, gcdisplay=T)
dev.off()

###Generate Manhattan plot
manhattan(ewas, annot3$Name, annot3$chr, annot3$pos, title=NULL,point.size=.6,file.type="pdf", save.plot="C:/Users/khodge4/OneDrive - Emory University/Faison/Output/Manhattan_Age_SVA_Sex_Celltype")

###Export results and check for significant CpGs (P.value and FDR) and UCSC_RefGene_Name
write.csv(both1,file="C:/Users/khodge4/OneDrive - Emory University/Faison/Output/ExpressionQC_Age_SVA_Sex_Cell_CORR_05052020_annot.csv")

###Generate and export scatterplots of significant CpGs
scatterplot(ewas, cpg.rank = NULL, cpg.name=c("cg00656420","cg01716122"), file.type="pdf", save.plot="Scatterplot_Age_5SVA_Sex_Celltype",pch=16, beta.values = b.m4)



