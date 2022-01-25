#!/bin/bash

module load Mothur/1.42.0-foss-2018a-Python-2.7.14

./mothur 

"#make.file(inputdir=/srv/MA/users/alau15/Illumina, type=fastq, prefix=stability); 
make.contigs(file=stability.files, processors=64); 
summary.seqs(fasta=stability.trim.contigs.fasta)"
#screen.seqs(fasta=current, maxambig=0, maxlength=275); 
#unique.seqs(); 
#count.seqs(name=current, group=current);
#summary.seqs(count=current);
#pcr.seqs(fasta=LTPs132_SSU_aligned.fasta, start=13000, end=25000, keepdots=F) 
#align.seqs(fasta=current, reference=/srv/MA/users/alau15/baxter/fastq/LTPs132_SSU_aligned.fasta);
#summary.seqs(fasta=current, count=current)
#screen.seqs(fasta=current, count=current, start=1300, end=25000, maxhomop=8); 
#pre.cluster(fasta=stability.trim.contigs.good.unique.good.filter.unique.fasta, count=stability.trim.contigs.good.unique.good.filter.count_table, diffs=2);
#chimera.vsearch(fasta=stability.trim.contigs.good.unique.good.filter.unique.precluster.fasta, count=stability.trim.contigs.good.unique.good.filter.unique.precluster.count_table, dereplicate=t);
#remove.seqs(fasta=stability.trim.contigs.good.unique.good.filter.unique.precluster.fasta, accnos=stability.trim.contigs.good.unique.good.filter.unique.precluster.denovo.vsearch.accnos);
#classify.seqs(fasta=stability.trim.contigs.good.unique.good.filter.unique.precluster.pick.fasta, count=stability.trim.contigs.good.unique.good.filter.unique.precluster.denovo.vsearch.pick.count_table, reference=/srv/MA/users/alau15/baxter/fastq/trainset18_062020.pds/trainset18_062020.pds.fasta, taxonomy=/srv/MA/users/alau15/baxter/fastq/trainset18_062020.pds/trainset18_062020.pds.tax, cutoff=80);
#remove.lineage(fasta=stability.trim.contigs.good.unique.good.filter.unique.precluster.pick.fasta, count=stability.trim.contigs.good.unique.good.filter.unique.precluster.denovo.vsearch.pick.count_table, taxonomy=stability.trim.contigs.good.unique.good.filter.unique.precluster.pick.pds.wang.taxonomy, taxon=Chloroplast-Mitochondria-unknown-Archaea-Eukaryota);
#cluster.split(fasta=current, count=current, taxonomy=current, splitmethod=classify, taxlevel=4, cutoff=0.03);
#make.shared(list=current, count=current, label=0.03);
#classify.otu(list=current, count=current, taxonomy=current, label=0.03)"


