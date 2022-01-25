#!/bin/bash

#Set directory
cd /srv/MA/users/alau15/baxter/

##################################################################################

#The SRA Toolkit, and the source-code SRA System Development
      #Kit (SDK), will allow you to programmatically access data
      #housed within SRA and convert it from the SRA format
      
      #More information
      #================
      # - Homepage: http://www.ncbi.nlm.nih.gov/Traces/sra/sra.cgi?view=software

##################################################################################
#Module load

module load SRA-Toolkit/2.10.8-foss-2018a-ubuntu64

##################################################################################
#SRA Run Selector: https://trace.ncbi.nlm.nih.gov/Traces/study/?acc=SRP062005&o=acc_s%3Aa
# - download the accecssion list and make sure it is in the cd with the name SRR_Acc_List.txt

#downloads the SRA files
#convert the SRA files into PE fastq files 
#Usage: fasterq-dump [ options ] [ accessions(s)... ]


prefetch --option-file SRR_Acc_List.txt #WORKS

cd /srv/MA/users/alau15/baxter/SRA/sra/

mkdir /srv/MA/users/alau15/baxter/fastq

for i in *.sra; do fasterq-dump --split-files ${i} -O /srv/MA/users/alau15/baxter/fastq; done

#cat SRR_Acc_List.txt | xargs fasterq-dump "/srv/MA/users/alau15/baxter/SRA/sra/"  --outdir "/srv/MA/users/alau15/baxter/fastq" #VIRKER IKKE

exit


