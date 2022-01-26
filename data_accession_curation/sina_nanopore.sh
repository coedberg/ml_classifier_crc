#!/bin/bash

module load Sina/1.6.0-rc.1-foss-2018a

#Define variables
cd /srv/MA/users/alau15/Nanopore/edit/
queryOut=/srv/MA/users/alau15/Nanopore/sina/
max_threads=100
database=/space/databases/SILVA/LTPs132_SSU.arb
files=$(ls /srv/MA/users/alau15/Nanopore/edit/*.fasta | cat)


for q in $files
do

  sina \
    -i "$q" \
    --db "$database" \
    --search \
    --search-db "$database" \
    --meta-fmt CSV \
    -o "${queryOut}/$(basename $q)_aligned.fasta" \
    --lca-fields tax_slv
  
  #remove all 5S, 5.8S, 23S, or 28S hits
  sed -i '/^\(5S\|5.8S\|23S\|28S\)/d' "${queryOut}/$(basename $q)_aligned.fasta"
  
  #only keep columns: name, align_quality_slv, lca_tax_slv
  #cut -d, -f1,5,7 ${queryOut}/${queryName}_bac_rRNA_aligned.csv > ${queryOut}/${queryName}_bac_rRNA_aligned_filtered.csv
done
