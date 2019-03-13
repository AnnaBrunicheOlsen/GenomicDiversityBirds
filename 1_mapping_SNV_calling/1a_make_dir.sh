#!/bin/sh

#PBS -N make_dir
#PBS -q fnrgenetics
#PBS -l nodes=1:ppn=1,naccesspolicy=shared
#PBS -l walltime=1:00:00
#PBS -m abe

module purge
module load bioinfo

cd $PBS_O_WORKDIR

masterfile=species.csv

ncol=$(head -1 $masterfile | sed 's/[^,]//g' | wc -c)

for i in $(seq 1 $ncol)

do
   
    cut -d',' -f ${i} $masterfile | awk 'NF' > temp.txt

    species=$(head -n 1 temp.txt)

    mkdir $species

    mv temp.txt ${species}/infile.txt

    cp 1_genome_sra.sh ${species}/1_genome_sra.sh

    cp 2_BWA_index_ref.sh ${species}/2_BWA_index_ref.sh

    cp 3_BWAmem.sh ${species}/3_BWAmem.sh

    cp 4_picard.sh ${species}/4_picard.sh

    cp 5_GATK_nt20.sh ${species}/5_GATK_nt20.sh

    cp 6_GATK_nt1.sh ${species}/6_GATK_nt1.sh

done

# END