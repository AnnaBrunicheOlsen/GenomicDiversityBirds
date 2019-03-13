#!/bin/sh

#PBS -N scaf_dist
#PBS -q fnrdewoody
#PBS -l nodes=1:ppn=1,naccesspolicy=shared
#PBS -l walltime=1:00:00
#PBS -m abe

module purge
module load bioinfo

cd $PBS_O_WORKDIR

cat species_20x.txt | while read -r LINE

do

echo $LINE

cd $LINE

# take line two from "scaffold_distribution.txt" file
summary=$(sed '2q;d' scaffold_distribution.txt)
scafnr=$(wc -l ref.fa.fai)

echo -e "$PWD\t $summary\t $scafnr" >> /scratch/snyder/a/abruenic/scaffold_distributions.txt

cd ..

done

#END
