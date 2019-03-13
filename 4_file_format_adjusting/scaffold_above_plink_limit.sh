#!/bin/sh

#PBS -N scaf
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

# print header linie fra ny map fil
# count number of scaffolds to see if there are more than plink can handle
scaf=$(wc -l test2.map)

echo -e "$PWD\t $scaf" >> /scratch/snyder/a/abruenic/divide_scaffolds.txt

fi

cd ..

# END
