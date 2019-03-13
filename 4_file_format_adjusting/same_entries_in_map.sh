#!/bin/sh

#PBS -N same_entries_in_map
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

low=$(awk '{print NF}' test4.map | sort -nu | head -n 1)
high=$(awk '{print NF}' test4.map | sort -nu | tail -n 1)

echo -e "$PWD\t $low\t $high" >> /scratch/snyder/a/abruenic/columns_in_map?10x.txt

cd ..

done

#END
