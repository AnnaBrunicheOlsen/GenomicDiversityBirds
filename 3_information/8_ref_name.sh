#!/bin/sh

#PBS -N make_map_file
#PBS -q fnrgenetics
#PBS -l nodes=1:ppn=1,naccesspolicy=shared
#PBS -l walltime=300:00:00
#PBS -m abe

module purge
module load bioinfo

cd $PBS_O_WORKDIR

cat species_20x.txt | while read -r LINE

do

echo $LINE

cd $LINE

# grep first line of ref.fa
name=$(grep -m1 "" ref.fa)

# print to file
echo -e "$PWD\t $name" >> /scratch/snyder/a/abruenic/ref_name.txt

cd ..

done

# END
