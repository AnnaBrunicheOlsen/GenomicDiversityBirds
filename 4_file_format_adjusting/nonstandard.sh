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

grep nonstandard  SNPs_10x_auto.log > nonstandard.txt
nonstandard=$(grep nonstandard  SNPs_10x_auto.log)

# check if nonstandard variable is not empty
if [ -z "$nonstandard" ]

then 
	echo "empty"
else
	echo -e "$PWD\t $summary\t $nonstandard" >> /scratch/snyder/a/abruenic/nonstandard_10x.txt
fi

cd ..

done

#END
