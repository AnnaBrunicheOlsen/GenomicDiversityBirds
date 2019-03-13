#!/bin/sh

#PBS -N map_ped
#PBS -q fnrdewoody
#PBS -l nodes=1:ppn=1,naccesspolicy=shared
#PBS -l walltime=5:00:00
#PBS -m abe

module purge
module load bioinfo

cd $PBS_O_WORKDIR

cat species_20x.txt | while read -r LINE

do

echo $LINE

cd $LINE

# check if test4.map file exists
nomap=$"no_map"
if [ -e test4.map ]
then
	echo "test4.map exists"
else
	echo "test4.map missing"
	echo -e "$PWD\t $nomap" >> /scratch/snyder/a/abruenic/zero_in_map_10x.txt
fi	

# check if there are zeros in first column in test4.map
zero=$(awk '$1 == 0' test4.map)

if [ -z "$zero" ]
then
	echo "no_zero_in_map"
else
	echo "zero_in_map"	
	echo -e "$PWD\t $zero" >> /scratch/snyder/a/abruenic/zero_in_map_10x.txt
fi

cd ..

done

#END
