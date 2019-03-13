#!/bin/sh

#PBS -N move_bird_ROH
#PBS -q fnrdewoody
#PBS -l nodes=1:ppn=1,naccesspolicy=shared
#PBS -l walltime=300:00:00
#PBS -m abe

module purge
module load bioinfo

cd $PBS_O_WORKDIR

cat birds.txt | while read -r LINE

do

cp /scratch/snyder/k/kabts/Avian_ROHs_archived/Avian_ROHs/${LINE}/2017-08-21_${LINE}.tar.gz .

# untar
tar -xzf 2017-08-21_${LINE}.tar.gz

# remove tar file
rm -rf 2017-08-21_${LINE}.tar.gz

done

# END
