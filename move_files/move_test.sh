#!/bin/sh

#PBS -N move_bird_ROH
#PBS -q standby
#PBS -l nodes=1:ppn=1,naccesspolicy=shared
#PBS -l walltime=4:00:00
#PBS -m abe

module purge
module load bioinfo

cd $PBS_O_WORKDIR

cp /scratch/snyder/k/kabts/Avian_ROHs_archived/Avian_ROHs/Acanthisitta_chloris/2017-08-21_Acanthisitta_chloris.tar.gz .

# untar
tar -xzf 2017-08-21_Acanthisitta_chloris.tar.gz

# END
