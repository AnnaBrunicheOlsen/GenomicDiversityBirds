#!/bin/sh

#PBS -N contig_number
#PBS -q standby
#PBS -l nodes=1:ppn=1,naccesspolicy=shared
#PBS -l walltime=0:30:00
#PBS -m abe

module purge
module load bioinfo

cd $PBS_O_WORKDIR

cat species_20x.txt | while read -r LINE

do

echo $LINE

cd $LINE

# grep line 20 from *log file
file=$(sed -n '20p' SNPs_20x_auto.log)
echo -e "$PWD\t $file" >> /scratch/snyder/a/abruenic/contig_number.txt

cd ..

done

# END
