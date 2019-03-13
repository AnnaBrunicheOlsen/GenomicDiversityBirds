#!/bin/sh

#PBS -N SRA_file_list
#PBS -q standby
#PBS -l nodes=1:ppn=1,naccesspolicy=shared
#PBS -l walltime=4:00:00
#PBS -m abe

module purge
module load bioinfo

cd $PBS_O_WORKDIR

# list with species we want SRA codes for
cat species_20x.txt | while read -r LINE

do

echo $LINE

cd $LINE

# for each species make a list of SRA codes used in the out.bam file
find . -name '*RR*.bam' > bam_list.txt
bam="$(tr '\n' '\t' < bam_list.txt)"

# get genome version
genome="$(grep -m1 '>' ref.fa)"

# print to file
echo -e "$PWD\t $genome\t $bam" >> /scratch/snyder/a/abruenic/SRA_genome_list.txt

cd ..

done

# END
