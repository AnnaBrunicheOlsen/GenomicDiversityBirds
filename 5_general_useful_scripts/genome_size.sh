#!/bin/sh

#PBS -N genomesize
#PBS -q fnrdewoody
#PBS -l nodes=1:ppn=1,naccesspolicy=shared
#PBS -l walltime=300:00:00
#PBS -m abe

#module purge
#module load bioinfo

cd $PBS_O_WORKDIR

# file with all species listed - species.txt
species=$(head -n 1 species.txt)
species=${species%$'\n'}

tail -n +2 species.txt | while read -r LINE

do

LINE=${LINE%$'\n'}

#echo $LINE

cd $LINE

ln="$(cut -f 2 ref.fa.fai | paste -sd+ | bc)"

cd ..

echo -e "$LINE \t $ln\n" >> genome_size.txt

done

# END
