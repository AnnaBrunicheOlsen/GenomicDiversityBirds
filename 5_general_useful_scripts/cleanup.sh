#!/bin/sh

#PBS -N cleanup
#PBS -q standby
#PBS -l nodes=1:ppn=1,naccesspolicy=shared
#PBS -l walltime=4:00:00
#PBS -m abe

module purge
module load bioinfo

cd $PBS_O_WORKDIR

masterfile=mate_pair_test.csv

ncol=$(head -1 $masterfile | sed 's/[^,]//g' | wc -c)

for i in $(seq 1 $ncol)

do
   
    cut -d',' -f ${i} $masterfile | awk 'NF' > cleanup.txt

    species=$(head -n 1 cleanup.txt)

    mv cleanup.txt ${species}/cleanup.txt

    cd $species

    tail -n +2 cleanup.txt | while read -r LINE

    do

    LINE=${LINE%$'\n'}

    echo ${LINE}*

    rm -r ${LINE}*

    done

    rm cleanup.txt

    cd ..

done

# END