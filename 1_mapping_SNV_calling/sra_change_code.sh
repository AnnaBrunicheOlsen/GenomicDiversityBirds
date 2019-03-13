#!/bin/sh

#PBS -N sra_code_change
#PBS -q standby
#PBS -l nodes=1:ppn=1,naccesspolicy=shared
#PBS -l walltime=4:00:00
#PBS -m abe

module purge
module load bioinfo

cd $PBS_O_WORKDIR

cat birds.txt | while read -r LINE

do

cd $LINE

# run on fnrquail
sed -i -e 's/fnrdewoody/fnrquail/g' 4_picard*sh

# change all $sra to ${sra}
sed -i -e 's/$sra1/${sra1}/g' 4_picard*sh
sed -i -e 's/$sra2/${sra2}/g' 4_picard*sh
sed -i -e 's/$sra3/${sra3}/g' 4_picard*sh
sed -i -e 's/$sra4/${sra4}/g' 4_picard*sh
sed -i -e 's/$sra5/${sra5}/g' 4_picard*sh
sed -i -e 's/$sra6/${sra6}/g' 4_picard*sh
sed -i -e 's/$sra7/${sra7}/g' 4_picard*sh
sed -i -e 's/$sra8/${sra8}/g' 4_picard*sh
sed -i -e 's/$sra9/${sra9}/g' 4_picard*sh
sed -i -e 's/$sra10/${sra10}/g' 4_picard*sh
sed -i -e 's/$sra11/${sra11}/g' 4_picard*sh

# comment out parts of the script that has already been run

sed -i -e 's/PicardCommandLine ValidateSamFile/#PicardCommandLine ValidateSamFile/g' 4_picard*sh
sed -i -e 's/PicardCommandLine SortSam/#PicardCommandLine SortSam/g' 4_picard*sh
sed -i -e 's/TMP/#TMP/g' 4_picard*sh

# put space after bams
sed -i -e "s/bam/bam /g" 4_picard*sh
sed -i -e "s/   / /g" 4_picard*sh
sed -i -e "s/  / /g" 4_picard*sh

qsub 4_picard*sh

cd ..

done

# END
