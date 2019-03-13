#!/bin/sh

#PBS -N check_file_exist
#PBS -q standby
#PBS -l nodes=1:ppn=1
#PBS -l walltime=4:00:00
#PBS -m abe

module purge
module load bioinfo

cd $PBS_O_WORKDIR

cat birds.txt | while read -r LINE

do

echo $LINE

cd $LINE

# check that marked.bam exist 
if [ -s "marked.bam" ]
then 
   echo " marked.bam exists and is not empty "
else
   echo " marked.bam does not exist, or is empty "
fi

# check that realigned_reads.bam exist
if [ -s "realigned_reads.bam" ]
then 
   echo " realigned_reads.bam exists and is not empty "
else
   echo " realigned_reads.bam does not exist, or is empty "
fi

# check that vcf exist
if [ -s "NO_QUAL_variants.g.vcf" ]
then 
   echo " NO_QUAL_variants.g.vcf exists and is not empty "
else
   echo " NO_QUAL_variants.g.vcf does not exist, or is empty "
fi

cd ..

done

# END
