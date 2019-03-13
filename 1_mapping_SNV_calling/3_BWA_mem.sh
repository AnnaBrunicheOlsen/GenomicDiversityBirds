#!/bin/sh

#PBS -N BWA_mem
#PBS -q fnrdewoody
#PBS -l nodes=1:ppn=5,naccesspolicy=shared
#PBS -l walltime=300:00:00
#PBS -m abe

module load bioinfo
module load bwa

cd $PBS_O_WORKDIR

sed -i -e 's/ /_/g' ref.fa
sed -i -e 's/|/_/g' ref.fa

# make list with sra
tail -n +2 infile2.txt > sra.txt

cat sra.txt | while read -r LINE

do

if [ -f ${LINE}_1.truncated.gz]
then
# truncated
bwa mem -t 5 -M -R "@RG\tID:group1\tSM:sample1\tPL:illumina\tLB:lib1\tPU:unit1" ref.fa ${LINE}_1.truncated.gz ${LINE}_2.truncated.gz > ${LINE}.sam

else

# fastq
bwa mem -t 5 -M -R "@RG\tID:group1\tSM:sample1\tPL:illumina\tLB:lib1\tPU:unit1" ref.fa ${LINE}_1.fastq.gz ${LINE}_2.fastq.gz > ${LINE}.sam

fi

done

# END
