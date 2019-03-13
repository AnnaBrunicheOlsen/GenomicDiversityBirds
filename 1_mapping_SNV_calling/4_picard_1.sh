#!/bin/sh

#PBS -N picard_1
#PBS -q fnrquail
#PBS -l nodes=1:ppn=1,naccesspolicy=singleuser
#PBS -l walltime=300:00:00
#PBS -m abe

module purge
module load bioinfo
module load samtools
module load picard-tools

cd $PBS_O_WORKDIR

# remove old tmp including files
rm -rf tmp

# make tmp directory for the files
mkdir tmp

# make list with sra
tail -n +2 infile2.txt > sra.txt

cat sra.txt | while read -r LINE

do

# validate the SAM file should produce a validate_output.txt file that says there are no errors.
# samtools view -h -o aln.sam out.bam 
# one section for each SRA is needed. 
# Easiest to copy/paste and use find/replace to insert SRA numbers.
PicardCommandLine ValidateSamFile I=${LINE}.sam MODE=SUMMARY O=${LINE}_samfile.txt
PicardCommandLine SortSam SORT_ORDER=coordinate INPUT=${LINE}.sam OUTPUT=${LINE}.bam \
TMP_DIR=`pwd`/tmp VALIDATION_STRINGENCY=LENIENT

done


# END
