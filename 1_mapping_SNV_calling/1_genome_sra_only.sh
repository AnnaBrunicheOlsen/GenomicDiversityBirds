#!/bin/sh

#PBS -N genome_sra
#PBS -q fnrquail
#PBS -l nodes=1:ppn=5,naccesspolicy=shared
#PBS -l walltime=300:00:00
#PBS -m abe

module purge
module load bioinfo
module load sra-toolkit
module load fastqc
module load python

cd $PBS_O_WORKDIR

sed -e 's/^M//g' infile.txt > infile2.txt

#### GENOME ####

# the first line in infile.txt should be the species name
# the next lines in infile.txt should be SRA

species=$(head -n 1 infile2.txt)
species=${species%$'\n'}


#### SRA ####

tail -n +2 infile2.txt | while read -r LINE

do

LINE=${LINE%$'\n'}

echo $LINE

acno=${LINE:3}
lng=${#acno}

first6=${LINE:0:6}

if [ $lng -lt 7 ]
then

wget ftp://ftp.sra.ebi.ac.uk/vol1/fastq/${first6}/${LINE}/*.fastq.gz

elif [ $lng -eq 7 ]
then

subdir=00${LINE:(-1)}

wget ftp://ftp.sra.ebi.ac.uk/vol1/fastq/${first6}/${subdir}/${LINE}/*.fastq.gz

elif [ $lng -eq 8 ]
then

subdir=0${LINE:(-2)}
wget ftp://ftp.sra.ebi.ac.uk/vol1/fastq/${first6}/${subdir}/${LINE}/*.fastq.gz

elif [ $lng -eq 9 ]
then

subdir=${LINE:(-3)}
wget ftp://ftp.sra.ebi.ac.uk/vol1/fastq/${first6}/${subdir}/${LINE}/*.fastq.gz

fi

# download by sra-toolkit and split in read1 and read2
#fastq-dump --gzip --split-3 $LINE

FILE1=${LINE}_1.fastq.gz
FILE2=${LINE}_2.fastq.gz

# get phred algorithm from one of this script
# https://github.com/brentp/bio-playground/blob/master/reads-utils/guess-encoding.py
zcat $FILE1 | awk 'NR % 4 == 0' | guess-encoding.py -n 1000 > guessout.txt

# insert result from previous step (sequencing platforms and associated PHRED scores) 
# in the next step under --qualitybase 
# default = 33
#'Sanger': (33, 73) = 33
#'Illumina-1.8': (33, 74) = 33
#'Solexa': (59, 104) = solexa
#'Illumina-1.3': (64, 104) = 64
#'Illumina-1.5': (67, 104) = 64

if grep -q 'Sanger' guessout.txt
then
    qualbase=33
elif grep -q 'Illumina-1.8' guessout.txt
then
    qualbase=33
elif grep -q 'Solexa' guessout.txt
then
    qualbase=solexa
elif grep -q 'Illumina-1.3' guessout.txt
then
    qualbase=64
elif grep -q 'Illumina-1.5' guessout.txt
then
    qualbase=64
else
    qualbase=33
fi

### NEW TEST

# QC individual files
fastqc $FILE1
fastqc $FILE2

unzip ${LINE}_1_fastqc.zip 
unzip ${LINE}_2_fastqc.zip

grep "Per base sequence quality" ${LINE}_1_fastqc/summary.txt >> temp_log.txt
grep "Adapter Content" ${LINE}_1_fastqc/summary.txt >> temp_log.txt

grep "Per base sequence quality" ${LINE}_2_fastqc/summary.txt >> temp_log.txt
grep "Adapter Content" ${LINE}_2_fastqc/summary.txt >> temp_log.txt


if [ $qualbase -ne 33 ] || grep -q 'FAIL' temp_log.txt

then

# AdaptorRemoval
# trimming + output Phred 33 score
# https://github.com/MikkelSchubert/adapterremoval
AdapterRemoval --file1 $FILE1 --file2 $FILE2 --threads 5 \
--basename $LINE --trimns --trimqualities --collapse \
--qualitybase $qualbase --gzip --qualitybase-output 33 --minquality 5

#Rename trimmed files
mv ${LINE}.pair1.truncated.gz ${LINE}_1.truncated.gz
mv ${LINE}.pair2.truncated.gz ${LINE}_2.truncated.gz

# QC individual files
fastqc ${LINE}_1.truncated.gz
fastqc ${LINE}_2.truncated.gz

unzip ${LINE}_1.truncated_fastqc.zip
unzip ${LINE}_2.truncated_fastqc.zip

grep "Per base sequence quality" ${LINE}_1.truncated_fastqc/summary.txt >> qc_log.txt
grep "Adapter Content" ${LINE}_1.truncated_fastqc/summary.txt >> qc_log.txt

grep "Per base sequence quality" ${LINE}_2.truncated_fastqc/summary.txt >> qc_log.txt
grep "Adapter Content" ${LINE}_2.truncated_fastqc/summary.txt >> qc_log.txt

rm -r ${LINE}_1.truncated_fastqc
rm -r ${LINE}_2.truncated_fastqc

else

cat temp_log.txt >> qc_log.txt 

fi

done

#Check if any QC checks failed
if grep -q 'FAIL' qc_log.txt
then
    echo "Warning: QC check failed"
fi

# END
