#!/bin/sh

#PBS -N picard
#PBS -q fnrdewoody
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

#remianing SRA   
sra1=$"SRR862083"
sra2=$"SRR863616"
sra3=$"SRR863646"
sra4=$"SRR866511"
sra5=$"SRR866512"
sra6=$"SRR866513"
sra7=$"SRR866514"
sra8=$"SRR866515"
sra9=$"SRR866516"
sra10=$"SRR866517"
sra11=$"SRR866518"
sra12=$"SRR866520"
sra13=$"SRR866595"
sra14=$"SRR866596"
sra15=$"SRR866597"
sra16=$"SRR866598"
sra17=$"SRR866599"
sra18=$"SRR866601"
sra19=$"SRR866603"
sra20=$"SRR866903"
sra21=$"SRR867100"
sra22=$"SRR867101"
sra23=$"SRR867102"
#sra=$"SRR867103"
#sra=$"SRR867104"
#sra=$"SRR867105"
#sra=$"SRR867107"
#sra=$"SRR867108"
#sra=$"SRR867110"
#sra=$"SRR867112"
#sra=$"SRR867113"
#sra=$"SRR867114"
#sra=$"SRR867115"
#sra=$"SRR867116"
#sra=$"SRR867117"
#sra=$"SRR867119"


# validate the SAM file should produce a validate_output.txt file that says there are no errors.
# samtools view -h -o aln.sam out.bam 
# one section for each SRA is needed. 
# Easiest to copy/paste and use find/replace to insert SRA numbers.
PicardCommandLine ValidateSamFile I=$sra1.sam MODE=SUMMARY O=$sra1_samfile.txt
PicardCommandLine SortSam SORT_ORDER=coordinate INPUT=$sra1.sam OUTPUT=$sra1.bam \
TMP_DIR=`pwd`/tmp VALIDATION_STRINGENCY=LENIENT

PicardCommandLine ValidateSamFile I=$sra2.sam MODE=SUMMARY O=$sra2_samfile.txt
PicardCommandLine SortSam SORT_ORDER=coordinate INPUT=$sra2.sam OUTPUT=$sra2.bam \
TMP_DIR=`pwd`/tmp VALIDATION_STRINGENCY=LENIENT

PicardCommandLine ValidateSamFile I=$sra3.sam MODE=SUMMARY O=$sra3_samfile.txt
PicardCommandLine SortSam SORT_ORDER=coordinate INPUT=$sra3.sam OUTPUT=$sra3.bam \
TMP_DIR=`pwd`/tmp VALIDATION_STRINGENCY=LENIENT

PicardCommandLine ValidateSamFile I=$sra4.sam MODE=SUMMARY O=$sra4_samfile.txt
PicardCommandLine SortSam SORT_ORDER=coordinate INPUT=$sra4.sam OUTPUT=$sra4.bam \
TMP_DIR=`pwd`/tmp VALIDATION_STRINGENCY=LENIENT

PicardCommandLine ValidateSamFile I=$sra5.sam MODE=SUMMARY O=$sra5_samfile.txt
PicardCommandLine SortSam SORT_ORDER=coordinate INPUT=$sra5.sam OUTPUT=$sra5.bam \
TMP_DIR=`pwd`/tmp VALIDATION_STRINGENCY=LENIENT

PicardCommandLine ValidateSamFile I=$sra6.sam MODE=SUMMARY O=$sra6_samfile.txt
PicardCommandLine SortSam SORT_ORDER=coordinate INPUT=$sra6.sam OUTPUT=$sra6.bam \
TMP_DIR=`pwd`/tmp VALIDATION_STRINGENCY=LENIENT

PicardCommandLine ValidateSamFile I=$sra7.sam MODE=SUMMARY O=$sra7_samfile.txt
PicardCommandLine SortSam SORT_ORDER=coordinate INPUT=$sra7.sam OUTPUT=$sra7.bam \
TMP_DIR=`pwd`/tmp VALIDATION_STRINGENCY=LENIENT

PicardCommandLine ValidateSamFile I=$sra8.sam MODE=SUMMARY O=$sra8_samfile.txt
PicardCommandLine SortSam SORT_ORDER=coordinate INPUT=$sra8.sam OUTPUT=$sra8.bam \
TMP_DIR=`pwd`/tmp VALIDATION_STRINGENCY=LENIENT

PicardCommandLine ValidateSamFile I=$sra9.sam MODE=SUMMARY O=$sra9_samfile.txt
PicardCommandLine SortSam SORT_ORDER=coordinate INPUT=$sra9.sam OUTPUT=$sra9.bam \
TMP_DIR=`pwd`/tmp VALIDATION_STRINGENCY=LENIENT

PicardCommandLine ValidateSamFile I=$sra10.sam MODE=SUMMARY O=$sra10_samfile.txt
PicardCommandLine SortSam SORT_ORDER=coordinate INPUT=$sra10.sam OUTPUT=$sra10.bam \
TMP_DIR=`pwd`/tmp VALIDATION_STRINGENCY=LENIENT

PicardCommandLine ValidateSamFile I=$sra11.sam MODE=SUMMARY O=$sra11_samfile.txt
PicardCommandLine SortSam SORT_ORDER=coordinate INPUT=$sra11.sam OUTPUT=$sra11.bam \
TMP_DIR=`pwd`/tmp VALIDATION_STRINGENCY=LENIENT

PicardCommandLine ValidateSamFile I=$sra12.sam MODE=SUMMARY O=$sra12_samfile.txt
PicardCommandLine SortSam SORT_ORDER=coordinate INPUT=$sra12.sam OUTPUT=$sra12.bam \
TMP_DIR=`pwd`/tmp VALIDATION_STRINGENCY=LENIENT

PicardCommandLine ValidateSamFile I=$sra13.sam MODE=SUMMARY O=$sra13_samfile.txt
PicardCommandLine SortSam SORT_ORDER=coordinate INPUT=$sra13.sam OUTPUT=$sra13.bam \
TMP_DIR=`pwd`/tmp VALIDATION_STRINGENCY=LENIENT

PicardCommandLine ValidateSamFile I=$sra14.sam MODE=SUMMARY O=$sra14_samfile.txt
PicardCommandLine SortSam SORT_ORDER=coordinate INPUT=$sra14.sam OUTPUT=$sra14.bam \
TMP_DIR=`pwd`/tmp VALIDATION_STRINGENCY=LENIENT

PicardCommandLine ValidateSamFile I=$sra15.sam MODE=SUMMARY O=$sra15_samfile.txt
PicardCommandLine SortSam SORT_ORDER=coordinate INPUT=$sra15.sam OUTPUT=$sra15.bam \
TMP_DIR=`pwd`/tmp VALIDATION_STRINGENCY=LENIENT

PicardCommandLine ValidateSamFile I=$sra16.sam MODE=SUMMARY O=$sra16_samfile.txt
PicardCommandLine SortSam SORT_ORDER=coordinate INPUT=$sra16.sam OUTPUT=$sra16.bam \
TMP_DIR=`pwd`/tmp VALIDATION_STRINGENCY=LENIENT

PicardCommandLine ValidateSamFile I=$sra17.sam MODE=SUMMARY O=$sra17_samfile.txt
PicardCommandLine SortSam SORT_ORDER=coordinate INPUT=$sra17.sam OUTPUT=$sra17.bam \
TMP_DIR=`pwd`/tmp VALIDATION_STRINGENCY=LENIENT

PicardCommandLine ValidateSamFile I=$sra18.sam MODE=SUMMARY O=$sra18_samfile.txt
PicardCommandLine SortSam SORT_ORDER=coordinate INPUT=$sra18.sam OUTPUT=$sra18.bam \
TMP_DIR=`pwd`/tmp VALIDATION_STRINGENCY=LENIENT

PicardCommandLine ValidateSamFile I=$sra19.sam MODE=SUMMARY O=$sra19_samfile.txt
PicardCommandLine SortSam SORT_ORDER=coordinate INPUT=$sra19.sam OUTPUT=$sra19.bam \
TMP_DIR=`pwd`/tmp VALIDATION_STRINGENCY=LENIENT

PicardCommandLine ValidateSamFile I=$sra20.sam MODE=SUMMARY O=$sra20_samfile.txt
PicardCommandLine SortSam SORT_ORDER=coordinate INPUT=$sra20.sam OUTPUT=$sra20.bam \
TMP_DIR=`pwd`/tmp VALIDATION_STRINGENCY=LENIENT

PicardCommandLine ValidateSamFile I=$sra21.sam MODE=SUMMARY O=$sra21_samfile.txt
PicardCommandLine SortSam SORT_ORDER=coordinate INPUT=$sra21.sam OUTPUT=$sra21.bam \
TMP_DIR=`pwd`/tmp VALIDATION_STRINGENCY=LENIENT

PicardCommandLine ValidateSamFile I=$sra22.sam MODE=SUMMARY O=$sra22_samfile.txt
PicardCommandLine SortSam SORT_ORDER=coordinate INPUT=$sra22.sam OUTPUT=$sra22.bam \
TMP_DIR=`pwd`/tmp VALIDATION_STRINGENCY=LENIENT

PicardCommandLine ValidateSamFile I=$sra23.sam MODE=SUMMARY O=$sra23_samfile.txt
PicardCommandLine SortSam SORT_ORDER=coordinate INPUT=$sra23.sam OUTPUT=$sra23.bam \
TMP_DIR=`pwd`/tmp VALIDATION_STRINGENCY=LENIENT


# merge bams
# list your SRA numbers in order to merge all bam files into one bam file.
samtools merge out.bam \
$sra1.bam \
$sra2.bam \
$sra3.bam \
$sra4.bam \
$sra5.bam \
$sra6.bam \
$sra7.bam \
$sra8.bam \
$sra9.bam \
$sra10.bam \
$sra11.bam \
$sra12.bam \
$sra13.bam \
$sra14.bam \
$sra15.bam \
$sra16.bam \
$sra17.bam \
$sra18.bam \
$sra19.bam \
$sra20.bam \
$sra21.bam \
$sra22.bam \
$sra23.bam \
SRR866520.bam \
SRR866521.bam \
SRR866523.bam \
SRR866524.bam \
SRR866525.bam \
SRR866526.bam \
SRR866527.bam \
SRR866528.bam \
SRR866593.bam \
SRR866594.bam

# marking PCR duplicated reads without removing them
PicardCommandLine MarkDuplicates INPUT=out.bam OUTPUT=marked.bam M=metrics.txt

# check coverage
PicardCommandLine CollectWgsMetrics I=marked.bam O=coverage_marked.txt R=ref.fa 

PicardCommandLine BuildBamIndex INPUT=marked.bam

# create reference that reads can be mapped to. Will produce .fai file
samtools faidx ref.fa

PicardCommandLine CreateSequenceDictionary reference=ref.fa output=ref.dict

# END
