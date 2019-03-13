#!/bin/sh

#PBS -N picard_2
#PBS -q fnrgenetics
#PBS -l nodes=1:ppn=1,naccesspolicy=singleuser
#PBS -l walltime=300:00:00
#PBS -m abe

module purge
module load bioinfo
module load samtools
module load picard-tools

cd $PBS_O_WORKDIR

PicardCommandLine ValidateSamFile I=SRR1794255.sam MODE=SUMMARY O=SRR1794255_samfile.txt
PicardCommandLine SortSam SORT_ORDER=coordinate INPUT=SRR1794255.sam OUTPUT=SRR1794255.bam \
TMP_DIR=`pwd`/tmp VALIDATION_STRINGENCY=LENIENT

PicardCommandLine ValidateSamFile I=SRR1796636.sam MODE=SUMMARY O=SRR1796636_samfile.txt
PicardCommandLine SortSam SORT_ORDER=coordinate INPUT=SRR1796636.sam OUTPUT=SRR1796636.bam \
TMP_DIR=`pwd`/tmp VALIDATION_STRINGENCY=LENIENT

PicardCommandLine ValidateSamFile I=SRR1796638.sam MODE=SUMMARY O=SRR1796638_samfile.txt
PicardCommandLine SortSam SORT_ORDER=coordinate INPUT=SRR1796638.sam OUTPUT=SRR1796638.bam \
TMP_DIR=`pwd`/tmp VALIDATION_STRINGENCY=LENIENT

PicardCommandLine ValidateSamFile I=SRR1796641.sam MODE=SUMMARY O=SRR1796641_samfile.txt
PicardCommandLine SortSam SORT_ORDER=coordinate INPUT=SRR1796641.sam OUTPUT=SRR1796641.bam \
TMP_DIR=`pwd`/tmp VALIDATION_STRINGENCY=LENIENT

rm -rf *gz.1
rm -rf core*
rm -rf *.html
rm -rf *.zip
rm -rf *_fastqc
rm -rf 1_genome_sra_only.sh
rm -rf 2_BWA*
rm -rf guessout*
rm -rf temp_log.txt
rm -rf *.o*
rm -rf *.e*
rm -rf chrlist
rm -rf *_samfile.txt
rm -rf *collapsed*
rm -rf *singleton*
rm -rf *discarded*
rm -rf *settings

# merge bams
# list your SRA numbers in order to merge all bam files into one bam file.
samtools merge out.bam \
SRR1794255.bam \
SRR1796636.bam \
SRR1796638.bam \
SRR1796641.bam

# marking PCR duplicated reads without removing them
PicardCommandLine MarkDuplicates INPUT=out.bam OUTPUT=marked.bam M=metrics.txt

# check coverage
PicardCommandLine CollectWgsMetrics I=marked.bam O=coverage_marked.txt R=ref.fa 

PicardCommandLine BuildBamIndex INPUT=marked.bam

# create reference that reads can be mapped to. Will produce .fai file
samtools faidx ref.fa

PicardCommandLine CreateSequenceDictionary reference=ref.fa output=ref.dict

# END
