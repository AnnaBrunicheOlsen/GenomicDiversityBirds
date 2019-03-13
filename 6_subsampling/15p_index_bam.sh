#!/bin/sh

#PBS -N picard
#PBS -q fnrdewoody
#PBS -l nodes=1:ppn=1,naccesspolicy=shared
#PBS -l walltime=300:00:00
#PBS -m abe

module purge
module load bioinfo
module load picard-tools

cd $PBS_O_WORKDIR


# marking PCR duplicated reads without removing them
PicardCommandLine MarkDuplicates INPUT=15p.bam OUTPUT=marked.bam M=metrics.txt

# check coverage
PicardCommandLine CollectWgsMetrics I=marked.bam O=coverage_marked.txt R=ref.fa 

PicardCommandLine BuildBamIndex INPUT=marked.bam

PicardCommandLine CreateSequenceDictionary reference=ref.fa output=ref.dict

# END
