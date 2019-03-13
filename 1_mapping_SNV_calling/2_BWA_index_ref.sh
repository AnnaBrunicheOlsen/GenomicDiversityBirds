#!/bin/sh

#PBS -N BWA_index_ref
#PBS -q fnrquail
#PBS -l nodes=1:ppn=1,naccesspolicy=shared
#PBS -l walltime=300:00:00
#PBS -m abe

module purge
module load bioinfo
module load bwa
module load samtools
module load picard-tools

cd $PBS_O_WORKDIR

cp ref.fa ref_ORIGINAL.fa

rm -rf ref.fa.*
rm -rf ref.dict

# index reference and map reads
bwa index -a bwtsw ref.fa

# create reference that reads can be mapped to. Will produce .fai file
samtools faidx ref.fa

PicardCommandLine CreateSequenceDictionary reference=ref.fa output=ref.dict

# END
