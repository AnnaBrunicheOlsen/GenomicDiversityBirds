#!/bin/sh

#PBS -N GATK_nt1
#PBS -q standby
#PBS -l nodes=1:ppn=1,naccesspolicy=shared
#PBS -l walltime=4:00:00
#PBS -m abe

module purge
module load bioinfo
module load GATK/3.8.1

cd $PBS_O_WORKDIR

# Run SelectVariants 
# coverage needs to be included we chose 15x as minimum
GenomeAnalysisTK -T SelectVariants -R ref.fa -V NO_QUAL_variants.g.vcf \
-selectType SNP -select "DP>=15" -o SNPs_15x.vcf

# END

