#!/bin/sh

#PBS -N GATK_nt1
#PBS -q fnrdewoody
#PBS -l nodes=1:ppn=1,naccesspolicy=shared
#PBS -l walltime=300:00:00
#PBS -m abe

module purge
module load bioinfo
module load GATK

cd $PBS_O_WORKDIR

cat species_20x.txt | while read -r LINE

do

echo $LINE

cd $LINE

# Run SelectVariants 
# coverage needs to be included we chose 20x as minimum
GenomeAnalysisTK -T SelectVariants -R ref.fa -V NO_QUAL_variants.g.vcf \
-selectType SNP -select "DP>=10" -o SNPs_10x.vcf

cd ..

done

# END

