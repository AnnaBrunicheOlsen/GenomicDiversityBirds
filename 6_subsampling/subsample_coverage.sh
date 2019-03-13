#!/bin/sh

#PBS -N Eptesicus_fuscus
#PBS -q fnrgenetics
#PBS -l nodes=1:ppn=20,naccesspolicy=shared
#PBS -l walltime=300:00:00
#PBS -m abe

module purge
module load bioinfo
module load GATK
module load samtools

cd $PBS_O_WORKDIR

# Species: Eptesicus_fuscus
# mean coverage 66x

############################


# subsample ~33x (50%)
samtools view -s 0.5 -b realigned_reads.bam > 50p.bam

# Run HaplotypeCaller
GenomeAnalysisTK -T HaplotypeCaller -R ref.fa -I 50p.bam -nct 20 \
--genotyping_mode DISCOVERY -stand_call_conf 0 --min_base_quality_score 20 \
--min_mapping_quality_score 20 -o 50p_variants.g.vcf

# all SNPs
GenomeAnalysisTK -T SelectVariants -R ref.fa -V 50p_variants.g.vcf \
-selectType SNP -o 50p_SNPs.vcf

# make file with all SNPs
grep 'GT:AD:DP:GQ:PL' 50p_SNPs.vcf > 50p_SNPs.txt
50p_SNP="$(wc -l < 50p_SNPs.txt)"

# coverage >=15x
GenomeAnalysisTK -T SelectVariants -R ref.fa -V 50p_variants.g.vcf \
-selectType SNP -select "DP>=15" -o 50p_SNPs_15x.vcf

# make file with all SNPs
grep 'GT:AD:DP:GQ:PL' 50p_SNPs_15x.vcf > 50p_SNPs_15x.txt
50p_SNP_15x="$(wc -l < 50p_SNPs_15x.txt)"


############################


# subsample ~15x (23%)
samtools view -s 0.5 -b realigned_reads.bam > 23p.bam

# Run HaplotypeCaller
GenomeAnalysisTK -T HaplotypeCaller -R ref.fa -I 23p.bam -nct 20 \
--genotyping_mode DISCOVERY -stand_call_conf 0 --min_base_quality_score 20 \
--min_mapping_quality_score 20 -o 23p_variants.g.vcf

# all SNPs
GenomeAnalysisTK -T SelectVariants -R ref.fa -V 23p_variants.g.vcf \
-selectType SNP -o 23p_SNPs.vcf

# make file with all SNPs
grep 'GT:AD:DP:GQ:PL' 23p_SNPs.vcf > 23p_SNPs.txt
23p_SNP="$(wc -l < 23p_SNPs.txt)"

# coverage >=15x
GenomeAnalysisTK -T SelectVariants -R ref.fa -V 23p_variants.g.vcf \
-selectType SNP -select "DP>=15" -o 23p_SNPs_15x.vcf

# make file with all SNPs
grep 'GT:AD:DP:GQ:PL' 23P_SNPs_15x.vcf > 23p_SNPs_15x.txt
23p_SNP_15x="$(wc -l < 23p_SNPs_15x.txt)"


############################


# subsample ~10x (15%)
samtools view -s 0.5 -b realigned_reads.bam > 15p.bam

# Run HaplotypeCaller
GenomeAnalysisTK -T HaplotypeCaller -R ref.fa -I 15p.bam -nct 20 \
--genotyping_mode DISCOVERY -stand_call_conf 0 --min_base_quality_score 20 \
--min_mapping_quality_score 20 -o 15p_variants.g.vcf

# all SNPs
GenomeAnalysisTK -T SelectVariants -R ref.fa -V 15p_variants.g.vcf \
-selectType SNP -o 15p_SNPs.vcf

# make file with all SNPs
grep 'GT:AD:DP:GQ:PL' 15p_SNPs.vcf > 15p_SNPs.txt
15p_SNP="$(wc -l < 15p_SNPs.txt)"

# coverage >=15x
GenomeAnalysisTK -T SelectVariants -R ref.fa -V 15p_variants.g.vcf \
-selectType SNP -select "DP>=15" -o 15p_SNPs_15x.vcf

# make file with all SNPs
grep 'GT:AD:DP:GQ:PL' 15P_SNPs_15x.vcf > 15p_SNPs_15x.txt
15p_SNP_15x="$(wc -l < 15p_SNPs_15x.txt)"


############################


# print to file
echo -e "$PWD\t $50p_SNP\t $50p_SNP_15x\t $23p_SNP\t $23p_SNP_15x\t $15p_SNP\t $15p_SNP_15x" >> /scratch/snyder/a/abruenic/subsample_coverage_SNPs_summary.txt


# END

