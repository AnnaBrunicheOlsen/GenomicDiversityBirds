#!/bin/sh

#PBS -N GATK_nt=20_QC
#PBS -q fnrgenetics
#PBS -l nodes=1:ppn=10,naccesspolicy=singleuser
#PBS -l walltime=300:00:00
#PBS -m abe

module purge
module load bioinfo
module load GATK/3.8.1

cd $PBS_O_WORKDIR

rm -rf SNP*
rm -rf NO_QUAL*
rm -rf realigned*
rm -rf forIndel*
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

#GATK was used for local realignment of reads
GenomeAnalysisTK -nt 10 -T RealignerTargetCreator -R ref.fa -I marked.bam \
-o forIndelRealigner.intervals

GenomeAnalysisTK -T IndelRealigner -R ref.fa -I marked.bam \
-targetIntervals forIndelRealigner.intervals -o realigned_reads.bam

# Run HaplotypeCaller
GenomeAnalysisTK -T HaplotypeCaller -R ref.fa -I realigned_reads.bam -nct 10 \
--genotyping_mode DISCOVERY -stand_call_conf 0 --min_base_quality_score 20 \
--min_mapping_quality_score 20 -o NO_QUAL_variants.g.vcf

# END

