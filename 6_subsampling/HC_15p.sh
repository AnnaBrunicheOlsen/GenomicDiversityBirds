#PBS -N HC_15p
#PBS -q fnrgenetics
#PBS -l nodes=1:ppn=20,naccesspolicy=shared
#PBS -l walltime=300:00:00
#PBS -m abe

module purge
module load bioinfo
module load GATK

cd $PBS_O_WORKDIR

#GATK was used for local realignment of reads
GenomeAnalysisTK -nt 20 -T RealignerTargetCreator -R ref.fa -I marked.bam \
-o forIndelRealigner.intervals

GenomeAnalysisTK -T IndelRealigner -R ref.fa -I marked.bam \
-targetIntervals forIndelRealigner.intervals -o realigned_reads.bam

# Run HaplotypeCaller
GenomeAnalysisTK -T HaplotypeCaller -R ref.fa -I realigned_reads.bam -nct 20 \
--genotyping_mode DISCOVERY -stand_call_conf 0 --min_base_quality_score 20 \
--min_mapping_quality_score 20 -o NO_QUAL_variants.g.vcf

# END
