#!/bin/sh

#PBS -N reduce_scratch
#PBS -q standby
#PBS -l nodes=1:ppn=1,naccesspolicy=shared
#PBS -l walltime=4:00:00
#PBS -m abe

module purge
module load bioinfo

cd $PBS_O_WORKDIR

cat birds_complete.txt | while read -r LINE

do

cd $LINE

# remove excess files
rm -rf *gz.1
rm -rf core*
rm -rf *gz
rm -rf *.html
rm -rf *.zip
rm -rf *_fastqc
rm -rf 1_genome_sra_only.sh
rm -rf 2_BWA*
rm -rf guessout*
rm -rf temp_log.txt
rm -rf *.sam
rm -rf *.o*
rm -rf *.e*
rm -rf chrlist
rm -rf *_samfile.txt
rm -rf *collapsed*
rm -rf *singleton*
rm -rf *discarded*
rm -rf *settings
rm -rf out.bam
rm -rf sorted.bam
rm -rf *.fq
rm -rf *.fastq
rm -rf *.sh
rm -rf tmp
rm -rf forIndel*
rm -rf infile2*
rm -rf metrics*
rm -rf marked*
rm -rf sra*
rm -rf SRR*
rm -rf qc*
rm -rf ref.fa.*

cd ..

done

# END
