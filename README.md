# Runs-of-homozygosity-in-birds
These scripts are developed for the Darwins finches and related tanagers project.

March 2019

Author: Anna Brüniche-Olsen

Make sure that you have: species list and a copy of each of the pipeline scripts (#1-9) in your work directory. For each of the scripts the computational needs are listed. For two of the scripts (3_BWA_mem.sh and 4_picard.sh) the user needs to specify SRAs numbers for some of the steps.

The scripts should be run in the following order:

# 1a_make_dir.sh 
1 CPU

Reads species list (species.csv), makes a dir for each and put the species SRA list in the dir. The species list format has to be: 1 species pr column, species name in the first row followed by X number of rows with SRAs. The script also copies all the pipeline scripts (#1-6) into each dir, so make sure to have a copy of these in your working dir before you run make_dir.sh. If you want to use the 10x depth of coverage sampling instead or aswell as the 20x depth of coverage sampling, ad '6a_GATK_nt1_10x.sh' to the '1a_make_dir.sh' script.

# 1_genome_sra.sh
20 CPU

Downloads and prepares genome and SRA files for all downstream analyses (e.g., check Phred encoding, convert to Phred +33, remove adaptors, trim reads, make FastQC report). Settings for AdaptorRemoval can be changed in "AdaptorRemoval" section of script. Check that ALL FastQC reports before going to bwa step. The FASTQC report summary can be found in qc_log.txt for all SRAs.

# 2_BWA_index_ref.sh
1 CPU

remove pipes and spaces in reference sequence and index it using 'bwtsw'.

# 3_BWA_mem.sh
20 CPU

Insert read group header and map reads using 'mem' to reference. You need to insert the SRA codes for the reads you want to include in this analysis, and thus make a list of BWA mem analyses. 
    NB: some reads will have fastq.gz ending (the ones that passed QC) others will have truncated.gz ending (the ones that were run through AdaptorRemoval). Make sure that these match.

# 4_picard.sh
1 CPU

This script validates each SAM file and produce a BAM file. It merges BAM files into one BAM file, mark duplicates, estimate coverage, and build indexes. Make sure to check that all SAM files pass qc (*_samfile.txt should read: "No errors found"). 
    NB: you will need to supply each SRA number to for the first picard section (SAM -> BAM) and for merging BAM files into one file.

# 5_GATK_nt20.sh
20 CPU

Realign reads, realign indels, and call haplotypes. The quality filters may be changed, see "run HaplotypeCaller" section in script.

# 6_GATK_nt1.sh
1 CPU

Write SNP.vcf, count SNPs, write SNP.vcf for SNPs with >20x coverage, count SNPs with >20x coverage, and estimate genome size. The results from #SNPs, # SNPs>20x, and genome size is writen to 'SNPs_summary.txt" file located in maindir.

# 6a_GATK_nt1_10x.sh
1 CPU

Use 10x depth of coverage instead of 20x depth of coverage. Only run this script if you´re interested in a sub-optimal depth of coverage for calling heterozygous and homozygous sites.

# 7_PLINK_heterozygosity.sh
1 CPU

Provide a list with the species 'species.txt' you want to analyse. The script identifies ROHs, heterozygosity, etc and write a list with: $PWD\t $rohNumber\t $rohLength\t $snpNumberROH\t $N50\t $genome\t $sites\t $het\t $homo\t $SNPs\t $non\t $heteroAll\t $heteroNoRoh\t $SNPrate" >> /scratch/snyder/a/abruenic/PLINK_heterozygosity.txt.
Parameters for PLINK for identifying ROHs can be changed within the script.
Make sure that 'fastaN50.pl' script is in 'scripts' dir. This script can be found in the "7_perl_python_scripts" folder. Also make sure to use the PLINK version that can take >64000 contigs. This version is located on: /depot/fnrdewoody/apps/ and is called plink-1.9.

# 8_ref_name.sh
1 CPU

Writes a list of the first line of the ref.fa file for each species. This is useful for checking if the editing of the MAP files in script '7_PLINK_heterozygosity.sh' is done correctly, i.e., that the right identifying (default is the first full stop) is correct for identifying unique contigs.

# 9_SRA_list.sh
1 CPU

Produces a list of the SRAs used in the final BAM file. 

# Additional folders contain useful scripts for
File format adjusting: identify problems in MAP files, get scaffold distributions, change names of files, etc.



# END
