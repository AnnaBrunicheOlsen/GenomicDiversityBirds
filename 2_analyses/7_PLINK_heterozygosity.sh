#!/bin/sh

#PBS -N roh_hetero
#PBS -q standby
#PBS -l nodes=1:ppn=1,naccesspolicy=shared
#PBS -l walltime=4:00:00
#PBS -m abe

module purge
module load bioinfo
module load vcftools
module load samtools
module load GATK

cd $PBS_O_WORKDIR

#export PATH=/home/abruenic:$PATH
export PATH=/depot/fnrdewoody/apps:$PATH

cat species_80_11.txt | while read -r LINE

do

echo $LINE

cd $LINE

######################################################
###### THIS PART REMOVE MTDNA FROM REF AND VCF ######
######################################################

# we want to use the file without mtDNA - only autosomes
# so we first check if the vcf without mtDNA exists

# 10x depth of coverage
FILE1=$"SNPs_10x_auto.vcf"

if [ -f $FILE1 ]
then
	echo "$FILE1 exist"
else	
	 remove all mitochondrial sequences from vcf
	grep -v -i 'mito\|mtDNA\|mt' SNPs_10x.vcf > SNPs_10x_auto.vcf
fi

# 20x depth of cov
FILE1=$"SNPs_20x_auto.vcf"

if [ -f $FILE1 ]
then
	echo "$FILE1 exist"
else	
	 remove all mitochondrial sequences from vcf
	grep -v -i 'mito\|mtDNA\|mt' SNPs_20x.vcf > SNPs_20x_auto.vcf
fi

# check if ref.fa without mtDNA exists
FILE2=$"ref_auto.fa"
if [ -f $FILE2 ]
then 
	echo "$FILE2 exist"
else	
	 remove all mitochondrial sequences from ref.fa
	grep -v -i 'mito\|mtDNA' ref.fa > ref_auto.fa
	grep -i 'mito\|mtDNA' ref.fa > ref_mito.txt

fi

############################################
###### THIS PART CHECKS THE MAP-FILES ######
############################################

# convert vcf to PLINK ped+map files
# make plink files

# 10x depth of cov
vcftools --vcf SNPs_10x_auto.vcf --plink --out SNPs_10x_auto

# make test file with scaffold info instead of chromosome info
rm -rf scaffold.txt
rm -rf scaffold2.txt
# take column with contig info
awk '{print $2}' SNPs_10x_auto.map > scaffold.txt
#split at the first full stop
awk -F'.' '{print $1}' scaffold.txt > scaffold2.txt
paste -d' ' scaffold2.txt SNPs_10x_auto.map > test.map
awk '{print $1,$3,$4,$5}' test.map > SNPs_10x_auto.map


# 20x depth of cov
vcftools --vcf SNPs_20x_auto.vcf --plink --out SNPs_20x_auto

# make test file with scaffold info instead of chromosome info
rm -rf scaffold.txt
rm -rf scaffold2.txt
# take column with contig info
awk '{print $2}' SNPs_20x_auto.map > scaffold.txt
#split at the first full stop
awk -F'.' '{print $1}' scaffold.txt > scaffold2.txt
paste -d' ' scaffold2.txt SNPs_20x_auto.map > test.map
awk '{print $1,$3,$4,$5}' test.map > SNPs_20x_auto.map


#########################
# 10x depth of coverage #
#########################

##################################################
###### THIS PART FINDS RUNS-OF-HOMOZYGOSITY ######
##################################################

# the parameters for ROH calling can be changed
# see Howrigan et al 2011 BMC Genomics
# find ROHs in vcf without mito

# 10x depth of cov
plink-1.9 --ped SNPs_10x_auto.ped --map SNPs_10x_auto.map --allow-extra-chr --homozyg-snp 20 \
--homozyg-kb 1 --homozyg-window-het 1 --homozyg-window-snp 20 \
--homozyg-window-threshold 0.01 --out SNPs_10x_auto

# count number of ROHs, ROHs are in column 9
awk '{print $9}' SNPs_10x_auto.hom > ROH.txt

# remove header
tail -n+2 ROH.txt > roh.txt

# count number of ROHs
wc -l roh.txt > rohNumber.txt
rohNumber=$(awk '{print $1}' rohNumber.txt)

# estimate length of ROHs
rohLength=$(awk '{ sum += $1} END {printf "%.2f", sum }' roh.txt)

# number of SNPs in ROHs, SNPs are in column 10
awk '{print $10}' SNPs_10x_auto.hom > ROH_SNPs.txt

# remove header
tail -n+2 ROH_SNPs.txt > roh_snps.txt

# estimate number of ROHs
snpNumberROH=$(awk '{ sum += $1} END {printf "%.2f", sum }' roh_snps.txt)

# change name in *.hom files
# this is to make the file ready for R-plots
sed -i "s/sample1/$LINE/g" SNPs_10x_auto.hom

# make a new ROHs_species.txt before you run all species
# append 
cat SNPs_10x_auto.hom >> /scratch/snyder/a/abruenic/species_80_10x_20SNP.txt


############################################
###### THIS PART FINDS HETEROZYGOSITY ######
############################################

# calculate N50
# make sure that 'fastaN50.pl' script is in 'scripts' dir
perl /scratch/snyder/a/abruenic/scripts/fastaN50.pl ref_auto.fa > ref_summary.txt

# replace ': ' with \n
tr ': ' '\n' <ref_summary.txt > N50.txt 

# grep line 3 (N50)
N50=$(sed -n '3p' N50.txt)

# index ref_auto.fa file
#samtools faidx ref_auto.fa

# genome size
genome="$(cut -f 2 ref.fa.fai | paste -sd+ | bc)"

# coverage needs to be included we chose 20x as minimum
# check if the file listing coverage/bp exists
FILE4=$"realigned_reads.sample_cumulative_coverage_counts"

if [ -f $FILE4 ]
then

    echo "$FILE4_exist"
    coverage=$"$FILE4_exist"

else
	# depht of coverage 
	GenomeAnalysisTK -T DepthOfCoverage -R ref.fa -o realigned_reads \
	-I realigned_reads.bam

fi

# 10x depth of coverage
# total number of sites at 10x depth of coverage
# replace \t with \n
tr '\t' '\n' < realigned_reads.sample_cumulative_coverage_counts > site_10x.txt
# grep line 514 (gte_10)
sites=$(sed -n '514p' site_10x.txt)

# count heterozygotes
grep '0/1' SNPs_10x_auto.vcf > het.txt
het="$(wc -l < het.txt)"

# count homozygotes
grep '0/0' SNPs_10x_auto.vcf > homo.txt
grep '1/1' SNPs_10x_auto.vcf >> homo.txt
homo="$(wc -l < homo.txt)"

# total number of SNPs
SNPs=$(($homo + $het)) 

# non-variant sites
bc <<< "($sites - $SNPs)" > non.txt
non=$(sed -n '1p' non.txt)

# heterozygosity
# adjust scale to allow for decimals
bc <<< "scale=9; ($het / $sites)" > hetero.txt
heteroAll=$(sed -n '1p' hetero.txt)

# heterozygosity minus ROH snps
bc <<< "($sites - $snpNumberROH)" > sitesNoRoh.txt
sitesNoRoh=$(sed -n '1p' sitesNoRoh.txt)
bc <<< "scale=9; ($het / $sitesNoRoh)" > hetNoRoh.txt
heteroNoRoh=$(sed -n '1p' hetNoRoh.txt)

# SNPrate
bc <<< "scale=6; ($SNPs / $sites)" > SNPrate.txt
SNPrate=$(sed -n '1p' SNPrate.txt)

# print to file
echo -e "$PWD\t $rohNumber\t $rohLength\t $snpNumberROH\t \
$N50\t $genome\t $sites\t $het\t $homo\t $SNPs\t $non\t $heteroAll\t $heteroNoRoh\t $SNPrate" \
>> /scratch/snyder/a/abruenic/species_80_10x_20SNP_1het_heterozygosity.txt


#########################
# 20x depth of coverage #
#########################

##################################################
###### THIS PART FINDS RUNS-OF-HOMOZYGOSITY ######
##################################################

plink-1.9 --ped SNPs_20x_auto.ped --map SNPs_20x_auto.map --allow-extra-chr --homozyg-snp 20 \
--homozyg-kb 1 --homozyg-window-het 1 --homozyg-window-snp 20 \
--homozyg-window-threshold 0.01 --out SNPs_20x_auto

# count number of ROHs, ROHs are in column 9
awk '{print $9}' SNPs_20x_auto.hom > ROH.txt

# remove header
tail -n+2 ROH.txt > roh.txt

# count number of ROHs
wc -l roh.txt > rohNumber.txt
rohNumber=$(awk '{print $1}' rohNumber.txt)

# estimate length of ROHs
rohLength=$(awk '{ sum += $1} END {printf "%.2f", sum }' roh.txt)

# number of SNPs in ROHs, SNPs are in column 10
awk '{print $10}' SNPs_20x_auto.hom > ROH_SNPs.txt

# remove header
tail -n+2 ROH_SNPs.txt > roh_snps.txt

# estimate number of ROHs
snpNumberROH=$(awk '{ sum += $1} END {printf "%.2f", sum }' roh_snps.txt)

# change name in *.hom files
# this is to make the file ready for R-plots
sed -i "s/sample1/$LINE/g" SNPs_20x_auto.hom

# make a new ROHs_species.txt before you run all species
# append 
cat SNPs_20x_auto.hom >> /scratch/snyder/a/abruenic/species_80_20x_20SNP.txt

############################################
###### THIS PART FINDS HETEROZYGOSITY ######
############################################

# total number of sites at 20x depth of coverage
# replace \t with \n
tr '\t' '\n' < realigned_reads.sample_cumulative_coverage_counts > site_20x.txt
# grep line 524 (gte_20)
sites=$(sed -n '524p' site_20x.txt)

# count heterozygotes
grep '0/1' SNPs_20x_auto.vcf > het.txt
het="$(wc -l < het.txt)"

# count homozygotes
grep '0/0' SNPs_20x_auto.vcf > homo.txt
grep '1/1' SNPs_20x_auto.vcf >> homo.txt
homo="$(wc -l < homo.txt)"

# total number of SNPs
SNPs=$(($homo + $het)) 

# non-variant sites
bc <<< "($sites - $SNPs)" > non.txt
non=$(sed -n '1p' non.txt)

# heterozygosity
# adjust scale to allow for decimals
bc <<< "scale=9; ($het / $sites)" > hetero.txt
heteroAll=$(sed -n '1p' hetero.txt)

# heterozygosity minus ROH snps
bc <<< "($sites - $snpNumberROH)" > sitesNoRoh.txt
sitesNoRoh=$(sed -n '1p' sitesNoRoh.txt)
bc <<< "scale=9; ($het / $sitesNoRoh)" > hetNoRoh.txt
heteroNoRoh=$(sed -n '1p' hetNoRoh.txt)

# SNPrate
bc <<< "scale=6; ($SNPs / $sites)" > SNPrate.txt
SNPrate=$(sed -n '1p' SNPrate.txt)


# first make file and put header in
#echo -e ""PWD"\t "rohNumber"\t "rohLength"\t "snpNumberROH"\t \
#"N50"\t "genome_size"\t "sites_cov20x"\t "heterozygote_sites"\t "homozygote_sites"\t \
#"total_SNPs"\t "nonvariant_sites"\t "heterozygosity_all"\t "heterozygosity_no_ROH"\t "SNPrate"" \
#> /scratch/snyder/a/abruenic/ROH_20x_20SNP_1het_heterozygosity.txt

# print to file
echo -e "$PWD\t $rohNumber\t $rohLength\t $snpNumberROH\t \
$N50\t $genome\t $sites\t $het\t $homo\t $SNPs\t $non\t $heteroAll\t $heteroNoRoh\t $SNPrate" \
>> /scratch/snyder/a/abruenic/species_80_20x_20SNP_1het_heterozygosity.txt

cd ..

done

# END
