#!/bin/sh

#PBS -N scaffold_map_files
#PBS -q fnrquail
#PBS -l nodes=1:ppn=1,naccesspolicy=shared
#PBS -l walltime=300:00:00
#PBS -m abe

module purge
module load bioinfo
module load vcftools

cd $PBS_O_WORKDIR

export PATH=/home/abruenic:$PATH

cat species_20x.txt | while read -r LINE

do

echo $LINE

cd $LINE

# get overview of ref.fa from all species
# this is to make map files
#ref=$(sed -n '1p' ref.fa)

# print to file
#echo -e "$PWD\t $ref" >> /scratch/snyder/a/abruenic/ref_header.txt


# make plink files
vcftools --vcf SNPs_10x_auto.vcf --plink --out SNPs_10x_auto

# make test file with scaffold info instead of chromosome info
rm scaffold.txt
# take column with contig info
awk '{print $2}' SNPs_10x_auto.map > scaffold.txt
#split at the first full stop
awk -F'.' '{print $1}' scaffold.txt > scaffold2.txt
paste -d' ' scaffold.txt SNPs_10x_auto.map > test.map
awk '{print $1,$3,$4,$5}' test.map > test4.map

# check that scaffold2.txt and original map file has the same number of lines
wc -l scaffold2.txt
wc -l SNPs_10x_auto.map

cd ..

done

# END