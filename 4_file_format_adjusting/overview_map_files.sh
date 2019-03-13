#!/bin/sh

#PBS -N scaffold_map_files
#PBS -q fnrdewoody
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

# this will vary across species
# look at the header.txt to find out what pattern to grep

cat species_20x.txt | while read -r LINE

do

echo $LINE

cd $LINE

# make plink files
#vcftools --vcf SNPs_15x_auto.vcf --plink --out SNPs_15x_auto

# make test file with scaffold info instead of chromosome info
# for Acino grep 'NW_' and 9 characters after
grep -E -o "NW_.{0,9}" SNPs_15x_auto.map > scaffold_15x.txt

# check if scaffolds have other identifiers
if [ -s scaffold_15x.txt ]
then
	echo -e "$PWD" >> /scratch/snyder/a/abruenic/map_files_with irregular_scaffold_codes_15x.txt
fi

# insert scaffold info in map file
paste -d' ' scaffold_15x.txt SNPs_15x_auto.map > test_15x.map

# remove column 2 with the only scaffold names
awk '{print $1,$3,$4,$5}' test_15x.map > test2_15x.map

cd ..

done

# END