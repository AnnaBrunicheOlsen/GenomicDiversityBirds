#!/bin/sh

#PBS -N unique
#PBS -q fnrdewoody
#PBS -l nodes=1:ppn=1,naccesspolicy=shared
#PBS -l walltime=100:00:00
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

# 10x
vcftools --vcf SNPs_10x_auto.vcf --plink --out SNPs_10x_auto

# make test file with scaffold info instead of chromosome info
rm scaffold.txt
rm scaffold2.txt
# take column with contig info
awk '{print $2}' SNPs_10x_auto.map > scaffold.txt
#split at the first full stop
awk -F'.' '{print $1}' scaffold.txt > scaffold2.txt
paste -d' ' scaffold.txt SNPs_10x_auto.map > test.map
awk '{print $1,$3,$4,$5}' test.map > SNPs_10x_auto.map 

# count number of unique scaffolds
sort scaffold2.txt | uniq -c > unique.txt
wc -l unique.txt > contig_number.txt
uniq10=$(awk '{print $1}' contig_number.txt)

# 20x
vcftools --vcf SNPs_20x_auto.vcf --plink --out SNPs_20x_auto

# make test file with scaffold info instead of chromosome info
rm scaffold.txt
rm scaffold2.txt
# take column with contig info
awk '{print $2}' SNPs_20x_auto.map > scaffold.txt
#split at the first full stop
awk -F'.' '{print $1}' scaffold.txt > scaffold2.txt
paste -d' ' scaffold.txt SNPs_20x_auto.map > test.map
awk '{print $1,$3,$4,$5}' test.map > SNPs_20x_auto.map 

# count number of unique scaffolds
sort scaffold2.txt | uniq -c > unique.txt
wc -l unique.txt > contig_number.txt
uniq20=$(awk '{print $1}' contig_number.txt)

echo -e "$PWD\t $uniq10\t $uniq20" >> /scratch/snyder/a/abruenic/divide_scaffolds_10x_20x.txt

cd ..

done

# END
