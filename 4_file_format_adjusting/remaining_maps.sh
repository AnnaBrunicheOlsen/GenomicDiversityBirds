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

cd /scratch/snyder/a/abruenic/Balaenoptera_bonaerensis #DF818470
vcftools --vcf SNPs_10x_auto.vcf --plink --out SNPs_10x_auto
grep -E -o "DF.{0,6}" SNPs_10x_auto.map > scaffold.txt
paste -d' ' scaffold.txt SNPs_10x_auto.map > test.map
awk '{print $1,$3,$4,$5}' test.map > test4.map
cd ..

cd /scratch/snyder/a/abruenic/Capra_aegagrus	#CM003214
vcftools --vcf SNPs_10x_auto.vcf --plink --out SNPs_10x_auto
grep -E -o "CM.{0,6}" SNPs_10x_auto.map > scaffold.txt
paste -d' ' scaffold.txt SNPs_10x_auto.map > test.map
awk '{print $1,$3,$4,$5}' test.map > test4.map
cd ..

cd /scratch/snyder/a/abruenic/Choloepus_hoffmanni	#ABVD02000001
vcftools --vcf SNPs_10x_auto.vcf --plink --out SNPs_10x_auto
grep -E -o "ABVD.{0,8}" SNPs_10x_auto.map > scaffold.txt
paste -d' ' scaffold.txt SNPs_10x_auto.map > test.map
awk '{print $1,$3,$4,$5}' test.map > test4.map
cd ..

cd /scratch/snyder/a/abruenic/Eidolon_helvum	#KE747934
vcftools --vcf SNPs_10x_auto.vcf --plink --out SNPs_10x_auto
grep -E -o "KE.{0,6}" SNPs_10x_auto.map > scaffold.txt
paste -d' ' scaffold.txt SNPs_10x_auto.map > test.map
awk '{print $1,$3,$4,$5}' test.map > test4.map
cd ..

cd /scratch/snyder/a/abruenic/Ellobius_lutescens	#LOEQ01000001
vcftools --vcf SNPs_10x_auto.vcf --plink --out SNPs_10x_auto
grep -E -o "LOEQ.{0,8}" SNPs_10x_auto.map > scaffold.txt
paste -d' ' scaffold.txt SNPs_10x_auto.map > test.map
awk '{print $1,$3,$4,$5}' test.map > test4.map
cd ..

cd /scratch/snyder/a/abruenic/Ellobius_talpinus	LOJH01000001
vcftools --vcf SNPs_10x_auto.vcf --plink --out SNPs_10x_auto
grep -E -o "LOJH.{0,8}" SNPs_10x_auto.map > scaffold.txt
paste -d' ' scaffold.txt SNPs_10x_auto.map > test.map
awk '{print $1,$3,$4,$5}' test.map > test4.map
cd ..

cd /scratch/snyder/a/abruenic/Eulemur_macaco #LGHX01000001
vcftools --vcf SNPs_10x_auto.vcf --plink --out SNPs_10x_auto
grep -E -o "LGHX.{0,8}" SNPs_10x_auto.map > scaffold.txt
paste -d' ' scaffold.txt SNPs_10x_auto.map > test.map
awk '{print $1,$3,$4,$5}' test.map > test4.map
cd ..

cd /scratch/snyder/a/abruenic/Felis_catus #NT_187803
vcftools --vcf SNPs_10x_auto.vcf --plink --out SNPs_10x_auto
grep -E -o "NT_.{0,6}" SNPs_10x_auto.map > scaffold.txt
paste -d' ' scaffold.txt SNPs_10x_auto.map > test.map
awk '{print $1,$3,$4,$5}' test.map > test4.map
cd ..

cd /scratch/snyder/a/abruenic/Giraffa_camelopardalis #LVKQ01S000001
vcftools --vcf SNPs_10x_auto.vcf --plink --out SNPs_10x_auto
grep -E -o "LVKQ.{0,9}" SNPs_10x_auto.map > scaffold.txt
paste -d' ' scaffold.txt SNPs_10x_auto.map > test.map
awk '{print $1,$3,$4,$5}' test.map > test4.map
cd ..

cd /scratch/snyder/a/abruenic/Macaca_mulatta	#NC_027893
vcftools --vcf SNPs_10x_auto.vcf --plink --out SNPs_10x_auto
grep -E -o "NC_.{0,6}" SNPs_10x_auto.map > scaffold.txt
paste -d' ' scaffold.txt SNPs_10x_auto.map > test.map
awk '{print $1,$3,$4,$5}' test.map > test4.map
cd ..

cd /scratch/snyder/a/abruenic/Megaderma_lyra #KI000001
vcftools --vcf SNPs_10x_auto.vcf --plink --out SNPs_10x_auto
grep -E -o "KI.{0,6}" SNPs_10x_auto.map > scaffold.txt
paste -d' ' scaffold.txt SNPs_10x_auto.map > test.map
awk '{print $1,$3,$4,$5}' test.map > test4.map
cd ..

cd /scratch/snyder/a/abruenic/Microcebus_murinus 	#NC_033660
vcftools --vcf SNPs_10x_auto.vcf --plink --out SNPs_10x_auto
grep -E -o "NC_.{0,6}" SNPs_10x_auto.map > scaffold.txt
paste -d' ' scaffold.txt SNPs_10x_auto.map > test.map
awk '{print $1,$3,$4,$5}' test.map > test4.map
cd ..

cd /scratch/snyder/a/abruenic/Microtus_ochrogaster	 #NC_022009
vcftools --vcf SNPs_10x_auto.vcf --plink --out SNPs_10x_auto
grep -E -o "NC_.{0,6}" SNPs_10x_auto.map > scaffold.txt
paste -d' ' scaffold.txt SNPs_10x_auto.map > test.map
awk '{print $1,$3,$4,$5}' test.map > test4.map

cd ..cd /scratch/snyder/a/abruenic/Macaca_mulatta	#NC_027893
vcftools --vcf SNPs_10x_auto.vcf --plink --out SNPs_10x_auto
grep -E -o "NC_.{0,6}" SNPs_10x_auto.map > scaffold.txt
paste -d' ' scaffold.txt SNPs_10x_auto.map > test.map
awk '{print $1,$3,$4,$5}' test.map > test4.map
cd ..

cd ..cd /scratch/snyder/a/abruenicMus_spretus	 #CM004094
vcftools --vcf SNPs_10x_auto.vcf --plink --out SNPs_10x_auto
grep -E -o "CM.{0,6}" SNPs_10x_auto.map > scaffold.txt
paste -d' ' scaffold.txt SNPs_10x_auto.map > test.map
awk '{print $1,$3,$4,$5}' test.map > test4.map
cd ..

cd ..cd /scratch/snyder/a/abruenic/Rattus_norvegicus	 #NC_005100
vcftools --vcf SNPs_10x_auto.vcf --plink --out SNPs_10x_auto
grep -E -o "NC_.{0,6}" SNPs_10x_auto.map > scaffold.txt
paste -d' ' scaffold.txt SNPs_10x_auto.map > test.map
awk '{print $1,$3,$4,$5}' test.map > test4.map
cd ..

cd ..cd /scratch/snyder/a/abruenic/Rhinolophus_ferrumequinum	#KI126401
vcftools --vcf SNPs_10x_auto.vcf --plink --out SNPs_10x_auto
grep -E -o "KI.{0,6}" SNPs_10x_auto.map > scaffold.txt
paste -d' ' scaffold.txt SNPs_10x_auto.map > test.map
awk '{print $1,$3,$4,$5}' test.map > test4.map
cd ..



# END
