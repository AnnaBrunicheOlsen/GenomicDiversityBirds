for i in *1.fastq.gz_2.truncated.gz
do
    mv "$i" "${i/1.fastq.gz_2.truncated.gz/2.truncated.gz}"
done

for i in *1.fastq.gz_1.truncated.gz
do
    mv "$i" "${i/1.fastq.gz_1.truncated.gz/1.truncated.gz}"
done
