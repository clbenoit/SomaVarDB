# How to prepare a vcf before importation SQLite db see https://github.com/tkoomar/VCFdbR for further explanation

MYVCF=Perry_truncated.vcf.gz
OUTVCF=Perry_truncated_ready.vcf.gz

MYVCF=SeqOne_truncated.vcf.gz
OUTVCF=SeqOne_truncated_ready.vcf.gz

bcftools norm -c ws -f /home/ptngs/genome_references/GRCh37.fa -m - ${MYVCF} | sed -e 's/Number=A/Number=1/g' | sed -e 's/Number=\./Number=1/g' | bgzip -c > TEMP.vcf.gz

bcftools view TEMP.vcf.gz | sed -e 's/Number=1,Type=Float/Number=1,Type=String/g' |  sed -e 's/Number=1,Type=Integer/Number=1,Type=String/g' | bgzip -c > TEMP2.vcf.gz

## if annotated_with_vep and CSQ column not be parsed correctly.
zcat TEMP2.vcf.gz| sed '/^#/\! s/;;/;/g' | bgzip -c > ${OUTVCF}
else mv TEMP2.vcf.gz ${OUTVCF}

tabix ${OUTVCF}


# Add COSMIC annotations to vcf

file=/home/ptngs/ClinicalResultsBrowser_vcfs/SEQONE_DATABASE_EXPORTS/27_02_2023/merge_grenoble_onco.vcf.gz
tmpdir=/home/ptngs/ClinicalResultsBrowser_vcfs/SEQONE_DATABASE_EXPORTS/27_02_2023/splited_onco_vcf_cosm
mkdir -p ${tmpdir}/
cd ${tmpdir}
VERSION=96
ID_STRING=$(echo 'cbenoit3@chu-grenoble.fr:12Azefg!' | base64)
curl -H "Authorization: Basic ${ID_STRING}" \
https://cancer.sanger.ac.uk/cosmic/file_download/GRCh37/cosmic/v${VERSION}/VCF/CosmicCodingMuts.vcf.gz > ${tmpdir}/url.txt
URL=$(grep "url" ${tmpdir}/url.txt | sed 's/{"url":"//g' | sed 's/"}//g')
curl -o ${tmpdir}/CosmicCodingMuts.vcf.gz  "${URL}"

#tabix ${tmpdir}/CosmicMutantExport.tab.gz
tabix -f ${tmpdir}/CosmicCodingMuts.vcf.gz
bcftools query -f'%CHROM\t%POS\t%POS\t%ID\n' ${tmpdir}/CosmicCodingMuts.vcf.gz | bgzip -c > ${tmpdir}/CosmicMutantExport.tab.gz 
echo '##INFO=<ID=COSMIC,Number=1,Type=String,Description="Mutation Cosmic ID">' > ${tmpdir}/cosmic.hdr
#tabix -f -s1 -b2 -e3 ${tmpdir}/CosmicMutantExport.tab.gz
#bcftools annotate -a ${tmpdir}/CosmicMutantExport.tab.gz  -h ${tmpdir}/cosmic.hdr -c CHROM,FROM,TO,ID ${file} | bgzip -c > ${tmpdir}/${file}.tmp
bcftools annotate -a ${tmpdir}/CosmicMutantExport.tab.gz  -h ${tmpdir}/cosmic.hdr -c CHROM,FROM,TO,INFO/COSMIC ${file} | bgzip -c > ${file}.tmp
#mv ${file}.tmp ${file}
cp -r ${file}.tmp ${file}
tabix -f ${file}

# if you vcf contains too many samples it could require to much RAM for your computer. So split it by sample like this
THREADS=2
outdir=/home/ptngs/ClinicalResultsBrowser_vcfs/SEQONE_DATABASE_EXPORTS/27_02_2023/splited_onco_vcf
outdir=/home/ptngs/ClinicalResultsBrowser_vcfs/SEQONE_DATABASE_EXPORTS/27_02_2023/splited_onco_twofiles/
file=/home/ptngs/ClinicalResultsBrowser_vcfs/SEQONE_DATABASE_EXPORTS/27_02_2023/merge_grenoble_onco.vcf.gz
for sample in `bcftools query -l $file`; do
	echo "bcftools view --threads ${THREADS} -Oz -s $sample -o ${outdir}/${sample}_tmp.vcf.gz $file"
	bcftools view --threads ${THREADS} -Oz -s ${sample}  -o ${outdir}/${sample}_tmp.vcf.gz $file
	echo "bcftools view --threads ${THREADS} -Oz -e 'GT=\"./.\"' -o ${outdir}/${sample}.vcf.gz ${outdir}/${sample}_tmp.vcf.gz"
	bcftools view --threads ${THREADS} -Oz -e 'GT="./."' -o ${outdir}/${sample}.vcf.gz ${outdir}/${sample}_tmp.vcf.gz
	rm ${outdir}/${sample}_tmp.vcf.gz
	echo "tabix ${outdir}/${sample}.vcf.gz"
	tabix ${outdir}/${sample}.vcf.gz
	echo -e "\n"
done



