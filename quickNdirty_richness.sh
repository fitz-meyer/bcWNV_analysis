#/bin/sh
#Date 5/31/23
#Author: Emily Fitzmeyer

#comment out appropriate segment before running
#scripts (this script and R script) must be in working directory with files


# define arguments passed as 'file_base'
file_base=$@

# alert the user if input is not supplied
if [ $# == 0 ]
then
	echo -e "quickNdirty>>>>> ERROR>>>>>
	Please provide input like so: ./this_script arg1.fastq"
fi

# print greeting if input is supplied
if [ $# -gt 0 ]
then
	echo -e "Processing samples: $file_base"
	date
fi

touch log.txt

for file_base in ${file_base[@]}
do
	#unzip file - Using zgrep instead 
	#gunzip $file_base.fastq.gz
	
	# define output file name
	output_file=${file_base/.fastq/_unique_barcodes.txt}
	
	#pull out barcode containing reads and barcode sequence
	#comment out appropriate segment before running
	
	# L SEGMENT
	#grep "ATTTAAAATTAATACATCAT" $file_base | grep "TATAGTAAAAATAATCCTGA" > int_file.csv
	
	# S SEGMENT
	grep "ACCCTCAATACCTGAAGAAG" $file_base | grep "GATAAGTGGATGTCACAGAA" > int_file.csv
	
	# call R script
	# comment out appropriate segment in R script before running 
	R --vanilla -f quickNdirty_richness.R
	
	# change name of barcodes.txt so it's unique and doesn't get overwritten
	barcode_file=${file_base/.fastq/_barcodes.txt}
	
	# select for true barcode pattern and read into output file
	
	# L SEGMENT
	#grep 'TC.CC.GC.CT.GT.CT.CG.GC.' barcodes.csv > $barcode_file
	
	# S SEGMENT
	grep 'GC.CT.CG.CAACG.TATGG.AC.CT.AC.GC.' barcodes.csv > $barcode_file
	
	#sort unique
	sort -u $barcode_file > $output_file
	
	#estimated unique barcodes=
	unique=$(wc -l $output_file)
	
	# print how many unique barcodes are in output file
	echo -e "$unique" >> log.txt
	
	#rm int_file.txt

done

# tell the user the script is done
echo -e "Done"

