#/bin/sh
#Date 9/20/22
#Author: Emily Fitzmeyer

# define arguments passed as 'file_base'
file_base=$@

# alert the user if input is not supplied
if [ $# == 0 ]
then
	echo -e "true_barcodes>>>>> ERROR>>>>>
	Please provide input like so: ./this_script arg1.txt"
fi

# print greeting if input is supplied
if [ $# -gt 0 ]
then
	echo -e "Processing samples: $file_base"
	date
fi

for file_base in ${file_base[@]}
do
	# get line count of input file
	line_count=$(wc -l $file_base | sed s/$file_base//g)

	# print line count of input file
	echo -e "The number of barcodes in $file_base is $line_count"

	# define output file name
	output_file=${file_base/barcode*.txt/true_barcodes.txt}

	# select for true barcode pattern and read into output file
	grep 'CT.AC.GT.AC.GT.AC.GC.GC.AC.CT.CT.' $file_base > $output_file

	# get line count of output file
	new_line_count=$(wc -l $output_file | sed s/$output_file//g)

	# print line count of output file
	echo -e "The number of barcodes in $output_file is $new_line_count"

	# define difference between input and output line counts
	difference=$((line_count-new_line_count))

	# print how many barcodes have been removed
	echo -e "$difference barcodes have been removed"
done

# tell the user the script is done
echo -e "Done"

