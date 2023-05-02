# bcWNV_analysis
PrimerID pipeline for bcWNV amplicon, and scripts for generating diversity metrics, probability values, rarefaction plots, and barcode progression maintenance plots.
All scripts require path changes to suit local directories. 
PrimerID scripts require changes to primer sequences and UMI length depending on application.

###PrimerID pipeline - 
PrimerID_BarcodeGenerator_WNVBC_V1.sh (calls the 3 scripts below, uses paired-end fastq.gz files as input)
TCS_Ebel_WNVBC.rb
make_columm_matrix
line_counts

### Filter out barcodes that are the result of mutation - 
true_barcodes.sh (uses barcode_counts.txt files from PrimerID pipeline as input)

### Generate barcode population diversity metrics -
barcode_analysis_V2.R (uses either barcode_counts.txt files or true_barcodes.txt files as input)

### Generate richness and subsample values for bootstrap rarefaction -
rarefaction_step1.sh (uses fastq.gz files as input)
bootstrap_rarefaction_V2.R (uses output of rarefaction_step1.sh as input)

### Generate barcode progression and maintenance plots, and barcode stock dynamics plots - 
barcode_maintenance_MG.R (uses true_barcodes.txt files as input)
intrahost_barcode_progression(input).R (uses true_barcodes.txt files as input)
stock_barcode_progression.R (true_barcodes.txt stock sample files as input)

### Generate infection and transmission probability values - 
mg_infection_probability.R (uses true_barcodes.txt files from midgut samples as input)
sg_infection_probability.R (uses true_barcodes.txt files from salivary gland samples as input)
transmission_probability.R (uses true_barcodes.txt files from saliva samples as input)
probability_merge.R (uses output files from the probability.R scripts listed above as input)
