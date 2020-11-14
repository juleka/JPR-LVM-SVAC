# Makefile
#
# :Authors: 	Ju
# :Maintainers: Ju

# ============================================

.PHONY: all clean

all: \
		output/SVAC_fixed.csv \
		output/SVAC_years.csv \
		output/SVAC_new_values.csv \
		output/SVAC_main.csv

clean:
	rm -f output/*

output/SVAC_fixed.csv: \
		src/fix-types.R \
		../import/output/SVAC-imported.csv \
		Makefile
	Rscript --vanilla $< \
		--inputfile=../import/output/SVAC-imported.csv \
		--outputfile=$@

output/SVAC_years.csv: \
		src/uncode-conflict-years.R \
		output/SVAC_fixed.csv \
		hand/uncode_conflict_years.csv \
		Makefile
	Rscript --vanilla $< \
		--inputfile=output/SVAC_fixed.csv \
		--recode_conflict_yrs=hand/uncode_conflict_years.csv \
		--outputfile=$@

output/SVAC_new_values.csv: \
		src/update-var-values.R \
		output/SVAC_years.csv \
		hand/CONSTANTS.yaml \
		Makefile
	Rscript --vanilla $< \
		--inputfile=output/SVAC_years.csv \
		--CONSTANTS=hand/CONSTANTS.yaml \
		--outputfile=$@

output/SVAC_main.csv: \
		src/aggregate-state-non-state.R \
		src/aggregation-functions.R \
		output/SVAC_new_values.csv \
		hand/CONSTANTS.yaml \
		Makefile
	Rscript --vanilla $< \
		--agg_func=src/aggregation-functions.R \
		--CONSTANTS=hand/CONSTANTS.yaml \
		--inputfile=output/SVAC_new_values.csv \
		--outputfile=$@ \
		--all_years=output/SVAC-all-years.csv

#done, end of Makefile.
