# Makefile
#
# :Authors: 	Ju
# :Maintainers: Ju

# ============================================

.PHONY: all clean

all: \
		output/SVAC-main.csv

clean:
	rm -f output/*

output/SVAC-main.csv: \
		src/read-in-SVAC.R \
		input/SVAC_2_180525.csv \
		hand/uncode_conflict_years.csv \
		src/Makefile
	Rscript --vanilla $< \
		--input=input/SVAC_2_180525.csv \
		--fix_conflict_yrs=hand/uncode_conflict_years.csv \
		--output=$@

#end.
