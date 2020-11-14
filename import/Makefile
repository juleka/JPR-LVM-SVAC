# Makefile
#
# :Authors: 	Ju
# :Maintainers: Ju

# ============================================

.PHONY: all clean

all: \
		output/SVAC-imported.csv

clean:
	rm -f output/*

output/SVAC-imported.csv: \
		src/read-in-SVAC.R \
		input/SVAC_2_180525.csv \
		Makefile
	Rscript --vanilla $< \
		--inputfile=input/SVAC_2_180525.csv \
		--outputfile=$@

#end.
