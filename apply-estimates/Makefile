# Makefile
#
# :Authors: 	Ju
# :Maintainers: Ju

# ============================================

.PHONY: all clean

all: \
		output/svac_est.csv \
		output/svac_new_vars.csv \
		output/svac_main_est.csv \
		output/xt-ai-results.tex

clean:
	rm -f output/*

output/svac_est.csv: \
		src/merge-estimates.R \
		../estimate/output/SVAC_static_est.csv \
		../estimate/output/SVAC_dynamic_est.csv \
		../estimate/hand/CONSTANTS.yaml \
		Makefile
	Rscript --vanilla $< \
		--est_static=../estimate/output/SVAC_static_est.csv \
		--est_dynamic=../estimate/output/SVAC_dynamic_est.csv \
		--CONSTANTS=../estimate/hand/CONSTANTS.yaml \
		--outputfile=$@

output/svac_new_vars.csv: \
		src/create-new-vars.R \
		src/lag-dep-var-functions.R \
		output/svac_est.csv \
		../estimate/hand/CONSTANTS.yaml \
		Makefile
	Rscript --vanilla $< \
		--inputfile=output/svac_est.csv \
		--CONSTANTS=../estimate/hand/CONSTANTS.yaml \
		--dep_var_func=src/lag-dep-var-functions.R \
		--outputfile=$@

output/svac_main_est.csv: \
		src/populate-from-all-years.R \
		src/lag-dep-var-functions.R \
		output/svac_new_vars.csv \
		../estimate/hand/CONSTANTS.yaml \
		../clean/output/SVAC-all-years.csv \
		../estimate/output/SVAC_static_est_all_years.csv \
		../estimate/output/SVAC_dynamic_est_all_years.csv \
		Makefile
	Rscript --vanilla $< \
		--svac=output/svac_new_vars.csv \
		--all_years=../clean/output/SVAC-all-years.csv \
		--all_years_static=../estimate/output/SVAC_static_est_all_years.csv \
		--all_years_dynamic=../estimate/output/SVAC_dynamic_est_all_years.csv \
		--lag_func=src/lag-dep-var-functions.R \
		--CONSTANTS=../estimate/hand/CONSTANTS.yaml \
		--outputfile=$@

output/xt-ai-results.tex: \
		src/run-simple-regressions.R \
		output/svac_main_est.csv \
		../estimate/hand/CONSTANTS.yaml \
		Makefile
	Rscript --vanilla $< \
		--inputfile=output/svac_main_est.csv \
		--CONSTANTS=../estimate/hand/CONSTANTS.yaml \
		--xt_ai=$@ \
		--xt_hrw=output/xt-hrw-results.tex \
		--xt_state=output/xt-state-results.tex \
		--xt_max=output/xt-max-results.tex

#end of Makefile.
