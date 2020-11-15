# Makefile
#
# :Authors: 	Ju
# :Maintainers: Ju

# ============================================

.PHONY: all clean

all: \
		output/SVAC_static_est.csv \
		output/SVAC_dynamic_est.csv \
		output/SVAC_static_est_all_years.csv \
		output/SVAC_dynamic_est_all_years.csv

clean:
	rm -f output/*

static_model_recs := \
		src/SVAC_static.stan \
		src/model-fit-functions.R \
		hand/CONSTANTS.yaml \
		Makefile

dynamic_model_recs := \
		src/SVAC_dynamic.stan \
		src/model-fit-functions.R \
		hand/CONSTANTS.yaml \
		Makefile

output/SVAC_static_est.csv: \
		src/estimate-SVAC-static.R \
		../clean/output/SVAC_main.csv \
		$(static_model_recs)
	Rscript --vanilla $< \
		--inputfile=../clean/output/SVAC_main.csv \
		--STANcode=src/SVAC_static.stan \
		--model_fit_functions=src/model-fit-functions.R \
		--extract_parameters=src/extract-alphas-betas.R \
		--CONSTANTS=hand/CONSTANTS.yaml \
		--static_params=output/static-alphas-betas.Rdata \
		--static_waic=output/static-waic.Rdata \
		--outputfile=$@

output/SVAC_dynamic_est.csv: \
		src/estimate-SVAC-dynamic.R \
		../clean/output/SVAC_main.csv \
		$(dynamic_model_recs)
	Rscript --vanilla $< \
		--inputfile=../clean/output/SVAC_main.csv \
		--STANcode=src/SVAC_dynamic.stan \
		--model_fit_functions=src/model-fit-functions.R \
		--extract_parameters=src/extract-alphas-betas.R \
		--CONSTANTS=hand/CONSTANTS.yaml \
		--dynamic_params=output/dynamic-alphas-betas.Rdata \
		--dynamic_waic=output/dynamic-waic.Rdata \
		--outputfile=$@

output/SVAC_static_est_all_years.csv: \
		src/estimate-SVAC-static-all-years.R \
		../clean/output/SVAC-all-years.csv \
		$(static_model_recs)
	Rscript --vanilla $< \
		--inputfile=../clean/output/SVAC-all-years.csv \
		--STANcode=src/SVAC_static.stan \
		--model_fit_functions=src/model-fit-functions.R \
		--CONSTANTS=hand/CONSTANTS.yaml \
		--outputfile=$@

output/SVAC_dynamic_est_all_years.csv: \
		src/estimate-SVAC-dynamic-all-years.R \
		../clean/output/SVAC-all-years.csv \
		$(dynamic_model_recs)
	Rscript --vanilla $< \
		--inputfile=../clean/output/SVAC-all-years.csv \
		--STANcode=src/SVAC_dynamic.stan \
		--model_fit_functions=src/model-fit-functions.R \
		--CONSTANTS=hand/CONSTANTS.yaml \
		--outputfile=$@
	rm Rplots.pdf

#end of Makefile.
