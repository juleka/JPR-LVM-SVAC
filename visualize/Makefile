# Makefile
#
# :Authors: 	Ju
# :Maintainers: Ju

# ============================================

.PHONY: all clean

all: \
		output/bp-prevalence-vectors-dist.pdf \
		output/bp-prev-over-time.pdf \
		output/log-barplot-SV-for-actor-type-by-conflict.txt \
		output/xt-irr-kappa.tex \
		output/tl-est-static-dynamic.pdf \
		output/log-boxplot-estimate-dist.txt \
		output/log-plot-estimates-by-conflict-actor.txt \
		output/xt-corr-obs-est.tex \
		output/tl-corr-obs-est-All-static.pdf \
		output/xt-alpha-beta-params.tex

clean:
	rm -f output/*

output/bp-prevalence-vectors-dist.pdf: \
		src/barplot-prev-distribution.R \
		../apply-estimates/output/svac_main_est.csv \
		../estimate/hand/CONSTANTS.yaml \
		Makefile
	Rscript --vanilla $< \
		--inputfile=../apply-estimates/output/svac_main_est.csv \
		--CONSTANTS=../estimate/hand/CONSTANTS.yaml \
		--out_plot=$@

output/bp-prev-over-time.pdf: \
		src/barplot-obs-over-time.R \
		../apply-estimates/output/svac_main_est.csv \
		../clean/output/SVAC_new_values.csv \
		Makefile
	Rscript --vanilla $< \
		--inputfile=../apply-estimates/output/svac_main_est.csv \
		--inputfile_nonagg=../clean/output/SVAC_new_values.csv \
		--outputfile_mainarticle=$@ \
		--outputfile_agg_max=output/bp-prev-over-time-agg-max-prev.pdf \
		--outputfile_nonagg_max=output/bp-prev-over-time-nonagg-max-prev.pdf \
		--outputfile_agg_ai=output/bp-prev-over-time-agg-ai.pdf \
		--outputfile_agg_hrw=output/bp-prev-over-time-agg-hrw.pdf \
		--outputfile_agg_ussd=output/bp-prev-over-time-agg-state.pdf \
		--outputfile_nonagg_ai=output/bp-prev-over-time-nonagg-ai.pdf \
		--outputfile_nonagg_hrw=output/bp-prev-over-time-nonagg-hrw.pdf \
		--outputfile_nonagg_ussd=output/bp-prev-over-time-nonagg-state.pdf

output/log-barplot-SV-for-actor-type-by-conflict.txt: \
		src/barplot-SV-for-actor-type-by-conflict.R \
		../estimate/hand/CONSTANTS.yaml \
		../clean/output/SVAC_main.csv \
		Makefile
	Rscript --vanilla $< \
		--inputfile=../clean/output/SVAC_main.csv \
		--CONSTANTS=../estimate/hand/CONSTANTS.yaml \
		--outputfile=$@

output/xt-irr-kappa.tex: \
		src/relate-prev-vars.R \
		../import/output/SVAC-imported.csv \
		Makefile
	Rscript --vanilla $< \
		--inputfile=../import/output/SVAC-imported.csv \
		--xt_out_kappa=$@ \
		--tl_irr_all=output/tl-irr-all.pdf \
		--tl_irr_gov=output/tl-irr-GOV.pdf \
		--tl_irr_reb=output/tl-irr-REB.pdf

output/tl-est-static-dynamic.pdf: \
		src/plot-estimates-over-time.R \
		../apply-estimates/output/svac_new_vars.csv \
		Makefile
	Rscript --vanilla $< \
		--inputfile=../apply-estimates/output/svac_new_vars.csv \
		--outputfile=$@

output/log-boxplot-estimate-dist.txt: \
		src/plot-estimates-distribution.R \
		../apply-estimates/output/svac_new_vars.csv \
		../estimate/hand/CONSTANTS.yaml \
		Makefile
	Rscript --vanilla $< \
		--inputfile=../apply-estimates/output/svac_new_vars.csv \
		--CONSTANTS=../estimate/hand/CONSTANTS.yaml \
		--outputlog=$@

output/log-plot-estimates-by-conflict-actor.txt: \
		src/plot-LVM-estimates-by-conflict-actor-type.R \
		../apply-estimates/output/svac_new_vars.csv \
		../estimate/hand/CONSTANTS.yaml \
		Makefile
	Rscript --vanilla $< \
		--inputfile=../apply-estimates/output/svac_new_vars.csv \
		--CONSTANTS=../estimate/hand/CONSTANTS.yaml \
		--out_log=$@

output/xt-corr-obs-est.tex: \
		src/relate-obs-estimates.R \
		src/corr-function.R \
		../apply-estimates/output/svac_new_vars.csv \
		Makefile
	Rscript --vanilla $< \
		--inputfile=../apply-estimates/output/svac_new_vars.csv \
		--corr_function=src/corr-function.R \
		--xt_out_corr_obs_est=$@

output/tl-corr-obs-est-All-static.pdf: \
		src/relate-obs-est-over-time.R \
		src/corr-function.R \
		../apply-estimates/output/svac_new_vars.csv \
		../estimate/hand/CONSTANTS.yaml \
		Makefile
	Rscript --vanilla $< \
		--inputfile=../apply-estimates/output/svac_new_vars.csv \
		--corr_function=src/corr-function.R \
		--CONSTANTS=../estimate/hand/CONSTANTS.yaml \
		--tl_corr_All_static=$@ \
		--tl_corr_All_dynamic=output/tl-corr-obs-est-All-dynamic.pdf \
		--tl_corr_GOV_static=output/tl-corr-obs-est-GOV-static.pdf \
		--tl_corr_GOV_dynamic=output/tl-corr-obs-est-GOV-dynamic.pdf \
		--tl_corr_REB_static=output/tl-corr-obs-est-REB-static.pdf \
		--tl_corr_REB_dynamic=output/tl-corr-obs-est-REB-dynamic.pdf

output/xt-alpha-beta-params.tex: \
		src/xt-alpha-beta-params.Rnw \
		../estimate/output/static-alphas-betas.Rdata \
		../estimate/output/dynamic-alphas-betas.Rdata \
		Makefile
	Rscript -e "library(knitr); knit('src/xt-alpha-beta-params.Rnw')"
	mv xt-alpha-beta-params.tex output/

#end of Makefile.
