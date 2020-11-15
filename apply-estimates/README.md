In the **apply-estimates task**, we  process the estimates to facilitate visualization and analysis. Specifically, we

* **[merge-estimates.R]**: merge the static and dynamic estimates into on csv file
* **[create-new-vars.R]**: set missing observed values to zero, compute maximum observed prevalence, create lagged variables
* **[populate-from-all-years.R]**: populates missing values (observed and estimated) from all_years,  to help fill in as many missing lagged values as possible
* **[run-simple-regressions.R]**: analyzes predictive validity of latent variable estimates (Section H in the Online Appendix_) by computing ordinal logistic regressions in which each observed human rights indicator, or the overall maximum, is treated as the dependent variable. Right-hand side predictive variables are a 1-year lagged version of the dependent variable, the static or dynamic estimates.  This analysis generates Tables III-VI in the Online Appendix. Note that we do not set a random seeds. The results will therefore vary slightly with each computation.
