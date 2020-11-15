In the **apply-estimates task**, we  process the estimates to facilitate visualization and analysis. Specifically, we

* **[merge-estimates.R]**: merge the static and dynamic estimates into on csv file
* **[create-new-vars.R]**: set missing observed values to zero, compute maximum observed prevalence, create lagged variables
* **[populate-from-all-years.R]**: populate missing values (observed and estimated) from all_years,  to  fill in as many missing lagged values as possible. This generates the main SVAC data file with LVM estimates and lagged variables.
* **[run-simple-regressions.R]**: analyze predictive validity of latent variable estimates (Section H in the Online Appendix) by computing ordinal logistic regressions in which each observed human rights indicator, or the overall observed maximum, are treated as the dependent variable, respectively. Right-hand side predictive variables are a 1-year lagged version of the dependent variable, the static or dynamic estimates, respectively.  This analysis generates Tables III-VI in the Online Appendix. Note that we do not set a random seed. The results will therefore vary slightly with each computation.
