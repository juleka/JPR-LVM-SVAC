In this pre-processing step, we prepare the SVAC data for analysis.

The performed cleaning steps include:
* __[fix-types.R]__: code internationalized intrastate conflicts as INTRA, make
  all actors in INTERstate conflicts of type STATE
* __[uncode-conflict-years.R]__: recode SVAC conflict years as 'not
conflict' if not listed in UCDP/PRIO data as a conflict year
* __[update-var-values.R]__: update conflict/actor country names from
hand/CONSTANTS.yaml
* __[aggregate-state-non-state.R]__: aggregate  non-state conflict-actor-year observations
