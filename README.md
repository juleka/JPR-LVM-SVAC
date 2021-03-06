# JPR-LVM-SVAC
This repository provides the Online Appendix, replication data and materials for Krüger, Jule &amp; Ragnhild Nordås (2020) [A latent variable  approach to measuring wartime sexual violence](https://journals.sagepub.com/doi/full/10.1177/0022343320961147). *Journal of Peace Research* 57(6): 728–739.

The empirical analysis in this repository was completed using [R](https://www.r-project.org/) version 3.5.2 (2018-12-20) and [rstan](https://github.com/stan-dev/rstan) (Version 2.17.3, GitRev: 2e1f913d3ca3).

The tasks should be run in this order:
* import
* clean
* estimate
* apply-estimates
* visualize

**Research Abstract:**
Conflict-related sexual violence is an international security problem and is sometimes used as a weapon of war. It is also a complex and hard-to-observe phenomenon, constituting perhaps one of the most hidden forms of wartime violence. Latent variable models (LVM) offer a promising avenue to account for differences in observed measures. Three annual human rights sources report on the sexual violence practices of armed conflict actors around the world since 1989 and were coded into ordinal indicators of conflict-year prevalence. Because information diverges significantly across these measures, we currently have a poor scientific understanding with regard to trends and patterns of the problem. In this article, we use an LVM approach to leverage information across multiple indicators of wartime sexual violence to estimate its true extent, to express uncertainty in the form of a credible interval, and to account for temporal trends in the underlying data. We argue that a dynamic LVM parametrization constitutes the best fit in this context. It outperforms a static latent variable model, as well as analysis of observed indicators. Based on our findings, we argue that an LVM approach currently constitutes the best practice for this line of inquiry and conclude with suggestions for future research.
