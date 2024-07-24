# Early-life Arsenic Exposure and Influenza-like Illness among Infants in Rural Northern Bangladesh

This project uses g-computation, a causal inference technique, to estimate how hypothetical interventions to reduce arsenic exposure during pregnancy may affect the incidence of influenza-like illness (the co-occurrence of high fever and cough, which is commonly used to monitor influenza activity) among infants in rural northern Bangladesh. Most of the scripts in this repo address data management and table and figure generation. The core of the analysis (g-computation using quasi-Poisson regression models) is implemented by the `gcomp` function defined in [01A_setup_functions.R](01A_setup_functions.R) and called in [02C_gcomp.R](02C_gcomp.R). 

The analysis was conducted using data from the enrollment visit of the Pregnancy, Arsenic, and Immune Response (PAIR) Study. The cohort profile for the PAIR Study has been published in *[Paediatric and Perinatal Epidemiology](https://doi.org/10.1111/ppe.12949)*. For more information on g-computation, see my review of causal inference for early-life environmental exposures published in *[Current Environmental Health Reports](https://doi.org/10.1007/s40572-022-00388-y)* (free PDF [here](https://www.tylerjssmith.com/uploads/Smith_et_al_2022_EstimatingCausalEffects.pdf)).

