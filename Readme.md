The influence of debut age, relative age effect and country rank on the
re-selection process in European basketball youth national teams
================
2020-07-09

This is repo for the article “The influence of debut age, relative age
effect and country rank on the re-selection process in European
basketball youth national teams”. All code is written in `R`, uses
[`drake`](https://docs.ropensci.org/drake/) to manage the workflow, and
[`renv`](https://rstudio.github.io/renv) to manage needed packages.

## Re-run the analysis

The easiest way to run the analysis on your local computer is:

1.  Download the files by clicking on `OSF Storage (Germany -
    Frankfurt)` and then `Download as zip`.
2.  Open the project file `basketball-reselection.Rproj` in
    [RStudio](https://rstudio.com),
3.  Run `drake::r_make()` to re-run the analysis. Notice that it will
    only do so if there are parts of the workflow that are not up to
    date (which they should all be).

You can check if all parts are up to date by running
`drake::r_vis_drake_graph()`. If you wish to re-run the full analysis
from scratch, run `drake::clean()` before `drake::r_make()`,

## Project organisation

The project has the following organisation:

    ├── Readme.md                     # This file which explains the project.
    ├── _drake.R                      # The file drake::r_make() uses to run everything.
    ├── basketball-reselection.Rproj  # RStudio project file.
    ├── renv.lock                     # Info of package versions used.
    │
    ├── R                             # Contains the analysis plan and all functions.
    ├── data                          # Contains the data and data explanation for the analysis.
    ├── manuscript                    # Contains the manuscript and files to produce it.
    ├── output                        # Contains tables, figures and other results.
    ├── .drake                        # Contains the workflow catch used by drake.
    └── renv                          # Contains files for renv to use correct package versions.

## Workflow information

The workflow was last ran 2020-07-09 and with the following session
info.

    ## R version 4.0.0 (2020-04-24)
    ## Platform: x86_64-apple-darwin17.0 (64-bit)
    ## Running under: macOS Catalina 10.15.5
    ## 
    ## Matrix products: default
    ## BLAS:   /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRblas.dylib
    ## LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices datasets  utils     methods   base     
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] Rcpp_1.0.4.6      compiler_4.0.0    pillar_1.4.4      prettyunits_1.1.1
    ##  [5] tools_4.0.0       progress_1.2.2    digest_0.6.25     evaluate_0.14    
    ##  [9] tibble_3.0.1      lifecycle_0.2.0   pkgconfig_2.0.3   rlang_0.4.6      
    ## [13] igraph_1.2.5      cli_2.0.2         rstudioapi_0.11   filelock_1.0.2   
    ## [17] yaml_2.2.1        parallel_4.0.0    xfun_0.14         storr_1.2.1      
    ## [21] stringr_1.4.0     knitr_1.28        vctrs_0.3.1       hms_0.5.3        
    ## [25] drake_7.12.2      glue_1.4.1        R6_2.4.1          fansi_0.4.1      
    ## [29] base64url_1.4     rmarkdown_2.3     txtq_0.2.0        magrittr_1.5     
    ## [33] backports_1.1.8   htmltools_0.5.0   ellipsis_0.3.1    fst_0.9.2        
    ## [37] assertthat_0.2.1  renv_0.10.0       stringi_1.4.6     crayon_1.3.4
