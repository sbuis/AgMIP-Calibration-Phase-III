# AgMIP-Calibration-Phase-III

This repository has been created to gather useful functions, scripts, examples and possibly data for participants of the AgMIP Calibration Phase III protocol who will use the CroptimizR R package. 

What does it contain?

* AgMIP-Calibration-Phase-III.Rproj, the R project. Open it in RStudio before working on your application.
* main_script.R, a template to be filled for the application of the AgMIP calibration phase III protocol on your own model and configuration. To use it you need a model wrapper for the CroptimizR package for your model (refer to the [Guidelines for implementing a crop model R wrapper for CroptimizR](https://sticsrpacks.github.io/CroptimizR/articles/Designing_a_model_wrapper.html)).
* main_script_Stics.R, an example for the Stics model (associated data and model not provided => cannot be run without them, it is just to show an example)
* R folder: contains functions used in main_script.R

What does it provide?

* main_script.R applies the forward selection algorithm both for AICc and BIC criteria using Nelder-Mead simplex minimization of Ordinary-Least-Squares for a given study case (e.g. French dataset cultivar Apache or French dataset cultivar Bermude, or Australian dataset).
* it automatically generates table4 as required by the AgMIP phase III protocol.

How to proceed?

* If, from your knowledge, your model has not yet been interfaced with CroptimizR, please send an email to "samuel DOT buis AT inrae DOT fr" precising that you are interested in using CroptimizR for the AgMIP phaseIII exercise and giving the name and version of your model. Maybe some colleagues have already developped an interface for CroptimizR or would like to.

* If your model has already been interfaced with CroptimizR:

  * download the repository (green button "Code")
  
  * adapt main_script.R to your case
  
  * execute main_script.R in Rstudio by setting maxeval to a low value (see comment in main_script.R) and check if everything seems fine (e.g., no error nor warning messages ...)
  
  * execute main_script.R in Rstudio setting maxeval to a high value (>500)
 


