# Performance Comparison of alternative deployment architectures

Datasets and Scripts to replicate the experiments reported in the manuscript: "Multi-level Scalability Assessment of Microservices Applications based on Actor-driven Decomposition" submitted for possible publication to the Journal of Systems and Software, Elsevier (2021).


### Data

In the "DATA" folder there are four set of experiments. Each dataset is saved in an excel file with naming convention 

```
iter<i>_<type>-profile_<date>.xlsx
```
where iter<i> indicates the iteration and its number: 1 or 2 and type is a boolean "bal" or "unbal"

###R scripts
there are five scripts:

* `start.R'
* computeThreshold.R; 
* computeDM_FUNCTIONS.R
* NewSpiders.R	
* WilcoxonAnalysis.R

that perform the analysis and create the plots. 

###HOW TO RUN

There are few ways to run the scripts. 

Download the whole folder 
* Run `start.R' in RStudio 
* Uncomment the following lines in `start.R'

```
#args = commandArgs(trailingOnly=TRUE)
#workingDirectory<-args[1]
#RScript start.R <path to workingDirectory> 
```

and run with command 
  
```
RScript start.R <path to workingDirectory> 
```

*Run start.Rmd in RStudio
*Run start.ipynb from command line (run it from the folder directory "PerformanceComparison")

```
Jupiter notebook start.ipynb
```
  
###Output

the output is contained in the folder "Results" and includes the failing services per architecture deployment and iteration, the plots (Radar, Ridge, Ploygons) and the Wicoxon tests per component
