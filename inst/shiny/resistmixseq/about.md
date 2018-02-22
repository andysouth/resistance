A model to investigate the evolution of resistance to two insecticides used sequentially or in a mixture. Described in this [Malaria Journal](https://doi.org/10.1186/s12936-018-2203-y) article.

Warning! This is a research tool and not for making operational decisions.

Andy South <a href="https://twitter.com/southmapr">(@southmapr)</a> and Ian Hastings [2018]


**To use**
* Select 'UI'  
* Modify inputs using the sliders for each scenario and insecticide
* Press the 'Run ...' buttons to replot either scenario 

The curves on the plots show the change in frequency of the allele giving resistance to insecticide1 (red) and insecticide2 (blue). A dashed line indicates use in a sequence and a solid line use in a mixture. If the allele frequencies are identical the curve appears purple. 

The inputs that can be changed are :

Model Input      | Description
------------------------- | ----------------------------------------------------
1. Start Freq.  | starting frequency of this resistance allele in the population
2. Exposure | proportion of insects exposed to insecticide
3. Effectiveness | proportion of susceptible (SS) insects killed by exposure to insecticide
4. R. restoration  | Resistance restoration, ability of resistance (RR) to restore fitness of insects exposed to insecticide
5. Dominance of r. | Dominance of resistance, sets fitness of heterozygous (SR) insects between that of SS & RR in presence of insecticide
6. Cost  |  reduction in fitness of resistant (RR) insects in absence of insecticide
7. Dominance of cost  | sets fitness of heterozygous (SR) insects between that of SS & RR in absence of insecticide    


**References**

South, A., & Hastings, I. M. (2018). Insecticide resistance evolution with mixtures and sequences : a model-based explanation. Malaria Journal. [https://doi.org/10.1186/s12936-018-2203-y](https://doi.org/10.1186/s12936-018-2203-y)

Levick, B., South, A., & Hastings, I. M. (2017). A two-locus model of the evolution of insecticide resistance to inform and optimise public health insecticide deployment strategies. PLoS Computational Biology, 13, e1005327. [https://doi.org/10.1371/journal.pcbi.1005327](https://doi.org/10.1371/journal.pcbi.1005327).
    
**Code**   
[repository](https://github.com/AndySouth/resistance)    
[UI](https://github.com/AndySouth/resistance/tree/master/inst/shiny/resistmixseq)    
[paper](https://github.com/AndySouth/resistance/blob/master/inst/documents/paper2/paper2_resistance_mechanisms_mixtures.Rmd)    



