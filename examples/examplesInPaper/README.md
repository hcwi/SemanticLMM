Exemplary semantic models of LMM analysis
=========================================

Below the datasets and linear mixed model (LMM) analyses of examplary datasets, given in paper of Ćwiek-Kupczyńska et al. (2019, submitted), are provided. The examples demonstrate the use of semLMM package for construction of semantic models of LMM analysis, and generate files containing the resulting RDF graphs.


**Example1** is a toy dataset with two factors (Treatment, Block) and one observed variable (y).
The data is imported from *example1.txt* file and analysed in R with lme4/nlme packages. The results are directly parsed and transformed to a semantic model with semLMM package (see *example1.R*). The output is serialized as an RDF graph in TriG syntax to *example1.trig* file.

**Example2** is a toy example with two factors (Treatment, Block) and four observed variables (y1-y4).
The data, given in *example2.txt*, is analysed externally with Genstat software (not shown). The output of the analysis is given in *example2_Genstat.out* file. Based on the textual output, a manual construction of a semantic model utilising classes from semLMM package is shown in *example2.R*. The output is serialized as an RDF graph in TriG syntax to *example2.trig* file.

**Example3** is a real experimental dataset, obtained in Polapgen project (http://polapgen.pl) and available in ISA-Tab format at PlantPheno database: http://cropnet.pl/plantphenodb/index.php?id=250. The datasets consists of three factors (InfraspecificName, StudyStart, Replication) and ten observed variables (TGW, GW_m2, GW_MS, HD, main_spike, main_stem, G_MS, TL, Wvpd, Precipitation).
The data is imported from *example3.txt* file and analysed in R with lme4/nlme packages. The results are directly parsed and transformed to a semantic model with semLMM package (see *example3.R*). The output is serialized as an RDF graph in TriG syntax to *example3.trig* file. Additionally, a smaller analysis is provided for a subset of example3 data as *example3_subset.trig*.
