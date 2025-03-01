# ST2-analysis; Scripts for analysis of Indoor Air Quality data produced according to IEA EBC Annex 86 standard
This repository includes a cellction of scripts and functions that can serve several purposes around the [IEA EBC Annex 86](https://annex86.iea-ebc.org/) IAQ analysis method. Amongst other:

## Analysis of Annex 86 standardized IAQ data (from data repository) / Meta-analysis of many IAQ data sources
A collection of scripts and functions to analyze the Indoor Air Quality (IAQ) dataset produced in project *IEA EBC Annex 86* within subtask 2 (ST2). They are intended to be used on IAQ datasets standardized according to IEA EBC Annex 86 standard and/or produced with the R package [annex](https://github.com/IEA-EBC-Annex86/annex) developed within this project.
The dataset produced during the Annex 86 project is available for download in a [Zenodo repository](https://doi.org/10.5281/zenodo.14917724).

## Processing individual IAQ sources according to Annex 86 procedure
It also collects pre- and postprocessing scripts (in R and Python) that were developed to process the original (raw) timeseries data used within IEA EBC Annex 86 to produce the Zenodo dataset. Included are script for data handling, config file creation and applying of the *annex* package as well as postprocesing the Excel output files (semi)automatically to add meta information. I.e. pre-/postprocessing scripts used to create the dataset on Zenodo. Those might serve as examples for newly available datasets.
