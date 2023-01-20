# Norms Reasoner i.e., a model of Moral Intuition and Construction

## Getting Setup
1. Download installer
2. *Do installer things*

## Running the Experiments
*Tell them how to use the interface*

------------------------------------------------------------
## Repo Contents
Norms-Reasoner-v1000
1. Installer for Norms Reasoner application. This will allow you to run the experiments yourself and see the models output and results. This also comes with support to browse the knowledge base by clicking on concepts within logical forms.

norm-mc-tests.lsp
1. runs the MCT task on an agent given a csv of the form: **ID,NL Query,Logical Query,True Label,Principle(s) Involved**

norms-source.lsp
1. norm source fire handlers
	- stmt-implies-stmt: takes in two conjunctions and sees whether the 1st implies the 2nd via subsumption
	- equiv-prop-prop: takes in two conjunctions sees if the two are equiv i.e., stmt-implies-stmt holds in both directions

utils.lsp
1. does necessary logical form encodings and transformations
	- e.g., from variables binded to norm frame slots, it constructs the multiple norm frame logical statements for each slot
	- e.g., conjunction surgery like from conjunction creates ist-information statements to be used with query
2. does norm frame storing and merging

### In ..\dempster-shafer\
------------------------------------------------------------
This directory contains the code that does Dempster Shafer operations to combine evidence.<br/>

dempster-shafer.lsp
1. function: <code>full-dempster-shafer (prop-set frame-of-discernment in-mt &key (verbose nil))</code>
	- returns interval [belief, plausbility] for prop-set given the frame-of-discernment and evidence in in-mt
2. function: <code>full-dempster-shafer-FOD (frame-of-discernment in-mt &key (verbose nil))</code>
	- same as above but returns a hash table of intervals for every prop-set from frame-of-discernment
	- Hash table looks like this: {(evaluation Norm511274 Obligated) : (0.9 . 1.0), (evaluation Norm511274 Optional) : (0.0 . 0.1), (evaluation Norm511274 Impermissble) : (0.0 . 0.1)}

dempster-shafer-source.lsp
1. outsourced predicate handlers for...
	- believes: true for a proposition if center of belief and plausbility >= 0.9
	- mostBelieved: gets proposition with highest center of mass between bel and pl
	- confidenceInterval: returns confidence interval for a given proposition

### All in ..\testing-data\
------------------------------------------------------------
This directory contains the csv files that are used to run the MCT task experiments.<br/>

Adversarial-TestingData.csv
- data used to run mct-task experiment
- of the form: ID,NL Query,Logical Query,True Label,Principle(s) Involved

Normal-TestingData.csv
- same as above but with true evaluative labels flipped
