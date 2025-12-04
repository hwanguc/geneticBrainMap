# geneticBrainMap

This respiratory contains the Python (Jupyter Notebook) and R code for extracting brain volumes using SynthSeg, modelling the brain volume growth curve using [Generalized Additive Models for Location, Scale and Shape](https://www.gamlss.com/), and mapping gene expressions of post-mortem samples to brain templates using [abagen](https://abagen.readthedocs.io/en/stable/).

**./archive/** - Archived files.


**./data/** - Data files.

**_./data/dat_grin2a_brainvol.csv_** - Brain volume measures obtained using the SynthSeg package.

**_./data/expression.csv_** - Gene experession matrix obtained with the abagen package.

All other files under this folder are deprecated.


**./notebooks/** - Jupyter Notebooks.

**_./notebooks/cre_braingrowthcurve_plots_grin2a_withIXI.ipynb_** - General Additive Models of brain growth curves in Python (Deprecated. Using _gamlss_agesex.R_ under **./scripts** is prefered).

**_./notebooks/geneBrainMap_dk_atlas.ipynb_** - Plotting genetic expression maps of post-mortem samples on brain templates using abagen.



**./outputs/** - Figures and outputs from the GAM (Python) growth curve pipeline.


**./scripts/** - Scripts and Models.

**_./scripts/0_copy_t1_multi-datasets_**: Copy T1 files for different batches of data (e.g., IXI, GRIN2A-Aus) to the folder used for SynthSeg.

**_./scripts/combine_ixi_volumes_withscannerinfo.py_**: Generate a spreadsheet combining individual extracted volume measures from the IXI dataset.

**_./scripts/gamlss_agesex.R_**: GAMLSS growth curve model of the brain volume measures.

All other files under this folder are deprecated.

