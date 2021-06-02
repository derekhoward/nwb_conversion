This project documents the process of converting a set of .ABF files for intracellular electrophysiology recordings to the NWB standard format.

## Setup
To setup the conda environment after cloning this repo:
```
conda env create -f environment.yml
conda activate nwbephys
```

We are using a forked and modified version of the [x-to-nwb](https://github.com/derekhoward/x-to-nwb) repo. 
To use the modified version, fork and clone that version of the repo and then install the dev version of the library from the directory:
```
pip install -e /path/to/x-to-nwb
```

The updates made to the *x-to-nwb* package also depend on the [ndx-dandi-icephys](https://github.com/catalystneuro/ndx-dandi-icephys) repo. You can try to install it with 
```
pip install ndx-dandi-icephys
```
or alternatively clone the repo to your local machine and install the development version:
```
pip install -e /path/to/ndx-dandi-icephys
```
