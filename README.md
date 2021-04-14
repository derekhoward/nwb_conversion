This project documents the process of converting a set of .ABF files for intracellular electrophysiology recordings to the NWB standard format.

## Setup
To setup the conda environment after cloning this repo:
```
conda env create -f environment.yml
conda activate nwbephys
```

We are using a modified version of the [x-to-nwb](https://github.com/derekhoward/x-to-nwb) repo. 
To use the modified version, clone that version of the repo and then from the directory:
```
pip install -e /path/to/x-to-nwb
```