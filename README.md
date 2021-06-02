The main objective of this project is to convert existing whole-cell patch clamp recordings in ABF format into the open NWB standard and publish these openly on the DANDI archive. This repository will be used to raise issues, track progress and ensure the process is reproducible.

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

## Process workflow
The conversion process is iterative since there are data issues with respect to duplicate recording IDs, different recording channels used, different stimulation methods used, incomplete metadata (gain, etc.).

![ephys workflow](https://user-images.githubusercontent.com/3498149/120544066-04194e00-c3bb-11eb-9b2b-a8b1c3c2388d.png)
