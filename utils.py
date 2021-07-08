from pathlib import Path
import pandas as pd
from ipfx.dataset.create import create_ephys_data_set
import matplotlib.pyplot as plt

import pyabf


def summary_df(path, suffix='.abf'):
    # The following provides a dataframe for all abf files found recursively within a directory
    # it outputs the filename, path to file and date created
    all_files = []
    for i in path.rglob(f'*[!.DS_Store]{suffix}'):  # searches for all files recursively excluding those named .DS_Store
        all_files.append((i.name,  i))

    columns = ["file_name", "path"]
    df = pd.DataFrame.from_records(all_files, columns=columns)
    df.path = df.path.astype('str')
    return df


def generate_subject_meta(row):
    meta = {
        'Subject': {
            'age': None,
            'description': f'{row.abf_comments}',
            'species': 'Homo sapiens',
            'subject_id': f'Donor_{row.file_id}'}
    }
    return meta


def load_nwb_ipfx(file_name, ontology):
    print('loading dataset into data structure...')
    print(file_name)
    data_set = create_ephys_data_set(nwb_file=file_name, ontology=ontology)  # loads nwb file into ipfx data structure
    return(data_set)


def plot_nwb_sweep(file_name, ontology, sweep_plot_index=None, sweep_num=0):
    data_set = load_nwb_ipfx(file_name, ontology)
    if sweep_plot_index is None:
        sweep_plot_index = (len(data_set.sweep_table) // 3)*2

    curr_sweep = data_set.sweep_set(sweep_plot_index).sweeps[sweep_num]
    t = curr_sweep.t
    v = curr_sweep.v
    i = curr_sweep.i

    fig, axes = plt.subplots(2, 1, sharex=True)

    fig.suptitle(f'{file_name}', fontsize=12)

    axes[0].plot(t, v)
    axes[0].set_xlim(0, 2)
    axes[0].set_ylabel("Membrane voltage (mV)")
    axes[0].set_title(f'sweep: {sweep_plot_index}')

    axes[1].plot(t, i, c="orange")
    axes[1].set_ylabel("Injected current (pA)")

    return(fig)
