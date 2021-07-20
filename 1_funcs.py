##########################################################################
##########################################################################
################# Python functions for Patient MD Communication Analysis
##########################################################################
##########################################################################

import pandas as pd
import numpy as np
import os
from datetime import datetime
import sys
import os
from docx import Document
from multiprocessing import Pool, cpu_count
from contextlib import closing

##########################################################################
################# Reading in transcript files
##########################################################################

# Used in parallel... reads in one transcript file
def get_it(fname):
    document = Document(fname)
    fname = fname.split('/')[-1]
    print(str(fname)+' with '+str(len(document.tables))+' tables...')

    trans_df = pd.DataFrame(data=None,index=range(len(document.tables[1].rows)))
    trans_df['Sequence'] = trans_df.index
    trans_df['File'] = str(fname.split("(")[0] if "(" in fname else fname.replace('.docx',''))
    i=0
    while i < len(document.tables[1].rows):
        speaker = document.tables[1].row_cells(i)[0].text.encode('utf-8').strip()
        line = document.tables[1].row_cells(i)[1].text.encode('utf-8').strip()
        if speaker:
            trans_df.loc[i,'Speaker'] = speaker
            trans_df.loc[i,'Text'] = line#document.tables[1].row_cells(i)[1].text
        elif line:
            trans_df.loc[i-1,'Text'] = str(trans_df.loc[i-1,'Text']) + ' '.join(line)
        else: pass
        i+=1
    return trans_df

# calls get_it in parallel
def get_it_parallel_izer(func,f_list,num_processes):
    '''Takes a list of file locations and reads them in'''
    if num_processes==None:
        num_processes = min(len(f_list), cpu_count())
    with closing(Pool(num_processes)) as pool:
        results_list = pool.map(func, f_list)
        pool.terminate()
    return pd.concat(results_list)#, axis=1)

# This takes a top level folder and pulls all data in files into dataframe
def get_transcripts(dir_path):
    # Create a list of all of the transcript files to read if __name__ == '__main__':
    directories = [dir_path]
    f_list = []
    for directory in directories:
        for fname in os.listdir(directory):
            if fname.endswith('.docx') and not fname.startswith('~') and not fname.startswith('VL database documentation'):
                f_list.append(os.path.join(directory,fname))

    num_processes = 10

    return(
    get_it_parallel_izer(get_it,f_list,num_processes)
    )

##########################################################################
################# Clean up transcript data
##########################################################################
