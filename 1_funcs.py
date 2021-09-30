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
################# Scripts for 'smoothing' speakers... NOT integrated yet.
##########################################################################

def speaker_smoother(df_chunk):
    '''This finds contiguous segments with the same speaker, and combines the text'''
    first = True
    cols = ['File','Speaker','Text','Sequence']
    new_chunk = pd.DataFrame(columns=cols,data=None)
    df_chunk.sort_values('Sequence',axis=0,ascending=True, inplace=True)
    df_chunk.reset_index(drop=True,inplace=True)
    for i,row in df_chunk.iterrows():
        if first:
            first = False
        elif row['Speaker'] == df_chunk.loc[i-1,'Speaker']:
            #combine the two chunks of text for the same speakers
            if new_chunk.empty:
                new_chunk = new_chunk.append({'File':row.File,'Speaker':row.Speaker,'Text':str(str(row.Text)+' '+str(df_chunk.loc[i-1,'Text'])),'Sequence':df_chunk.loc[i-1,'Sequence']}, ignore_index=True)
            else:
                new_chunk['Text'].iloc[-1] = str(new_chunk['Text'].iloc[-1])+' '+str(row.Text)
            print('Smoothed one...')
        else:
            new_chunk = new_chunk.append(row, ignore_index=True)
    return new_chunk

def spkr_smthr_paralellizer(func,df,num_processes):
    '''Takes a function and dataframe, chunks up by file'''
    if num_processes==None:
        num_processes = cpu_count()#min(df.shape[1], cpu_count())
    with closing(Pool(num_processes)) as pool:
        # creates list of data frames for each badge
        df_chunks = [df[df.File == F].copy() for F in df.File.unique()]
        results_list = pool.map(func, df_chunks)#, chunksize=1)
        pool.terminate()
        return pd.concat(results_list)#, axis=1)

# num_processes = 12
# df_trans_smoothed = spkr_smthr_paralellizer(speaker_smoother,df_trans,num_processes)
# df_trans_smoothed.head()


##########################################################################
################# Scripts for generating speach features and LSM
##########################################################################

def vader(row):
    # applies vader to both TbyT and Grouped df's
    sid = SentimentIntensityAnalyzer()

    ss = sid.polarity_scores(row['Text'])
    row['Vader_neg'] = ss['neg']
    row['Vader_neu'] = ss['neu']
    row['Vader_pos'] = ss['pos']
    row['Vader_compound'] = ss['compound']
    return row

cols = ['Vader_neg','Vader_neu','Vader_pos','Vader_compound']
for col in cols:
    df_Grouped[col] = np.nan
    df_TbyT[col] = np.nan

# df_Grouped = df_Grouped.apply(vader,axis=1)
# df_TbyT = df_TbyT.apply(vader,axis=1)


# create all turn level autocorrelations and varriances
def big_agg(group):
    print 'got this group... '+str(group.File.unique())
    d = {}
    features = ['Vader_compound','Vader_neg','Vader_neu','Vader_pos','WC','Analytic','Clout','Authentic','Tone','Sixltr',
                'function','ppron','ipron', 'article', 'prep', 'auxverb', 'adverb','negate',
                'conj','quant']
    i_list = []
    for feature in features:
        d[str('TbyT_'+feature+'_autocorr')] = group.sort_values(by=['Sequence'],inplace=False,ascending=True)[feature].autocorr(lag=1)
        d[str('TbyT_'+feature+'_var')] = group[feature].var()
        i_list.append('TbyT_'+feature+'_autocorr')
        i_list.append('TbyT_'+feature+'_var')
    return pd.Series(d, index=i_list)

# df_TbyT_agg = df_TbyT.groupby(['File']).apply(big_agg)
# df_TbyT_agg.head()

# Create conversation level metrics for LSM
def LSM(conv):
    '''This checks to make sure there are just two speakers, and does LSM calc if only 2.
        Works on conversation level data... all speaker's text aggregated for one encounter '''
    if len(conv) == 2:
        return 1-(abs(conv.iloc[0]-conv.iloc[1])/(conv.iloc[0]+conv.iloc[1]+.0001))
    else:
        print conv
        pass

# LSM_feature = ['ppron','ipron', 'article', 'prep', 'auxverb', 'adverb','negate','conj','quant']
#
# df_Grouped
# df_LSM = df_Grouped.groupby('File')[LSM_feature].apply(LSM)
# df_LSM.columns = ['Conv_LSM_'+s for s in LSM_feature]
# df_LSM['Conv_LSM_Avg'] = df_LSM.mean(axis=1)
# df_LSM.index.name = 'File'
# df_LSM.describe(percentiles = [.25, .5, .75],include='all')
#df_LSM.head()

# Create spaker specific metrics
# df_pivot = df_Grouped.pivot(index='File',columns='Speaker')
# df_pivot.columns = ['_'.join(col).rstrip('_') for col in df_pivot.columns.values]
# df_pivot.drop(labels=['Text_D','Text_P'],axis=1,inplace=True)
# df_pivot.head()
