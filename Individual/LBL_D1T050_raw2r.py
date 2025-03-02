#!/bin/python
import numpy as np
import pandas as pd

import os
import sys


def main(argv):
    
    dir_in = '../tmp/D1T050/IAQ_Activity_Monitoring'
    dir_out = '../tmp/D1T050/cfg-annex'
    dir_xls = '../tmp/D1T050/tmp'
    
    os.makedirs(dir_out, exist_ok=True)
    os.makedirs(dir_xls, exist_ok=True)
    
    location = ['BA1', 'BA2', 'BR1', 'BR2', 'IN1', 'KIT', 'OUT']
    mtype = ['CO2', '''NO2',''' 'PM', 'PM1', 'PM25', 'PM10', 'UFP', 'RH', 'T']
    
    def get_unit(mtype):
        if mtype == 'CO2':
            return 'ppm'
        # TODO: no support for NO2 in ppb by annex-86 R script
        #elif mtype == 'NO2':
            #return 'ppb'
        elif mtype in ['PM', 'PM1', 'PM25', 'PM10']:
            return 'ug/m3'
        elif mtype == 'UFP':
            return '#/m3'
        elif mtype == 'RH':
            return '%'
        elif mtype == 'T':
            return 'C'
        return
    
    def map_room(loc):
        if loc == 'BA1':
            return 'BAT1'
        elif loc == 'BA2':
            return 'BAT2'
        elif loc == 'BR1':
            return 'BED1'
        elif loc == 'BR2':
            return 'BED2'
        elif loc == 'IN1':
            return 'OTH'
        elif loc == 'OUT':
            return 'AMB'
        return
    
    # create config
    for fn in os.listdir(dir_in):
    
        if fn.endswith('.csv'):
        
            recs = []
            recs.append({'column': 'Time',
                         'variable': 'datetime',
                         'study': 'empty',
                         'unit': 'empty',
                         'home': 'empty',
                         'room': 'empty'})

            df = pd.read_csv(os.path.join(dir_in, fn))
            
            for col in df.columns:
            
                toks = col.split('_')
                
                if len(toks) == 3 and toks[1] in location and toks[2] in mtype:
                    
                    var = 'PM25' if toks[2] == 'PM' else toks[2]
                    study = 'LBL_D1T050'
                    unit = get_unit(toks[2])
                    home = 'h_{:002d}'.format(int(fn.replace('DataTable', '').replace('.csv', '')))
                    room = map_room(toks[1])
                    
                    recs.append({'column': col,
                                 'variable': var,
                                 'study': study,
                                 'unit': unit,
                                 'home': home,
                                 'room': room})
            
            # crate dataframe
            df = pd.DataFrame().from_records(recs)
            
            # remove douple entries to make sure we get a unique key
            grouped = df.groupby(['variable', 'study', 'home', 'room'])
            drop_index = []
            for name, group in grouped:
                if len(group) > 1:
                    drop_index.extend(group.index[1:])
            
            # drop douple rows
            df.drop(index=drop_index, inplace=True)
            
            # sort and save cfg to file
            df.sort_values(['home', 'variable'], inplace=True)
            df.reset_index(drop=True, inplace=True)
            fn_config = fn.replace('.csv', '.cfg')
            df.to_csv(os.path.join(dir_out, fn_config), index=False)
    
    print('Finished...')


if __name__ == '__main__':
    main(sys.argv)
