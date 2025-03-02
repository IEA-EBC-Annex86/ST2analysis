#!\bin\python
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.dates

import os
import sys


def main(argv):
    
    dir_in  = '../data/GBR/Ventilation standards study'
    dir_cfg = '../tmp/VentStdStudy/cfg'
    dir_dat = '../tmp/VentStdStudy/data'
    dir_int = '../tmp/VentStdStudy/intermediate'
    
    # create folders
    os.makedirs(dir_cfg, exist_ok=True)
    os.makedirs(dir_dat, exist_ok=True)
    os.makedirs(dir_int, exist_ok=True)
    
    ### load pm data
    dir_x = os.path.join(dir_in, 'WP4. Mar-Apr 2022 (Temp, RH, CO2, PM) x9/1 raw data sensors')
    
    dfs = []
    for fn in os.listdir(dir_x):
        if os.path.isfile(os.path.join(dir_x, fn)):
            
            print(fn)
            home = int(fn.split(' ')[1].replace('.csv', ''))
            df = pd.read_csv(f'{dir_x}/{fn}')
            df['tst'] = pd.to_datetime(df['time'])
            df['home'] = f'H_{home:02d}'
            dfs.append(df)
    
    df = pd.concat(dfs)
    df.drop(columns='time', inplace=True)
    df.drop(columns='IAQ', inplace=True)
    df.drop(columns='Battery Voltage', inplace=True)
    
    # change column names
    cols = []
    for col in df.columns:
        if col == 'CO2 PPM':
            cols.append('living_co2_2')
        elif col == 'Humidity':
            cols.append('living_rh_2')
        elif col == 'Temperature':
            cols.append('living_temp_2')
        elif col == 'PM10':
            cols.append('living_pm10')
        elif col == 'PM2.5':
            cols.append('living_pm2.5')
        elif col == 'PM4.0':
            cols.append('living_pm4.0')
        else:
            cols.append(col)
    df.columns = cols
    
    df.info()
    # write data to file
    df.sort_values(['home', 'tst'], inplace=True)
    df.reset_index(drop=True, inplace=True)
    df.to_csv(f'{dir_int}/pm_dat.csv', index=False)
    
    # load pm data
    df_pm = pd.read_csv(f'{dir_int}/pm_dat.csv')
    
    ### load temp, rh, co2
    dir_x = os.path.join(dir_in, 'WP3. 2021-2022 (temp, RH, CO2) x16')
    
    for dirn in os.listdir(dir_x):
        dirname = os.path.join(dir_x, dirn)
        if os.path.isdir(dirname):
            
            # create data file
            dfs = []
            for fn in os.listdir(dirname):
            
                room_type = ''
                if '_Living' in fn:
                    room_type = 'living'
                elif '_Bedroom 1_' in fn:
                    room_type = 'bedroom1'
                elif '_Bedroom 2_' in fn:
                    room_type = 'bedroom2'
                elif '_Bedroom 3_' in fn:
                    room_type = 'bedroom3'
                elif '_Kitchen' in fn:
                    room_type = 'kitchen'
                elif '_Outside' in fn:
                    room_type = 'outside'
                
                unit = ''
                if '_CO2_' in fn:
                    unit = 'co2'
                elif '_T_' in fn:
                    unit = 'temp'
                elif '_RH_' in fn:
                    unit = 'rh'
                
                home = fn.split('_')[0].replace('H', '')
                
                if room_type == '' or unit == '' or home == '':
                    print(fn)
                    raise Exception
                
                df = pd.read_csv(f'{dirname}/{fn}')
                df.columns = ['tst', f'{room_type}_{unit}']
                df.drop_duplicates('tst', inplace=True)
                df.set_index('tst', inplace=True)
                dfs.append(df)
            
            df = pd.concat(dfs, axis=1)
            
            # add pm data if available
            # print(len(df))
            df_t = df_pm[df_pm['home'] == f'H_{int(home):02d}'].copy()
            #df_t.drop(columns='home', inplace=True)
            #df_t.drop(columns='living_co2', inplace=True)
            #df_t.drop(columns='living_rh', inplace=True)
            #df_t.drop(columns='living_temp', inplace=True)
            df_t.drop_duplicates('tst', inplace=True)
            df_t.set_index('tst', inplace=True)
            df_t.sort_index(inplace=True)
            df = pd.concat([df, df_t], axis=1)
            # print(len(df))
            # print()
            
            df.sort_index(inplace=True)
            df.to_csv(f'{dir_dat}/H_{int(home):02d}.csv', index_label='tst')
            
            # create config file
            recs = []
            
            recs.append({
                'column':      'tst',
                'variable':    'datetime',
                'study':       'empty',
                'unit':        'empty',
                'home':        'empty',
                'room':        'empty',
            })
            
            typing = {
                'co2':      ('CO2', 'ppm'),
                'temp':     ('T', 'C'),
                'rh':       ('RH', '%'),
                'pm10':     ('PM10', 'ug/m3'),      # unit is a educated guess
                'pm2.5':    ('PM25', 'ug/m3'),
                #'pm4.0':    ('PMOther', 'ppm'), # combination (variable, study, home, room) must be unique
            }
            r_type = {
                'living':   'LIV',
                'kitchen':  'KIT',
                'outside':  'AMB',
                'bedroom1': 'BED1',
                'bedroom2': 'BED2',
                'bedroom3': 'BED3',
            }
            
            for col in df.columns:
                tok = col.split('_')
                if len(tok) > 1 and tok[1] in typing.keys():
                    recs.append({
                        'column':   col,
                        'variable': typing[tok[1]][0],
                        'study':    'VentStdStudy',
                        'unit':     typing[tok[1]][1],
                        'home':     f'H_{int(home):02d}',
                        'room':     r_type[tok[0]],
                    })
            
            df = pd.DataFrame.from_records(recs)
            df.to_csv(f'{dir_cfg}/H_{int(home):02d}.cfg', index=False)


if __name__ == '__main__':
    main(sys.argv)
