import pandas as pd


def config_data_esp_seq(header_list_final, study, city):
    if header_list_final:
        # Create a DataFrame
        df = pd.DataFrame()

        # Add a new column to the existing DataFrame
        df['column'] = header_list_final

        # Initialize new columns with default values
        df['variable'] = ''
        df['study'] = study
        df['unit'] = ''
        df['home'] = ''
        df['room'] = ''

        # Check and update values based on conditions
        df.loc[df['column'].str.startswith('X'), 'variable'] = 'datetime'
        df.loc[df['column'].str.startswith('X'), 'unit'] = 'empty'
        df.loc[df['column'].str.startswith('ta1'), 'variable'] = 'T'
        df.loc[df['column'].str.startswith('ta1'), 'unit'] = 'C'
        df.loc[df['column'].str.startswith('rh'), 'variable'] = 'rH'
        df.loc[df['column'].str.startswith('rh'), 'unit'] = '%'
        df.loc[df['column'].str.startswith('co2'), 'variable'] = 'CO2'
        df.loc[df['column'].str.startswith('co2'), 'unit'] = 'ppm'
        df.loc[df['column'].str.startswith('pm25'), 'variable'] = 'PM25'
        df.loc[df['column'].str.startswith('pm25'), 'unit'] = 'ug/m3'
        df.loc[df['column'].str.startswith('tg'), 'variable'] = 'T'
        df.loc[df['column'].str.startswith('tg'), 'unit'] = 'C'
        df.loc[df['column'].str.startswith('ta2'), 'variable'] = 'T'
        df.loc[df['column'].str.startswith('ta2'), 'unit'] = 'C'
        df.loc[df['column'].str.startswith('Tavg'), 'variable'] = 'T'
        df.loc[df['column'].str.startswith('Tavg'), 'unit'] = 'C'
        df.loc[df['column'].str.startswith('Tmax'), 'variable'] = 'T'
        df.loc[df['column'].str.startswith('Tmax'), 'unit'] = 'C'
        df.loc[df['column'].str.startswith('Tmin'), 'variable'] = 'T'
        df.loc[df['column'].str.startswith('Tmin'), 'unit'] = 'C

        # Split each string by "_" and extract the segments 1 + 2
        df['home'] = df['column'].str.split('_').str[1] + df['column'].str.split('_').str[2]

        # Split each string by "_" and extract the last segment
        df['room'] = df['column'].str.split('_').str[3]

        # special handling for datetime row
        df.loc[df['column'].str.startswith('X'), 'study'] = 'empty'
        df.loc[df['column'].str.startswith('X'), 'home'] = 'empty'
        df.loc[df['column'].str.startswith('X'), 'room'] = 'empty'

        # Replace values in column based on conditions
        df['room'] = df['room'].replace({
            'Dormitorio': 'BED1',
            'Salon': 'LIV1'
        })

        # Example conditions: Replace 'abc1' in 'room' column when 'status' is 'active'
        df.loc[(df['column'].str.startswith('ta1')) & (df['room'] == 'BED1'), 'room'] = 'BED1'
        df.loc[(df['column'].str.startswith('tg')) & (df['room'] == 'BED1'), 'room'] = 'BED2'
        df.loc[(df['column'].str.startswith('ta2')) & (df['room'] == 'BED1'), 'room'] = 'BED3'
        df.loc[(df['column'].str.startswith('ta1')) & (df['room'] == 'LIV1'), 'room'] = 'LIV1'
        df.loc[(df['column'].str.startswith('tg')) & (df['room'] == 'LIV1'), 'room'] = 'LIV2'
        df.loc[(df['column'].str.startswith('ta2')) & (df['room'] == 'LIV1'), 'room'] = 'LIV3'
        df.loc[(df['column'].str.startswith('Tavg')) & (df['room'] == 'BED1'), 'room'] = 'AMB'
        df.loc[(df['column'].str.startswith('Tavg')) & (df['room'] == 'LIV1'), 'room'] = 'AMB'
        df.loc[(df['column'].str.startswith('Tmin')) & (df['room'] == 'BED1'), 'room'] = 'AMB'
        df.loc[(df['column'].str.startswith('Tmin')) & (df['room'] == 'LIV1'), 'room'] = 'AMB'
        df.loc[(df['column'].str.startswith('Tmax')) & (df['room'] == 'BED1'), 'room'] = 'AMB'
        df.loc[(df['column'].str.startswith('Tmax')) & (df['room'] == 'LIV1'), 'room'] = 'AMB'

        # Save the DataFrame as a .txt file
        df.to_csv(study + "_" + city + '_data_config.txt', sep='\t', index=False)


def config_data_esp(header_list, study):
    if header_list:
        # Create a DataFrame
        df = pd.DataFrame()

        # Add a new column to the existing DataFrame
        df['column'] = header_list

        # Initialize new columns with default values
        df['variable'] = ''
        df['study'] = study
        df['unit'] = ''
        df['home'] = ''
        df['room'] = ''

        # Check and update values based on conditions
        df.loc[df['column'].str.startswith('X'), 'variable'] = 'datetime'
        df.loc[df['column'].str.startswith('X'), 'unit'] = 'empty'
        df.loc[df['column'].str.startswith('ta1'), 'variable'] = 'T'
        df.loc[df['column'].str.startswith('ta1'), 'unit'] = 'C'
        df.loc[df['column'].str.startswith('rh'), 'variable'] = 'rH'
        df.loc[df['column'].str.startswith('rh'), 'unit'] = '%'
        df.loc[df['column'].str.startswith('co2'), 'variable'] = 'CO2'
        df.loc[df['column'].str.startswith('co2'), 'unit'] = 'ppm'
        df.loc[df['column'].str.startswith('pm25'), 'variable'] = 'PM25'
        df.loc[df['column'].str.startswith('pm25'), 'unit'] = 'ug/m3'
        df.loc[df['column'].str.startswith('tg'), 'variable'] = 'T'
        df.loc[df['column'].str.startswith('tg'), 'unit'] = 'C'
        df.loc[df['column'].str.startswith('ta2'), 'variable'] = 'T'
        df.loc[df['column'].str.startswith('ta2'), 'unit'] = 'C'
        df.loc[df['column'].str.startswith('Tavg'), 'variable'] = 'T'
        df.loc[df['column'].str.startswith('Tavg'), 'unit'] = 'C'
        df.loc[df['column'].str.startswith('Tmax'), 'variable'] = 'T'
        df.loc[df['column'].str.startswith('Tmax'), 'unit'] = 'C'
        df.loc[df['column'].str.startswith('Tmin'), 'variable'] = 'T'
        df.loc[df['column'].str.startswith('Tmin'), 'unit'] = 'C

        # Split each string by "_" and extract the segments 1 + 2
        df['home'] = df['column'].str.split('_').str[1] + df['column'].str.split('_').str[2]

        # Split each string by "_" and extract the last segment
        df['room'] = df['column'].str.split('_').str[3]

        # special handling for datetime row
        df.loc[df['column'].str.startswith('X'), 'study'] = 'empty'
        df.loc[df['column'].str.startswith('X'), 'home'] = 'empty'
        df.loc[df['column'].str.startswith('X'), 'room'] = 'empty'

        # Replace values in column based on conditions
        df['room'] = df['room'].replace({
            'Dormitorio': 'BED1',
            'Salon': 'LIV1'
        })

        # Example conditions: Replace 'abc1' in 'room' column when 'status' is 'active'
        df.loc[(df['column'].str.startswith('ta1')) & (df['room'] == 'BED1'), 'room'] = 'BED1'
        df.loc[(df['column'].str.startswith('tg')) & (df['room'] == 'BED1'), 'room'] = 'BED2'
        df.loc[(df['column'].str.startswith('ta2')) & (df['room'] == 'BED1'), 'room'] = 'BED3'
        df.loc[(df['column'].str.startswith('ta1')) & (df['room'] == 'LIV1'), 'room'] = 'LIV1'
        df.loc[(df['column'].str.startswith('tg')) & (df['room'] == 'LIV1'), 'room'] = 'LIV2'
        df.loc[(df['column'].str.startswith('ta2')) & (df['room'] == 'LIV1'), 'room'] = 'LIV3'
        df.loc[(df['column'].str.startswith('Tavg')) & (df['room'] == 'BED1'), 'room'] = 'AMB'
        df.loc[(df['column'].str.startswith('Tavg')) & (df['room'] == 'LIV1'), 'room'] = 'AMB'
        df.loc[(df['column'].str.startswith('Tmin')) & (df['room'] == 'BED1'), 'room'] = 'AMB'
        df.loc[(df['column'].str.startswith('Tmin')) & (df['room'] == 'LIV1'), 'room'] = 'AMB'
        df.loc[(df['column'].str.startswith('Tmax')) & (df['room'] == 'BED1'), 'room'] = 'AMB'
        df.loc[(df['column'].str.startswith('Tmax')) & (df['room'] == 'LIV1'), 'room'] = 'AMB'

        # Save the DataFrame as a .txt file
        df.to_csv(study + '_data_config.txt', sep='\t', index=False)
