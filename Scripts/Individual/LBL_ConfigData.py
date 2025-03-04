import pandas as pd


def config_data_hengh(header_list, study, iteration_counter, study_short, output):
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
        df['temp_val'] = ''
        df['temp_id'] = ''

        # Check and update values based on conditions
        df.loc[df['column'].str.startswith('X'), 'variable'] = 'datetime'
        df.loc[df['column'].str.startswith('X'), 'unit'] = 'empty'
        df.loc[df['column'].str.startswith('CO2'), 'variable'] = 'CO2'
        df.loc[df['column'].str.startswith('CO2'), 'unit'] = 'ppm'
        df.loc[df['column'].str.startswith('NO2'), 'variable'] = 'NO2'
        df.loc[df['column'].str.startswith('NO2'), 'unit'] = 'ug/m3'
        df.loc[df['column'].str.startswith('PM'), 'variable'] = 'PM25'
        df.loc[df['column'].str.startswith('PM'), 'unit'] = 'ug/m3'
        df.loc[df['column'].str.startswith('RH'), 'variable'] = 'rH'
        df.loc[df['column'].str.startswith('RH'), 'unit'] = '%'
        df.loc[df['column'].str.startswith('T'), 'variable'] = 'T'
        df.loc[df['column'].str.startswith('T'), 'unit'] = 'C'
        df.loc[df['column'].str.startswith('FRM'), 'variable'] = 'HCHO'
        df.loc[df['column'].str.startswith('FRM'), 'unit'] = 'ppm'

        # Create a new column with the last three characters of 'col1'
        df['home'] = "building_id_" + df['column'].str[-3:]

        # Split each string by "_" and extract the third segment
        df['room'] = df['column'].str.split('_').str[2]

        # special handling for datetime row
        df.loc[df['column'].str.startswith('X'), 'study'] = 'empty'
        df.loc[df['column'].str.startswith('X'), 'home'] = 'empty'
        df.loc[df['column'].str.startswith('X'), 'room'] = 'empty'

        # Replace values in column based on conditions
        df['room'] = df['room'].replace({
            'IN1': 'LAU1',
            'DEN': 'LAU2',
            'BR1': 'BED1', 'BR2': 'BED2', 'BR3': 'BED3', 'BR4': 'BED4', 'BR5': 'BED5', 'BR6': 'BED6',
            'LOFT': 'OTH1',
            'FIR': 'OTH2',
            'LAU': 'OTH3',
            'DNR': 'LIV',
            'BA1': 'BAT1', 'BA2': 'BAT2', 'BA3': 'BAT3', 'BA4': 'BAT4', 'BA5': 'BAT5', 'BA6': 'BAT6',
            'TO1': 'BAT11', 'TO2': 'BAT12', 'TO3': 'BAT13', 'TO4': 'BAT14', 'TO5': 'BAT15', 'TO6': 'BAT16',
            'SW1': 'BAT21',
            'OUT': 'AMB',
        })

        # Add temp data
        df['temp_val'] = df['column'].str.split('_').str[0]
        df['temp_id'] = df['column'].str.split('_').str[3]

        # Function to generate numeric suffix
        def generate_suffix(dfx):
            result = []
            suffix_count = {}

            for index, row in dfx.iterrows():
                key = (row['room'], row['temp_val'], row['temp_id'])

                if key not in suffix_count:
                    suffix_count[key] = 1
                else:
                    suffix_count[key] += 1
                suffix = suffix_count[key] if suffix_count[key] > 1 else ""

                if suffix_count[key] >= 2:
                    # Remove the last character from col1
                    if len(row['room']) > 3:
                        modified_col = row['room'][:-1]
                    else:
                        modified_col = row['room']
                    result.append(f"{modified_col}{suffix}")
                else:
                    result.append(f"{row['room']}{''}")
            return result

        # Apply the function to generate the numeric suffix, resetting numbering when col3 changes
        df['room'] = generate_suffix(df)

        # drop unnecessary columns
        df = df.drop('temp_val', axis=1)
        df = df.drop('temp_id', axis=1)
        print(df)
        # Save the DataFrame as a .txt file
        df.to_csv(output + study_short + '_data_config_' + str(iteration_counter) + '.txt', sep='\t', index=False)
    else:
        print("Iteration " + str(iteration_counter) + " was skipped, no files available for this range")


def config_data_lia(header_list, study, iteration_counter, study_short, output):
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
        df['temp_val'] = ''
        df['temp_id'] = ''

        # Check and update values based on conditions
        df.loc[df['column'].str.startswith('X'), 'variable'] = 'datetime'
        df.loc[df['column'].str.startswith('X'), 'unit'] = 'empty'
        df.loc[df['column'].str.startswith('CO2'), 'variable'] = 'CO2'
        df.loc[df['column'].str.startswith('CO2'), 'unit'] = 'ppm'
        df.loc[df['column'].str.startswith('NO2'), 'variable'] = 'NO2'
        df.loc[df['column'].str.startswith('NO2'), 'unit'] = 'ug/m3'
        df.loc[df['column'].str.startswith('PM'), 'variable'] = 'PMOther'
        df.loc[df['column'].str.startswith('PM'), 'unit'] = 'ug/m3'
        df.loc[df['column'].str.startswith('PM1'), 'variable'] = 'PM1'
        df.loc[df['column'].str.startswith('PM1'), 'unit'] = 'ug/m3'
        df.loc[df['column'].str.startswith('PM10'), 'variable'] = 'PM10'
        df.loc[df['column'].str.startswith('PM10'), 'unit'] = 'ug/m3'
        df.loc[df['column'].str.startswith('PM25'), 'variable'] = 'PM25'
        df.loc[df['column'].str.startswith('PM25'), 'unit'] = 'ug/m3'
        df.loc[df['column'].str.startswith('RH'), 'variable'] = 'rH'
        df.loc[df['column'].str.startswith('RH'), 'unit'] = '%'
        df.loc[df['column'].str.startswith('T'), 'variable'] = 'T'
        df.loc[df['column'].str.startswith('T'), 'unit'] = 'C'
        df.loc[df['column'].str.startswith('FRM'), 'variable'] = 'HCHO'
        df.loc[df['column'].str.startswith('FRM'), 'unit'] = 'ppm'

        # Create a new column with the last three characters of 'col1'
        df['home'] = "building_id_" + df['column'].str.extract(r'(\d{3})(?:[A-Z])?$')

        # Split each string by "_" and extract the third segment
        df['room'] = df['column'].str.split('_').str[2]

        # special handling for datetime row
        df.loc[df['column'].str.startswith('X'), 'study'] = 'empty'
        df.loc[df['column'].str.startswith('X'), 'home'] = 'empty'
        df.loc[df['column'].str.startswith('X'), 'room'] = 'empty'

        # Replace values in column based on conditions
        df['room'] = df['room'].replace({
            'IN1': 'LAU1',
            'BR1': 'BED1', 'BR2': 'BED2', 'BR3': 'BED3', 'BR4': 'BED4', 'BR5': 'BED5', 'BR6': 'BED6',
            'BA1': 'BAT1', 'BA2': 'BAT2', 'BA3': 'BAT3', 'BA4': 'BAT4', 'BA5': 'BAT5', 'BA6': 'BAT6',
            'OUT': 'AMB',
        })

        # Add temp data
        df['temp_val'] = df['column'].str.split('_').str[0]
        df['temp_id'] = df['column'].str.extract(r'(\d{3})(?:[A-Z])?$')

        # Function to generate numeric suffix
        def generate_suffix(dfx):
            result = []
            suffix_count = {}

            for index, row in dfx.iterrows():
                key = (row['room'], row['temp_val'], row['temp_id'])

                if key not in suffix_count:
                    suffix_count[key] = 1
                else:
                    suffix_count[key] += 1
                suffix = suffix_count[key] if suffix_count[key] > 1 else ""

                if suffix_count[key] >= 2:
                    # Remove the last character from col1
                    if len(row['room']) > 3:
                        modified_col = row['room'][:-1]
                    else:
                        modified_col = row['room']
                    result.append(f"{modified_col}{suffix}")
                else:
                    result.append(f"{row['room']}{''}")
            return result

        # Apply the function to generate the numeric suffix, resetting numbering when col3 changes
        df['room'] = generate_suffix(df)

        # drop unnecessary columns
        df = df.drop('temp_val', axis=1)
        df = df.drop('temp_id', axis=1)
        print(df)
        # Save the DataFrame as a .txt file
        df.to_csv(output + study_short + '_data_config_' + str(iteration_counter) + '.txt', sep='\t', index=False)
    else:
        print("Iteration " + str(iteration_counter) + " was skipped, no files available for this range")
