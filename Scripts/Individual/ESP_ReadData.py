import pandas as pd


def read_data_esp_seq(base, study, cities):
    # Read file into a DataFrame
    df = pd.read_csv(base + "230221_BaseDatos_ClimateReady.csv", sep=";")

    # Define the list of column names to keep
    columns_to_keep = ['city', 'dwell_numb', 'room', 'ta1', 'rh', 'co2', 'pm25', 'tg', 'ta2', 'datetime', 'dwelling', 'Tavg', 'Tmax', 'Tmin']

    # Use DataFrame.loc to select and keep only the specified columns
    df = df.loc[:, columns_to_keep]

    # Convert the 'datetime_col' to datetime format with the given format
    df['datetime'] = pd.to_datetime(df['datetime'], format='%d.%m.%Y %H:%M')

    # Convert the datetime column to the desired format
    df['formatted_datetime'] = df['datetime'].dt.strftime('%Y-%m-%d %H:%M:%S')

    # Set the formatted datetime column as the index
    df.set_index('formatted_datetime', inplace=True)

    # Delete a column by name
    df = df.drop('datetime', axis=1)

    # Extract unique elements from the 'Column1' and store them in a list
    unique_elements_list = df['dwelling'].unique().tolist()

    # Create separate lists for elements starting with "alpha" and "beta"
    pamplona_list = [element for element in unique_elements_list if element.startswith("Pamplona")]
    sevilla_list = [element for element in unique_elements_list if element.startswith("Sevilla")]

    city_list = [pamplona_list, sevilla_list]
    dfs = [df, df]
    header_list_final = []
    for i in range(0, len(city_list)):
        merging_list = []
        for ele in city_list[i]:
            # Filter the DataFrame based on a condition
            df_temp = dfs[i][dfs[i]['dwelling'] == ele]

            # Creating a copy of the DataFrame
            sub_df = df_temp.copy()

            # Extract the first element from each column
            columns_list = ['city', 'dwell_numb', 'room']
            first_elements = []
            for elements in columns_list:
                first_elements.append(str(sub_df.iat[1, sub_df.columns.get_loc(elements)]))
            suffix = '_'.join(first_elements)

            # Delete a column by name
            sub_df = sub_df.drop('city', axis=1)
            sub_df = sub_df.drop('dwell_numb', axis=1)
            sub_df = sub_df.drop('room', axis=1)
            sub_df = sub_df.drop('dwelling', axis=1)

            # Append the string to each element in the specified columns
            # List of columns to modify
            columns_to_modify = ['ta1', 'rh', 'co2', 'pm25', 'tg', 'ta2', 'Tavg', 'Tmax', 'Tmin']
            modified_col_list = [item + "_" + suffix for item in columns_to_modify]

            # Create a dictionary to map old column names to new column names
            column_mapping = dict(zip(sub_df.columns, modified_col_list))

            # Use the rename method with the dictionary to rename the columns
            sub_df.rename(columns=column_mapping, inplace=True)
            merging_list.append(sub_df)

        # Merge DataFrames in the list based on the 'formatted_datetime' column
        merged_df = merging_list[0]
        for df in merging_list[1:]:
            merged_df = pd.merge(merged_df, df, on='formatted_datetime', how='outer')

        # Sort the DataFrame by the datetime index
        merged_df = merged_df.sort_index()

        # Rename the index column
        merged_df = merged_df.rename_axis('')

        # Reset the index
        merged_df = merged_df.reset_index()

        # Get a list from the header of the dataframe
        header_list = merged_df.columns.to_list()
        header_list[0] = "X"
        header_list_final.append(header_list)

        # Save the combined DataFrame to a new CSV file
        merged_df.to_csv(study + "_" + cities[i] + '_data.csv', sep=";", index=False)
        # Save the DataFrame as a .txt file
        merged_df.to_csv(study + "_" + cities[i] + '_data.txt', sep=',', index=False)

    return header_list_final


def read_data_esp(base, study):
    # Read file into a DataFrame
    df = pd.read_csv(base + "230221_BaseDatos_ClimateReady.csv", sep=";")

    # Define the list of column names to keep
    columns_to_keep = ['city', 'dwell_numb', 'room', 'ta1', 'rh', 'co2', 'pm25', 'tg', 'ta2', 'datetime', 'dwelling', 'Tavg', 'Tmax', 'Tmin']

    # Use DataFrame.loc to select and keep only the specified columns
    df = df.loc[:, columns_to_keep]

    # Convert the 'datetime_col' to datetime format with the given format
    df['datetime'] = pd.to_datetime(df['datetime'], format='%d.%m.%Y %H:%M')

    # Convert the datetime column to the desired format
    df['formatted_datetime'] = df['datetime'].dt.strftime('%Y-%m-%d %H:%M:%S')

    # Set the formatted datetime column as the index
    df.set_index('formatted_datetime', inplace=True)

    # Delete a column by name
    df = df.drop('datetime', axis=1)

    # Extract unique elements from the 'Column1' and store them in a list
    unique_elements_list = df['dwelling'].unique().tolist()

    merging_list = []
    for ele in unique_elements_list:
        # Filter the DataFrame based on a condition
        df_temp = df[df['dwelling'] == ele]

        # Creating a copy of the DataFrame
        sub_df = df_temp.copy()

        # Extract the first element from each column
        columns_list = ['city', 'dwell_numb', 'room']
        first_elements = []
        for elements in columns_list:
            first_elements.append(str(sub_df.iat[1, sub_df.columns.get_loc(elements)]))
        suffix = '_'.join(first_elements)

        # Delete a column by name
        sub_df = sub_df.drop('city', axis=1)
        sub_df = sub_df.drop('dwell_numb', axis=1)
        sub_df = sub_df.drop('room', axis=1)
        sub_df = sub_df.drop('dwelling', axis=1)

        # Append the string to each element in the specified columns
        # List of columns to modify
        columns_to_modify = ['ta1', 'rh', 'co2', 'pm25', 'tg', 'ta2', 'Tavg', 'Tmax', 'Tmin']
        modified_col_list = [item + "_" + suffix for item in columns_to_modify]

        # Create a dictionary to map old column names to new column names
        column_mapping = dict(zip(sub_df.columns, modified_col_list))

        # Use the rename method with the dictionary to rename the columns
        sub_df.rename(columns=column_mapping, inplace=True)
        merging_list.append(sub_df)

    # Merge DataFrames in the list based on the 'formatted_datetime' column
    merged_df = merging_list[0]
    for df in merging_list[1:]:
        merged_df = pd.merge(merged_df, df, on='formatted_datetime', how='outer')

    # Sort the DataFrame by the datetime index
    merged_df = merged_df.sort_index()

    # Rename the index column
    merged_df = merged_df.rename_axis('')

    # Reset the index
    merged_df = merged_df.reset_index()

    # Get a list from the header of the dataframe
    header_list = merged_df.columns.to_list()
    header_list[0] = "X"

    # Save the combined DataFrame to a new CSV file
    merged_df.to_csv(study + '_data.csv', sep=";", index=False)
    # Save the DataFrame as a .txt file
    merged_df.to_csv(study + '_data.txt', sep=',', index=False)

    return header_list
