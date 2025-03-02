import pandas as pd
import os


def read_data_hengh(filepath, startpoint, endpoint, iteration_counter, study_short, output):
    # Initialize an empty list to store DataFrames
    csv_files = []

    # Loop through the first 10 sequential numbers
    for i in range(startpoint, endpoint):
        # Construct the file name
        file_name_pattern = f"{i}.csv"

        # Find files in the directory that end with {i}.csv
        matching_files = [file for file in os.listdir(filepath) if file.endswith(file_name_pattern)]

        # Check if matching files were found
        if matching_files:
            # Use the first matching file
            file_name = matching_files[0]

            # Append the DataFrame to the list
            csv_files.append(file_name)

    if csv_files:

        # Initialize an empty list to store DataFrames
        dfs = []

        # List of starting strings to keep in columns
        x = ['Time', 'T_', 'RH_', 'CO2_', 'NO2_',  'PM_', 'FRM_']

        # Loop through each CSV file and append its data to the list
        for csv_file in csv_files:
            file_path = os.path.join(filepath, csv_file)
            df = pd.read_csv(file_path, sep=",")

            # Extract the last three characters of the file name (excluding '.csv') as a suffix
            suffix = csv_file[:-4][-3:]

            # Append a suffix to all columns (except the first one)
            df.columns = [df.columns[0]] + [f"{col}_{suffix}" if i != 0 else col for i, col in enumerate(df.columns[1:])]
            df = df.rename(columns={df.columns[1]: df.columns[1] + "_" + suffix})
            # Assuming there's a column named 'datetime_column' containing date-time information
            df['Time'] = pd.to_datetime(df['Time'])

            dfs.append(df)

        # Concatenate the list of DataFrames into a single DataFrame
        combined_data = pd.concat(dfs, ignore_index=True)


        # Sort the combined DataFrame based on the date-time column
        combined_data = combined_data.sort_values(by='Time')

        # Filter columns based on starting strings in the list x
        columns_to_keep = [col for col in combined_data.columns if any(col.startswith(prefix) for prefix in x)]
        combined_data = combined_data[columns_to_keep]

        # Define columns to divide by 1000
        columns_to_divide = combined_data.filter(regex='FRM_').columns

        # Divide selected columns by 1000
        combined_data[columns_to_divide] = \
            combined_data[columns_to_divide].apply(pd.to_numeric, errors='coerce', downcast='integer')
        combined_data[columns_to_divide] = combined_data[columns_to_divide].apply(lambda z: z / 1000)
        combined_data[columns_to_divide] = combined_data[columns_to_divide].astype(str)

        # Get a list from the header of the dataframe
        header_list = combined_data.columns.to_list()
        header_list[0] = "X"
        print(header_list)

        combined_data = combined_data.rename(columns={combined_data.columns[0]: ""})
        # Save the combined DataFrame to a new CSV file
        combined_data.to_csv(output + study_short + '_combined_data_' + str(iteration_counter) + '.csv', sep=";", index=False)
        # Save the DataFrame as a .txt file
        combined_data.to_csv(output + study_short + '_data_' + str(iteration_counter) + '.txt', sep=',', index=False)

    else:
        header_list = []

    return header_list


def read_data_lia(filepath, startpoint, endpoint, iteration_counter, study_short, output):
    # Initialize an empty list to store DataFrames
    csv_files = []

    # Loop through the first 10 sequential numbers
    for i in range(startpoint, endpoint):
        # Construct the file name
        file_name_pattern = f"{i}.csv"

        # Find files in the directory that end with {i}.csv
        matching_files = [file for file in os.listdir(filepath) if file.endswith(file_name_pattern)]

        # Check if matching files were found
        if matching_files:
            # Use the first matching file
            file_name = matching_files[0]

            # Append the DataFrame to the list
            csv_files.append(file_name)

    if csv_files:

        # Initialize an empty list to store DataFrames
        dfs = []

        # List of starting strings to keep in columns
        x = ['Time', 'T_', 'RH_', 'CO2_', 'NO2_', 'PM_', 'FRM_']

        # Define the lists
        list_a = ['CO2', 'FRM', 'NO2', 'PM', 'PM1', 'PM25', 'PM10', 'RH', 'T']
        list_b = ['BA1', 'BA2', 'BA3', 'BA4', 'BA5',
                  'BR1', 'BR2', 'BR3', 'BR4', 'BR5',
                  'IN1', 'OUT', 'KIT', 'AS1']
        list_c = ['AVP', 'CLR', 'ADJ', 'AQS', 'PDR']

        # Loop through each CSV file and append its data to the list
        for csv_file in csv_files:
            file_path = os.path.join(filepath, csv_file)
            df = pd.read_csv(file_path, sep=",")

            # Function to reorder headers
            def reorder_header(header):
                segments = header.split('_')

                # Exclude the first column ("Time") from ordering
                if segments[0].lower() == 'time':
                    return header

                # Convert segments to uppercase
                segments = [seg.upper() for seg in segments]

                # Fill missing segments with 'XXX'
                while len(segments) < 3:
                    segments.append('XXX')

                # Order segments based on the defined lists
                ordered_segments = []
                for counter, segment_list in enumerate([list_a, list_c, list_b]):
                    for segment in segments:
                        if segment in segment_list:
                            ordered_segments.append(segment)
                            # Exit the inner loop
                            break
                    if len(ordered_segments) == counter:
                        ordered_segments.append('XXX')
                    # Continue with the next iteration of the outer loop
                    continue

                # Join segments with '_'
                return '_'.join(ordered_segments)

            # Apply the transformation to all columns
            df.columns = [reorder_header(col) for col in df.columns]

            # Extract the last three characters of the file name (excluding '.csv') as a suffix
            suffix = csv_file[:-4][-3:]

            # Append a suffix to all columns (except the first one)
            df.columns = [df.columns[0]] + [f"{col}_{suffix}" if i != 0 else col for i, col in
                                            enumerate(df.columns[1:])]
            df = df.rename(columns={df.columns[1]: df.columns[1] + "_" + suffix})

            # Assuming there's a column named 'datetime_column' containing date-time information
            date_format = '%d/%m/%y %H:%M'
            df['Time'] = pd.to_datetime(df['Time'], format=date_format, errors='coerce', dayfirst=True)

            # Filter columns based on the condition
            drop_list = []
            for elements in df.columns.to_list():
                if elements.startswith('XXX_'):
                    drop_list.append(elements)
            drop_list = list(dict.fromkeys(drop_list))
            for elements in drop_list:
                df = df.drop(elements, axis=1)

            # Dictionary to keep track of encountered column names
            column_count = {}

            # Function to generate unique column names
            def get_unique_column_name(column_name):
                if column_name in column_count:
                    column_count[column_name] += 1
                    return f"{column_name}{chr(65 + column_count[column_name])}"
                else:
                    column_count[column_name] = 0
                    return column_name

            # Iterate over columns and update names if needed
            df.columns = [get_unique_column_name(col) for col in df.columns]

            dfs.append(df)

        # Concatenate the list of DataFrames into a single DataFrame
        combined_data = pd.concat(dfs, ignore_index=True)

        # Sort the combined DataFrame based on the date-time column
        combined_data = combined_data.sort_values(by='Time')

        # Define columns to divide by 1000
        columns_to_divide = combined_data.filter(regex='FRM_').columns

        # Divide selected columns by 1000
        combined_data[columns_to_divide] = \
            combined_data[columns_to_divide].apply(pd.to_numeric, errors='coerce', downcast='integer')
        combined_data[columns_to_divide] = combined_data[columns_to_divide].apply(lambda z: z / 1000)
        combined_data[columns_to_divide] = combined_data[columns_to_divide].astype(str)

        # Function to check if 'XXX' is present in the third segment
        def has_XXX_in_third_segment(column_name):
            segments = column_name.split('_')
            return len(segments) >= 3 and segments[2] == 'XXX'

        # Filter columns based on the condition
        filtered_columns = [col for col in combined_data.columns if not has_XXX_in_third_segment(col)]

        # Create a new DataFrame with the filtered columns
        combined_data = combined_data[filtered_columns]

        # Specify the substrings you want to check for
        substrings_to_remove = ['_AS1_']

        # Filter columns based on the conditions
        filtered_columns = [col for col in combined_data.columns if
                            not any(substring in col for substring in substrings_to_remove)]

        # Drop rows where Time column is NaN
        combined_data = combined_data.dropna(subset=['Time'])

        # Create a new DataFrame with the filtered columns
        combined_data = combined_data[filtered_columns]

        # Get a list from the header of the dataframe
        header_list = combined_data.columns.to_list()
        header_list[0] = "X"
        print(header_list)

        combined_data = combined_data.rename(columns={combined_data.columns[0]: ""})
        # Save the combined DataFrame to a new CSV file
        combined_data.to_csv(output + study_short + '_combined_data_' + str(iteration_counter) + '.csv', sep=";", index=False)
        # Save the DataFrame as a .txt file
        combined_data.to_csv(output + study_short + '_data_' + str(iteration_counter) + '.txt', sep=',', index=False)

    else:
        header_list = []

    return header_list
