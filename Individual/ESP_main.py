from ESP_ReadData import read_data_esp_seq, read_data_esp
from ESP_ConfigData import config_data_esp_seq, config_data_esp


# pre-processing for r-script
study = "Output-Data/ClimateReady"
base = 'data/ES_AuroraMonge/'

# seq-function used to create a file per city
cities = ["Pamplona", "Sevilla"]
header_list_seq = read_data_esp_seq(base, study, cities)
for i in range(0, len(cities)):
    config_data_esp_seq(header_list_seq[i], study, cities[i])

# all data in one file
header_list = read_data_esp(base, study)
config_data_esp(header_list, study)