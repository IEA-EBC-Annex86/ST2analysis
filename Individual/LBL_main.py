from LBL_ReadData import read_data_hengh, read_data_lia
from LBL_ConfigData import config_data_hengh, config_data_lia


if __name__ == '__main__':
    # pre-processing for r-script for study HENGH https://datadryad.org/stash/dataset/doi:10.7941/D1ZS7X
    study = "HENGH"
    study_short = "hengh"
    base = '../data/USA/HENGH/raw/doi_10.7941_D1ZS7X__v5/IAQ_Monitoring/IAQ_Monitoring'
    output = '../data/USA/HENGH/prepro/'
    for i in range(0, 13):
        startpoint = i * 10 + 1
        endpoint = i * 10 + 10
        header_list = read_data_hengh(base, startpoint, endpoint, i, study_short,output)
        config_data_hengh(header_list, study, i, study_short,output)
    # pre-processing for r-script for study low‐income apartments https://datadryad.org/stash/dataset/doi:10.7941/D1T050
    study = "LowIncomeApartments"
    study_short = "lia"
    base = '../data/USA/LIA/raw/doi_10.7941_D1T050__v5/IAQ_Activity_Monitoring/IAQ_Activity_Monitoring'
    output = '../data/USA/LIA/prepro/'
    for i in range(0, 4):
        startpoint = i * 10 + 901
        endpoint = i * 10 + 910
        header_list = read_data_lia(base, startpoint, endpoint, i, study_short,output)
        config_data_lia(header_list, study, i, study_short,output)
