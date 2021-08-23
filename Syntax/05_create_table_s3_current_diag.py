#------------------------------------------------------------------------------#
# Create Table S3 (Current Diagnoses)
# Author: Jianhui Sun
#------------------------------------------------------------------------------#

# Use Python (ver. 3.6.8)

import os
import csv

import pandas as pd
# (ver. 1.3.0)

os.chdir('C:/Users/sjhuv\Desktop/Misc/2021 Jeremy Revision/Data')

patient_record = []
col_name = []

# read in the csv file exported from R
with open('mentalHealthHx_itt.csv') as csv_file:
    csv_reader = csv.reader(csv_file, delimiter=',')
    line_count = 0
    for row in csv_reader:
        if line_count == 0:
            print(f'Column names are {", ".join(row)}')
            col_name = row
            line_count += 1
        else:
            single_patient = {}
            # print(f'\t{row[0]} is in the {row[1]} condition, and has disease {row[3]}.')
            single_patient['ID'] = row[col_name.index('participantId')]
            single_patient['condition'] = row[col_name.index('condition')]
            single_patient['cur_diag'] = row[col_name.index('disorders')]
            single_patient['past_diag'] = row[col_name.index('pastDisorders')]
            single_patient['cur_help'] = row[col_name.index('help')]
            single_patient['past_help'] = row[col_name.index('pastHelp')]
            line_count += 1
            patient_record.append(single_patient)
    print(f'Processed {line_count} lines.')

### NEUTRAL FIFTY_FIFTY_RANDOM POSITIVE_NEGATION POSITIVE FIFTY_FIFTY_BLOCKED

### CodeBook    
# GAD = Generalized anxiety disorder (GAD)
# OCD = Obsessive-compulsive disorder (OCD)
# Panic = Panic Disorder
# Agor = Agoraphobia
# PTSD = Posttraumatic stress disorder
# SAD = Social anxiety disorder
# Phobias = Specific phobia(s)
# dem = Dementia or other cognitive disorder
# subst = Substance-related disorder
# schiz = Schizophrenia or other related disorder
# depression = Depression or depressive disorder
# bipolar = Bipolar disorder
# eating = Eating disorder
# personality = Personality disorder
# 555 = Prefer not to answer
# noDiagnosis = No diagnosis
# other = Other (please specify)
###

# process the current diagnosis: remove all special characters (including meaningless 'u') and split by ','
for single_patient in patient_record:
    
    intermediate = single_patient['cur_diag']
    intermediate = intermediate.translate({ord(i): None for i in "u[]''"})
    intermediate = intermediate.split(", ")
    single_patient['cur_diag'] = intermediate
    
patient_record = pd.DataFrame(patient_record)

### subset the dataframe by conditions
NEUTRAL = pd.DataFrame(patient_record[patient_record['condition'] == 'NEUTRAL'])
FIFTY_FIFTY_RANDOM = pd.DataFrame(patient_record[patient_record['condition'] == 'FIFTY_FIFTY_RANDOM'])
POSITIVE_NEGATION = pd.DataFrame(patient_record[patient_record['condition'] == 'POSITIVE_NEGATION'])
POSITIVE = pd.DataFrame(patient_record[patient_record['condition'] == 'POSITIVE'])
FIFTY_FIFTY_BLOCKED = pd.DataFrame(patient_record[patient_record['condition'] == 'FIFTY_FIFTY_BLOCKED'])

### count the number of participants in each condition
group_size = {'NEUTRAL': len(NEUTRAL), 'FIFTY_FIFTY_RANDOM': len(FIFTY_FIFTY_RANDOM), 
              'POSITIVE_NEGATION': len(POSITIVE_NEGATION), 'POSITIVE': len(POSITIVE), 
              'FIFTY_FIFTY_BLOCKED': len(FIFTY_FIFTY_BLOCKED)}

print(group_size)

### iterate over each condition
### I store the ID of all patients that has one certain disorder in this dictionary
cur_diag_neural = {'GAD': [], 'OCD': [], 'Panic': [], 'Agor': [], 'PSTD': [], 'SAD': [], 'Phobias': [], 
            'dem': [], 'sbst': [], 'schiz': [], 'depression': [], 'bipolar': [], 'eating': [], 'personality': [], 
            '555': [], 'noDiagnosis': [], 'Other': []}
for index, row in NEUTRAL.iterrows():
    diag = row['cur_diag']
    ID = int(row['ID'])
    
    ### iterate over all disorders
    for key in cur_diag_neural:
        if key in diag:
            cur_diag_neural[key].append(ID)
            

cur_diag_fifty_fifty_random = {'GAD': [], 'OCD': [], 'Panic': [], 'Agor': [], 'PSTD': [], 'SAD': [], 'Phobias': [], 
            'dem': [], 'sbst': [], 'schiz': [], 'depression': [], 'bipolar': [], 'eating': [], 'personality': [], 
            '555': [], 'noDiagnosis': [], 'Other': []}
for index, row in FIFTY_FIFTY_RANDOM.iterrows():
    diag = row['cur_diag']
    ID = int(row['ID'])
    ### iterate over all disorders
    for key in cur_diag_fifty_fifty_random:
        if key in diag:
            cur_diag_fifty_fifty_random[key].append(ID)
            
            
cur_diag_positive_negation = {'GAD': [], 'OCD': [], 'Panic': [], 'Agor': [], 'PSTD': [], 'SAD': [], 'Phobias': [], 
            'dem': [], 'sbst': [], 'schiz': [], 'depression': [], 'bipolar': [], 'eating': [], 'personality': [], 
            '555': [], 'noDiagnosis': [], 'Other': []}
for index, row in POSITIVE_NEGATION.iterrows():
    diag = row['cur_diag']
    ID = int(row['ID'])
    ### iterate over all disorders
    for key in cur_diag_positive_negation:
        if key in diag:
            cur_diag_positive_negation[key].append(ID)
            

cur_diag_positive = {'GAD': [], 'OCD': [], 'Panic': [], 'Agor': [], 'PSTD': [], 'SAD': [], 'Phobias': [], 
            'dem': [], 'sbst': [], 'schiz': [], 'depression': [], 'bipolar': [], 'eating': [], 'personality': [], 
            '555': [], 'noDiagnosis': [], 'Other': []}
for index, row in POSITIVE.iterrows():
    diag = row['cur_diag']
    ID = int(row['ID'])
    ### iterate over all disorders
    for key in cur_diag_positive:
        if key in diag:
            cur_diag_positive[key].append(ID)
            

cur_diag_fifty_fifty_blocked = {'GAD': [], 'OCD': [], 'Panic': [], 'Agor': [], 'PSTD': [], 'SAD': [], 'Phobias': [], 
            'dem': [], 'sbst': [], 'schiz': [], 'depression': [], 'bipolar': [], 'eating': [], 'personality': [], 
            '555': [], 'noDiagnosis': [], 'Other': []}
for index, row in FIFTY_FIFTY_BLOCKED.iterrows():
    diag = row['cur_diag']
    ID = int(row['ID'])
    ### iterate over all disorders
    for key in cur_diag_fifty_fifty_blocked:
        if key in diag:
            cur_diag_fifty_fifty_blocked[key].append(ID)
            

       
freq_table_cur_diag = { 
                       'POSITIVE_NEGATION': {'GAD': None, 'OCD': None, 'Panic': None, 'Agor': None, 'PSTD': None, 'SAD': None, 'Phobias': None, 
                                             'dem': None, 'sbst': None, 'schiz': None, 'depression': None, 'bipolar': None, 'eating': None, 'personality': None, 'Other': None, 'noDiagnosis': None, '555': None}, 
                       'POSITIVE': {'GAD': None, 'OCD': None, 'Panic': None, 'Agor': None, 'PSTD': None, 'SAD': None, 'Phobias': None, 
                                    'dem': None, 'sbst': None, 'schiz': None, 'depression': None, 'bipolar': None, 'eating': None, 'personality': None, 'Other': None, 'noDiagnosis': None, '555': None}, 
                       'FIFTY_FIFTY_BLOCKED': {'GAD': None, 'OCD': None, 'Panic': None, 'Agor': None, 'PSTD': None, 'SAD': None, 'Phobias': None, 
                                               'dem': None, 'sbst': None, 'schiz': None, 'depression': None, 'bipolar': None, 'eating': None, 'personality': None, 'Other': None, 'noDiagnosis': None, '555': None},
                       'FIFTY_FIFTY_RANDOM': {'GAD': None, 'OCD': None, 'Panic': None, 'Agor': None, 'PSTD': None, 'SAD': None, 'Phobias': None, 
                                              'dem': None, 'sbst': None, 'schiz': None, 'depression': None, 'bipolar': None, 'eating': None, 'personality': None, 'Other': None, 'noDiagnosis': None, '555': None},
                        'NEUTRAL': {'GAD': None, 'OCD': None, 'Panic': None, 'Agor': None, 'PSTD': None, 'SAD': None, 'Phobias': None, 
                                   'dem': None, 'sbst': None, 'schiz': None, 'depression': None, 'bipolar': None, 'eating': None, 'personality': None, 'Other': None, 'noDiagnosis': None, '555': None}
                     
                       }

#### count the freq of each disorders
print('NEUTRAL')
for key, value in freq_table_cur_diag['NEUTRAL'].items():
    value = len(cur_diag_neural[key]) / group_size['NEUTRAL']
    value = str("{:.1%}".format(value))
    value = float(value.translate({ord(i): None for i in "%"}))
    value = str("{:.1f}".format(value))

    
    freq_table_cur_diag['NEUTRAL'][key] = value

    print(key)
    print(value)
    

    
print('FIFTY_FIFTY_RANDOM')
for key, value in freq_table_cur_diag['FIFTY_FIFTY_RANDOM'].items():
    value = len(cur_diag_fifty_fifty_random[key]) / group_size['FIFTY_FIFTY_RANDOM']
    value = str("{:.1%}".format(value))
    value = float(value.translate({ord(i): None for i in "%"}))
    value = str("{:.1f}".format(value))

    freq_table_cur_diag['FIFTY_FIFTY_RANDOM'][key] = value

    print(key)
    print(value)


print('POSITIVE_NEGATION')
for key, value in freq_table_cur_diag['POSITIVE_NEGATION'].items():
    value = len(cur_diag_positive_negation[key]) / group_size['POSITIVE_NEGATION']
    value = str("{:.1%}".format(value))
    value = float(value.translate({ord(i): None for i in "%"}))    
    value = str("{:.1f}".format(value))

    freq_table_cur_diag['POSITIVE_NEGATION'][key] = value

    print(key)
    print(value)
    

print('POSITIVE')
for key, value in freq_table_cur_diag['POSITIVE'].items():
    value = len(cur_diag_positive[key]) / group_size['POSITIVE']
    value = str("{:.1%}".format(value))
    value = float(value.translate({ord(i): None for i in "%"}))
    value = str("{:.1f}".format(value))

    freq_table_cur_diag['POSITIVE'][key] = value

    print(key)
    print(value)
    

print('FIFTY_FIFTY_BLOCKED')
for key, value in freq_table_cur_diag['FIFTY_FIFTY_BLOCKED'].items():
    value = len(cur_diag_fifty_fifty_blocked[key]) / group_size['FIFTY_FIFTY_BLOCKED']
    value = str("{:.1%}".format(value))
    value = float(value.translate({ord(i): None for i in "%"}))    
    value = str("{:.1f}".format(value))

    freq_table_cur_diag['FIFTY_FIFTY_BLOCKED'][key] = value

    print(key)
    print(value)
    

### transform the dictionary into a table
reformed_dict = {'GAD': [], 'OCD': [], 'Panic': [], 'Agor': [], 'PSTD': [], 'SAD': [], 'Phobias': [], 
                 'dem': [], 'sbst': [], 'schiz': [], 'depression': [], 'bipolar': [], 'eating': [],
                 'personality': [], 'Other': [], 'noDiagnosis': [], '555': []}

for outerKey, innerDict in freq_table_cur_diag.items():
    for innerKey, values in innerDict.items():
        reformed_dict[innerKey].append(values)
  
cur_diag_table = pd.DataFrame.from_dict(reformed_dict, orient='index', columns=['POSITIVE_NEGATION', 'POSITIVE', 'FIFTY_FIFTY_BLOCKED', 'FIFTY_FIFTY_RANDOM', 'NEUTRAL'])
cur_diag_table.to_excel('cur_diag_table.xlsx')
