#------------------------------------------------------------------------------#
# Create Table S3 (Past Help)
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
# psychiatrist = Psychiatrist
# psychologist = Psychologist
# school_counselor = School counselor
# LMHC = Licensed mental health practitioner
# general_practitioner = General practitioner (e.g. family doctor)
# teacher = Teacher
# family = Family member
# friend = Friend
# religious_leader = Religious leader
# coach = Coach
# book = Self-help book or blog/online post
# medicine = Prescription medicine
# online = Online treatment
# app = Mobile (phone or tablet) application
# support_group = Support group
# 555 = Prefer not to answer
# NotGottenHelp = I am not seeking help
# Other = Other (Please specify)
###
    
# process the past help: remove all special characters (including meaningless 'u') and split by ','
for single_patient in patient_record:
    
    intermediate = single_patient['past_help']
    intermediate = intermediate.translate({ord(i): None for i in "u[]''"})
    intermediate = intermediate.split(", ")
    single_patient['past_help'] = intermediate
    
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
### I store the ID of all patients that has one certain help in this dictionary
past_help_neural = {'psychiatrist': [], 'psychologist': [], 'school_conselor': [], 'LMHC': [], 'general_practitioner': [],
            'teacher': [], 'family': [], 'friend': [], 'religios_leader': [], 'coach': [], 'book': [], 'medicine': [], 
            'online': [], 'app': [], 'spport_grop': [], '555': [], 'NotGottenHelp': [], 'Other': []}

for index, row in NEUTRAL.iterrows():
    diag = row['past_help']
    ID = int(row['ID'])
    
    ### iterate over all helps
    for key in past_help_neural:
        if key in diag:
            past_help_neural[key].append(ID)
            

past_help_fifty_fifty_random = {'psychiatrist': [], 'psychologist': [], 'school_conselor': [], 'LMHC': [], 'general_practitioner': [],
            'teacher': [], 'family': [], 'friend': [], 'religios_leader': [], 'coach': [], 'book': [], 'medicine': [], 
            'online': [], 'app': [], 'spport_grop': [], '555': [], 'NotGottenHelp': [], 'Other': []}

for index, row in FIFTY_FIFTY_RANDOM.iterrows():
    diag = row['past_help']
    ID = int(row['ID'])
    ### iterate over all helps
    for key in past_help_fifty_fifty_random:
        if key in diag:
            past_help_fifty_fifty_random[key].append(ID)
            
            
past_help_positive_negation = {'psychiatrist': [], 'psychologist': [], 'school_conselor': [], 'LMHC': [], 'general_practitioner': [],
            'teacher': [], 'family': [], 'friend': [], 'religios_leader': [], 'coach': [], 'book': [], 'medicine': [], 
            'online': [], 'app': [], 'spport_grop': [], '555': [], 'NotGottenHelp': [], 'Other': []}

for index, row in POSITIVE_NEGATION.iterrows():
    diag = row['past_help']
    ID = int(row['ID'])
    ### iterate over all helps
    for key in past_help_positive_negation:
        if key in diag:
            past_help_positive_negation[key].append(ID)
            

past_help_positive = {'psychiatrist': [], 'psychologist': [], 'school_conselor': [], 'LMHC': [], 'general_practitioner': [],
            'teacher': [], 'family': [], 'friend': [], 'religios_leader': [], 'coach': [], 'book': [], 'medicine': [], 
            'online': [], 'app': [], 'spport_grop': [], '555': [], 'NotGottenHelp': [], 'Other': []}

for index, row in POSITIVE.iterrows():
    diag = row['past_help']
    ID = int(row['ID'])
    ### iterate over all helps
    for key in past_help_positive:
        if key in diag:
            past_help_positive[key].append(ID)
            

past_help_fifty_fifty_blocked = {'psychiatrist': [], 'psychologist': [], 'school_conselor': [], 'LMHC': [], 'general_practitioner': [],
            'teacher': [], 'family': [], 'friend': [], 'religios_leader': [], 'coach': [], 'book': [], 'medicine': [], 
            'online': [], 'app': [], 'spport_grop': [], '555': [], 'NotGottenHelp': [], 'Other': []}

for index, row in FIFTY_FIFTY_BLOCKED.iterrows():
    diag = row['past_help']
    ID = int(row['ID'])
    ### iterate over all helps
    for key in past_help_fifty_fifty_blocked:
        if key in diag:
            past_help_fifty_fifty_blocked[key].append(ID)


freq_table_past_help = {
                        
                       'POSITIVE_NEGATION': {'psychiatrist': None, 'psychologist': None, 'school_conselor': None, 'LMHC': None, 'general_practitioner': None, 'teacher': None, 'family': None, 
                                             'friend': None, 'religios_leader': None, 'coach': None, 'book': None, 'medicine': None, 
                                             'online': None, 'app': None, 'spport_grop': None, 'Other': None, 'NotGottenHelp': None, '555': None},
                       'POSITIVE': {'psychiatrist': None, 'psychologist': None, 'school_conselor': None, 'LMHC': None, 'general_practitioner': None, 'teacher': None, 'family': None, 
                                    'friend': None, 'religios_leader': None, 'coach': None, 'book': None, 'medicine': None, 
                                    'online': None, 'app': None, 'spport_grop': None, 'Other': None, 'NotGottenHelp': None, '555': None},
                       'FIFTY_FIFTY_BLOCKED': {'psychiatrist': None, 'psychologist': None, 'school_conselor': None, 'LMHC': None, 'general_practitioner': None, 'teacher': None, 'family': None, 
                                               'friend': None, 'religios_leader': None, 'coach': None, 'book': None, 'medicine': None, 
                                               'online': None, 'app': None, 'spport_grop': None, 'Other': None, 'NotGottenHelp': None, '555': None},
                       'FIFTY_FIFTY_RANDOM': {'psychiatrist': None, 'psychologist': None, 'school_conselor': None, 'LMHC': None, 'general_practitioner': None, 'teacher': None, 'family': None, 
                                              'friend': None, 'religios_leader': None, 'coach': None, 'book': None, 'medicine': None, 
                                              'online': None, 'app': None, 'spport_grop': None, 'Other': None, 'NotGottenHelp': None, '555': None},
                       'NEUTRAL': {'psychiatrist': None, 'psychologist': None, 'school_conselor': None, 'LMHC': None, 'general_practitioner': None, 'teacher': None, 'family': None, 
                                   'friend': None, 'religios_leader': None, 'coach': None, 'book': None, 'medicine': None, 
                                   'online': None, 'app': None, 'spport_grop': None, 'Other': None, 'NotGottenHelp': None, '555': None}}


#### count the freq of each helps
print('NEUTRAL')
for key, value in freq_table_past_help['NEUTRAL'].items():
    value = len(past_help_neural[key]) / group_size['NEUTRAL']
    value = str("{:.1%}".format(value))
    value = float(value.translate({ord(i): None for i in "%"}))    
    value = str("{:.1f}".format(value))

    freq_table_past_help['NEUTRAL'][key] = value

    print(key)
    print(value)
    
    
print('FIFTY_FIFTY_RANDOM')
for key, value in freq_table_past_help['FIFTY_FIFTY_RANDOM'].items():
    value = len(past_help_fifty_fifty_random[key]) / group_size['FIFTY_FIFTY_RANDOM']
    value = str("{:.1%}".format(value))
    value = float(value.translate({ord(i): None for i in "%"}))    
    value = str("{:.1f}".format(value))

    freq_table_past_help['FIFTY_FIFTY_RANDOM'][key] = value

    print(key)
    print(value)


print('POSITIVE_NEGATION')
for key, value in freq_table_past_help['POSITIVE_NEGATION'].items():
    value = len(past_help_positive_negation[key]) / group_size['POSITIVE_NEGATION']
    value = str("{:.1%}".format(value))
    value = float(value.translate({ord(i): None for i in "%"}))    
    value = str("{:.1f}".format(value))

    freq_table_past_help['POSITIVE_NEGATION'][key] = value

    print(key)
    print(value)
    

print('POSITIVE')
for key, value in freq_table_past_help['POSITIVE'].items():
    value = len(past_help_positive[key]) / group_size['POSITIVE']
    value = str("{:.1%}".format(value))
    value = float(value.translate({ord(i): None for i in "%"}))    
    value = str("{:.1f}".format(value))

    freq_table_past_help['POSITIVE'][key] = value

    print(key)
    print(value)
    

print('FIFTY_FIFTY_BLOCKED')
for key, value in freq_table_past_help['FIFTY_FIFTY_BLOCKED'].items():
    value = len(past_help_fifty_fifty_blocked[key]) / group_size['FIFTY_FIFTY_BLOCKED']
    value = str("{:.1%}".format(value))
    value = float(value.translate({ord(i): None for i in "%"}))
    value = str("{:.1f}".format(value))

    freq_table_past_help['FIFTY_FIFTY_BLOCKED'][key] = value

    print(key)
    print(value)
    
### transform the dictionary into a table
reformed_dict = {'psychiatrist': [], 'psychologist': [], 'school_conselor': [], 'LMHC': [], 'general_practitioner': [], 'teacher': [], 'family': [], 
                 'friend': [], 'religios_leader': [], 'coach': [], 'book': [], 'medicine': [], 
                 'online': [], 'app': [], 'spport_grop': [], 'Other': [], 'NotGottenHelp': [], '555': []}

for outerKey, innerDict in freq_table_past_help.items():
    for innerKey, values in innerDict.items():
        reformed_dict[innerKey].append(values)
  
past_help_table = pd.DataFrame.from_dict(reformed_dict, orient='index', columns=['POSITIVE_NEGATION', 'POSITIVE', 'FIFTY_FIFTY_BLOCKED', 'FIFTY_FIFTY_RANDOM', 'NEUTRAL'])
past_help_table.to_excel('past_help_table.xlsx')
