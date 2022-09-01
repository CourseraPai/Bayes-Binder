# import libraries
import pandas as pd
import matplotlib.pyplot as plt

# load the data
data_url_1 = "https://gist.githubusercontent.com/mbonsma/8da0990b71ba9a09f7de395574e54df1/raw/aec88b30af87fad8d45da7e774223f91dad09e88/lh_data.csv"
lefthanded_data = pd.read_csv(data_url_1)

# import library
import numpy as np

# create a function for P(LH | A)

def P_lh_given_A(ages_of_death, study_year = 1990):
	

early_1900s_rate = lefthanded_data['Mean_lh'].tail(10).mean()
late_1900s_rate = lefthanded_data['Mean_lh'].head(10).mean()

middle_rates = lefthanded_data.loc[lefthanded_data['Birth_year'].isin(study_year \
																																			- ages_of_death)]['Mean_lh']

youngest_age = study_year - 1986 + 10 # the youngest age is 10
oldest_age = study_year - 1986 + 86 # the oldest age is 86
P_return = np.zeros(ages_of_death.shape)

P_return[ages_of_death > oldest_age] = early_1900s_rate / 100
P_return[ages_of_death < youngest_age] = late_1900s_rate / 100
P_return[np.logical_and((ages_of_death <= oldest_age), \
												(ages_of_death >= youngest_age))] = middle_rates / 100

return P_return


def P_lh(death_distribution_data, study_year=1990): 
	# sum over P_lh for each age group
	""" Overall probability of being left-handed if you died in the study year
    Input: dataframe of death distribution data, study year
    Output: P(LH), a single floating point number """

# multiply number of dead people by P_lh_given_A

p_list = death_distribution_data['Both Sexes'] * \
P_lh_given_A(death_distribution_data['Age'], study_year) 

# calculate the sum of p_list

p = p_list.sum()

# normalize to total number of people (sum of death_distribution_data['Both Sexes'])

return p / death_distribution_data['Both Sexes'].sum()

print(P_lh(death_distribution_data))

def P_A_given_lh(ages_of_death, death_distribution_data, study_year = 1990):
	""" The overall probability of being a particular `age_of_death` 
    given that you're left-handed """

#Overall probality of dying at certain age

P_A = death_distribution_data["Both Sexes"][ages_of_death] / \
np.sum(death_distribution_data["Both Sexes"])

# use P_lh function to get probability of left-handedness overall

P_left = P_lh(death_distribution_data, study_year) 

# use P_lh_given_A to get probability of left-handedness for a certain age 

P_lh_A = P_lh_given_A(ages_of_death, study_year)

return P_lh_A*P_A/P_left

