import pandas as pd
import os
import matplotlib.pyplot as plt

# Sets working directory to project directory.
os.chdir('/mnt/c/Users/iozeroff/OneDrive - Earthwatch/Desktop/Data-Science/Python-Projects/oha-ups/')

aq_data = pd.read_csv('inputs/Red Acre Stow (outside) (42.445953 -71.475113) Primary Real Time 03_27_2019 10_21_2019.csv', parse_dates=True,index_col='created_at')
print(aq_data.head())
print(aq_data.info())
print(aq_data.describe())
print(aq_data.columns)

aq_daily = aq_data.resample('15T').mean()
print(aq_daily.head())
plt.savefig('outputs/graphics/red-acre-stow-all-temp.png')
plt.show()
