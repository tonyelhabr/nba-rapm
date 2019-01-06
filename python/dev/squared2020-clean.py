
import numpy as np
import pandas as pd

pd.set_option('display.max_rows', 50)
pd.set_option('display.max_columns', 50)
pd.set_option('display.width', 1000)

# file = 'data-raw/play_by_play_with_lineup_2017.csv'
file = 'data/rapm_coefs_cors_grid_2017.csv'
data_raw = pd.read_csv(file, delimiter=',')

print(data_raw.head())
