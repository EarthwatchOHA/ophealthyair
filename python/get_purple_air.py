# Accesses sensor data via purple air API. Can be used for monitoring purposes.

import requests
import json
import pandas as pd
from pandas.io.json import json_normalize

def get_purple_air(sensor_id):
    """GET Requests Purple Air JSON API for sensor or list of sensors"""
    api = "https://www.purpleair.com/json?show="
    for i in range(len(sensor_id) - 1):
        request = api + str(sensor_id[i])
        data = requests.get(request)
        return(data)
sensor_list = [29709]

api = "https://www.purpleair.com/json?show="
request = api + str(29709)
response = requests.get(request)
data = response.json()
data = json_normalize(data['results'])
print(data)
