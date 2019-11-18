import thingspeak
import os
import requests
import pandas as pd
import json
from datetime import datetime
import openpyxl
import urllib
from concurrent.futures import ThreadPoolExecutor
import aqi

    # TODO: Timezone offsets.
    # TODO: Figure out transformations that need to be done to data.
    # TODO: Calculating the Purple Air EPA score from the concentration numbers.
    # TODO: Type Annotation

def get_thingspeak_keys(sensors):
    """Retrieves channelID and read key from PurpleAir API for PurpleAir sensor.

    :param sensor: List of 5-digit integer PurpleAir sensor ID's.
    :return: Dictionary containing sensor Labels and Thingspeak channelID and read keys for 'A' and 'B' channels.
    """
    sensor_dict = {}
    API_root = "https://www.purpleair.com/json?"
    # Asserts entered argument is of type list.
    # FIXME: This will be an issue if passing in a Pandas Series.
    assert isinstance(sensors, list), "Sensors should be a list of 5-digit integers."
    # Asserts length of all values in sensors list are of length 5.
    assert any([len(str(x)) == 5 for x in sensors]), "One or more sensors IDs are not 5-digits"
    with ThreadPoolExecutor(max_workers=2) as executor:
        for i in sensors:
            response = requests.get(API_root, params={'show':i})
            assert response.status_code == 200, print(response.status_code)
            # Pulls results dictionary from response JSON.
            results = response.json().get('results')
            # Defines channel ids and read keys for primary and secondary channels.
            device_name = results[0].get('Label')
            channelA_id = results[0].get('THINGSPEAK_PRIMARY_ID')
            channelA_key = results[0].get('THINGSPEAK_PRIMARY_ID_READ_KEY')
            channelB_id = results[1].get('THINGSPEAK_PRIMARY_ID')
            channelB_key = results[1].get('THINGSPEAK_PRIMARY_ID_READ_KEY')
            # Writes primary and secondary channel id's and read keys into dictionary.
            sensor_dict[device_name] = {'A':(channelA_id, channelA_key), 'B':(channelB_id, channelB_key)}
    return(sensor_dict)

def get_thingspeak(channel, **kwargs):
    """Submit GET request to thingspeak API using specified arguments.

    :param channel: An object of class thingspeak.channel.
    :Keyword Arguments:
       start (string): Character string of date in ISO 8601 format to supply to requests.get options argument as start date for channel data.
       end (string): Character string of date in ISO 8601 format to supply to requests.get options argument as end date for channel data.
       days (int): TODO Fill
       average (int): TODO Fill
    :return: pandas DataFrame of sensor readings from Thingspeak API.
    """
    # Thingspeak API may still reject inputs if it doesn't recognize channelID or read_key.
    # TODO: Figure out TimeZone offset (daylight savings changes the offset, would think this is already solved fix).
    # This block confirms all arguments are acceptable.
    expected_args = ('start', 'end', 'days', 'average')
    assert any([i in expected_args for i in
                kwargs.keys()]), "received an unexpected argument. See documentation for list of expected arguments."
    if not ('average' in kwargs.keys()):
        # Appends default argument average=10 to kwargs if not present.
        kwargs.update({'average': 10})
    try:
        response = json.loads(channel.get(options=kwargs))
    except urllib.error.HTTPError as error:
        print(error)
    else:
        # Creates fields object to use for column naming.
        fields = response['channel']
        # Creates data frame from response JSON dictionary.
        df = pd.DataFrame.from_dict(response['feeds']).set_index(['created_at']).rename(columns=fields)
        return (df)

# TODO: Input should be a dictionary, not two channels.

# Testing of final function.
# TODO: Functionalize
# FIXME: Find way to pass down **kwargs.
sensor_dict = get_thingspeak_keys([29709])
start = "2019-10-01 00:00:00"
end = "2019-10-01 06:59:59"

with ThreadPoolExecutor(max_workers=2) as executor:
    for sensor in sensor_dict:
        sensor_df_dict = {}
        for i in sensor_dict[sensor]:
            channel_id = sensor_dict[sensor][i][0]
            read_key = sensor_dict[sensor][i][1]
            # Establishes thingspeak channel object (connection).
            channel = thingspeak.Channel(id=channel_id, api_key=read_key)
            # Submits GET request to thingspeak API using pre-specified arguments.
            df = get_thingspeak(channel, start=start, end=end)
            sensor_df_dict[i] = df
        unused_colsA = ['Uptime', 'RSSI']
        unused_colsB = ['Mem', 'Adc', 'Unused']
        sensor_df_dict['A'] = sensor_df_dict['A'].drop(unused_colsA, axis='columns')
        sensor_df_dict['B'] = sensor_df_dict['B'].drop(unused_colsB, axis='columns')
        filename = sensor
        write_to_excel(sensor_df_dict['A'], sensor_df_dict['B'], filename)
