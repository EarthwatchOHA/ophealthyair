import json
import urllib
import pandas as pd
import requests
import thingspeak


# TODO: Figure out transformations that need to be done to data.
# TODO: Calculating the Purple Air EPA score from the concentration numbers.
# TODO: Type Annotation
# TODO: Add CLI

def get_thingspeak_keys(sensor_list: list) -> dict:
    """Retrieves channelID and read key from PurpleAir API for PurpleAir sensor.

    :param sensor_list: List of 5-digit integer PurpleAir sensor ID's.
    :return: Dictionary containing sensor Labels and Thingspeak channelID and read keys for 'A' and 'B' channels.
    """
    sensor_dict = {}
    api_root = "https://www.purpleair.com/json?"
    # Asserts entered argument is of type list.
    # FIXME: This will be an issue if passing in a Pandas Series.
    assert isinstance(sensor_list, list), "Sensors should be a list of 5-digit integers."
    # Asserts length of all values in sensors list are of length 5.
    assert any([len(str(x)) == 5 for x in sensor_list]), "One or more sensors IDs are not 5-digits"
    for i in sensor_list:
        response = requests.get(api_root, params={'show': i})
        assert response.status_code == 200, print(response.status_code)
        # Pulls results dictionary from response JSON.
        results = response.json().get('results')
        # Defines channel ids and read keys for primary and secondary channels.
        device_name = results[0].get('Label')
        id_a = results[0].get('THINGSPEAK_PRIMARY_ID')
        key_a = results[0].get('THINGSPEAK_PRIMARY_ID_READ_KEY')
        id_b = results[1].get('THINGSPEAK_PRIMARY_ID')
        key_b = results[1].get('THINGSPEAK_PRIMARY_ID_READ_KEY')
        # Writes primary and secondary channel id's and read keys into dictionary.
        sensor_dict[device_name] = {'A': (id_a, key_a), 'B': (id_b, key_b)}
    return sensor_dict


def get_thingspeak(channel: thingspeak.Channel, **kwargs) -> pd.DataFrame:
    """Submit GET request to thingspeak API using specified arguments.

    :param channel: An object of class thingspeak.channel.
    :param **kwargs: Keyword arguments are passed to thingspeak API as options for the GET request. Expected arguments
    to be supplied are: start (char, ISO 8601 date), end (char, ISO 8601 date), and average (number of minutes to be
    used for resampling).
    :return: pandas DataFrame of sensor readings from Thingspeak API.
    """
    # Thingspeak API may still reject inputs if it doesn't recognize channelID or read_key.
    # TODO: Figure out TimeZone offset (daylight savings changes the offset, would think this is already solved fix).
    # This block confirms all arguments are acceptable.
    expected_args = ('start', 'end', 'average')
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
        return df

# TODO: Note that sensor dates and times are returned in UTC.
