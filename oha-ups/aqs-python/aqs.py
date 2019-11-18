import requests
import pandas as pd

# TODO: Complete docstrings with examples.

def aqs_status():
    """Returns the current status of the EPA's Air Quality System (AQS) API.

    :return: Status message from the AQS API.
    """
    api_root = 'https://aqs.epa.gov/data/api/metaData/isAvailable'
    response = requests.get(api_root)
    df = pd.DataFrame(response.json().get('Header'))
    print(df['status'])


def aqs_field_info(field):
    """ Returns the description of the given AQS data field/s.
    Queries the EPA's Air Quality System (AQS) API for field description metadata.

    :param field: A field or list of fields to return descriptions of.
    :return: Description of inputted fields from aqs API.
    """
    api_root = 'https://aqs.epa.gov/data/api/metaData/fieldsByService?email=test@aqs.api&key=test&service=sampleData'
    response = requests.get(api_root)
    df = pd.DataFrame(response.json().get('Data'))
    df = df.set_index('field_name')
    info = df.loc['field', 'field_description']
    print(info)


def state_lookup(state, email, key):
    """ Queries AQS API for lookup codes corresponding to state.
    In addition can look up Guam, Puerto Rico, Virgin Islands, Country of Mexico, and Canada.
    # TODO: Complete docstring.
    :param state: A string or list of strings of US or above listed additional territories.
    :param email:
    :param key:
    :return: The AQS API reference code for the inputted state/s.
    """
    api_root = "https://aqs.epa.gov/data/api/list/states?"
    params_dict = {'email': email, 'key': key}
    response = requests.get(api_root, params=params_dict)
    data = response.json().get('Data')
    state = state.upper()
    state_dict = {}
    for i in data:
        value = i.get('value_represented')
        value = value.upper()
        state_dict[value] = i.get('code')
    return state_dict[state]


def county_lookup(county, state, email, key):
    """
    # TODO: Complete docstring.
    :param county:
    :param state:
    :param email:
    :param key:
    :return:
    """
    api_root = "https://aqs.epa.gov/data/api/list/countiesByState?"
    county = county.upper()
    state_code = state_lookup(state=state, email=email, key=key)
    params_dict = {'email': email, 'key': key, 'state': state_code}
    response = requests.get(api_root, params=params_dict)
    data = response.json().get('Data')
    county_dict = {}
    for i in data:
        value = i.get('value_represented')
        value = value.upper()
        county_dict[value] = i.get('code')
    county_code = county_dict[county]
    return (state_code, county_code)


def get_county_monitors(county, state, start_date, end_date, email, key):
    """
    
    :param county: 
    :param state: 
    :param start_date: Desired start date of monitor operation. Format must be 'YYYYMMDD'.
    :param end_date: Desired last date of monitor operation. Format must be 'YYYYMMDD'.
    :param email: 
    :param key: 
    :return: 
    """
    api_root = "https://aqs.epa.gov/data/api/monitors/byCounty?"
    lookup_tup = county_lookup(county=county, state=state, email=email, key=key)
    param_dict = {'state': lookup_tup[0], 'county': lookup_tup[1], 'email': email, 'key': key, 'param': "88101",
                  'bdate': start_date, 'edate': end_date}
    response = requests.get(api_root, params=param_dict)
    data = response.json().get('Data')


    # TODO: Could also lookup monitors using a bounding box around the site, but what would be the appropriate size of the bounding box.
    # That function would like like this.
def get_nearest_monitors(lon, lat,  bdate, edate):
    """

    :param lon:
    :param lat:
    :param bdate:
    :param edate:
    :return:
    """
    # TODO:
    # First generate a bounding box of a desired size around lon, lat point.
    # Next pass min and max lats and lons of bounding box to params_dict.
    #
    api_root = 'https://aqs.epa.gov/data/api/sampleData/byBox?'
    params_dict = {'email':, 'key':, 'param':88101, 'bdate':, 'edate':, 'minlat':, 'maxlat':, 'minlon':, 'maxlo':}

