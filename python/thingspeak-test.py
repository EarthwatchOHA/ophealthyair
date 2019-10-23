import thingspeak
import pandas as pd

channel_id = 742875
read_key = 'JC4FCUJOPST94F65'

def get_historic_pa (channel_id, read_key):
    channel = thingspeak.Channel(id=channel_id, api_key=read_key)

try:
    # Get the last 2 results from field 1 of your channel
    print(channel.get_field(field='field1', options = {'results': 2}))
    # Get the age of the last data in field 1 of your channel in seconds
    print(channel.get_last_data_age(field='field1'))
    # Get the last data in field 1 of your channel
    print(channel.get_field_last(field='field1'))
except:
    raise
    print("connection failed")



df = pd.read_csv("https://www.purpleair.com/sensorlist?key=P998ADU3D1YJVZQ8&show=29767")
print(df.head())
