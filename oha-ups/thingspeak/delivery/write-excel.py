def write_to_excel(channel_A, channel_B, filename):
    """Write inputted pandas DataFrames to excel files with sheets for Channels A and B.

    :param channel_A: pandas DataFrame of Channel A readings from a specific PurpleAir sensor.
    :param channel_B: pandas DataFrame of Channel B readings from a specific PurpleAir sensor.
    :param filename: Character string label for PurpleAir sensor.
    """
    d = datetime.today()
    assert isinstance(channel_A, pd.DataFrame), "channel_A should be a pandas DataFrame."
    assert isinstance(channel_B, pd.DataFrame), "channel_A should be a pandas DataFrame."
    assert isinstance(filename, str), "Filename passed to write_df is not a character string."
    pathname = 'outputs/datasets/' + filename + "-" + str(d.strftime('%Y%m%d'))
    full_path = pathname + ".xlsx"
    with pd.ExcelWriter(full_path) as writer:  # doctest: +SKIP
        channel_A.to_excel(writer, sheet_name='Channel A')
        channel_B.to_excel(writer, sheet_name='Channel B')

# Define parameters to be used in build testing.
sensor_list = [29709]
start = '2019-10-01T00:00:00' # If either of the dates are set as None, datetime.strptime fails.
end = '2019-10-23T23:59:59'
channel_id = 742875
read_key = 'JC4FCUJOPST94F65'
channel = thingspeak.Channel(id=channel_id, api_key=read_key)
df = get_thingspeak(channel, start=start, end=end)