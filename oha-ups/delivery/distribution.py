# TODO: Input should be a dictionary, not two channels.
# Testing of final function.
# TODO: Functionalize
# FIXME: Find way to pass down **kwargs.
sensor_dict = get_thingspeak_keys([29709])
start = "2019-10-01 00:00:00"
end = "2019-10-01 06:59:59"


def main(sensor):
    # TODO: Add docstring.

            write_to_excel(sensor_df_dict['A'], sensor_df_dict['B'], filename)
