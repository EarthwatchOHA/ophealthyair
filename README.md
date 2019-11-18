# oha-ups
To Do:
Figure out downsampling from 2 minutes to 10 minutes. (See Note below)
Continue cleaning and functionalizing jupyter-notebook for pipeline.
Turn into .py files.
Create plotting script.
Add Command Line Argument Parsing

Notes:
"A much better approach than to read more than 8000 points in a loop is to downsample the data and store it in another channel.

So, let's say you are getting raw data from your sensors every 15 seconds. You could set up a timecontrol that runs every hour and averages this data and saves it to another channel. If you needed averaging at a lower resolution, you could further resample the 1 hour averaged data to every day and write to another channel, etc.

The main advantage of this approach is that it allows you to operate on smaller amounts of data, which is much more efficient and quick than operating on large arrays of data.

To give an analogy, if you had to count the number of grains of sand in a bucket of sand, rather than count each grain, a better approach would be to count the grains in a small cup, and count the number of cups in the bucket. The second is an approximation, but it is a good enough approximation that you can hope to complete the task in a reasonable amount of time than spending a few years on something that doesn't need to be that precise."
