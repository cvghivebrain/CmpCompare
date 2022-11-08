Tool for comparing different compression formats. Place your compressors of choice in the ```bin``` folder and select the file you want to test them on.

# bin\commands.ini
To add a compressor which doesn't use the default command, simply add its own command to the ```ini``` file:

<pre>"{program}" "{infile}" "{outfile}"

bin\newcompressor.exe -example "{infile}" "{outfile}"</pre>

In this example, the program has an additional switch.