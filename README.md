Tool for comparing different compression formats. Place your compressors of choice in the ```bin``` folder and select the file you want to test them on.

![CmpCompare](https://user-images.githubusercontent.com/66194501/200449614-ec01d13b-c1fb-4478-b240-5203f8a98756.png)

# bin\commands.ini
To add a compressor which doesn't use the default command, simply add its own command to the ```ini``` file:

<pre>"{program}" "{infile}" "{outfile}"

bin\newcompressor.exe -example "{infile}" "{outfile}"</pre>

In this example, the program has an additional switch. The first line is the default command and shouldn't normally be edited.
