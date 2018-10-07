# pgedata

Tool for translating PG&E data into influxdb wire format.

This is similar to https://github.com/tomdee/GreenButton, except I ran
into some time handling issues, so I just wrote a new thing.

Download your PG&E Green Button data.  Run this tool with a "site"
argument, taking the csv on stdin.  It will spit out influxdb wire
format on stdout.
