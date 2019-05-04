#!/bin/bash
gnome-terminal -e "erl -sname server -run messenger start_server"
gnome-terminal -e "erl -sname a -run messenger logonA"
gnome-terminal -e "erl -sname b -run messenger logonB"