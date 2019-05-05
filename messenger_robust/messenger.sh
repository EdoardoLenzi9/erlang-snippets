#!/bin/bash
gnome-terminal -e "erl -sname server -run mess_server start_server"
gnome-terminal -e "erl -sname a -run user_interface logonA"
gnome-terminal -e "erl -sname b -run user_interface logonB"

# user_interface:message(a, "hello").