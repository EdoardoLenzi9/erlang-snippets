#!/bin/bash
gnome-terminal -e "erl -sname pong -run ping_pong_dist start_pong"
gnome-terminal -e "erl -sname ping -run ping_pong_dist start_ping"
#erl -sname pong -run ping_pong_dist start_pong
#erl -sname ping -run ping_pong_dist start_ping pong@eddy 