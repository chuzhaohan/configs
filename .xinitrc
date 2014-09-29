#!/bin/bash

# set display calibration
dispwin -d 1 /home/deepak/lgpanel.icc

# set CAPS LOCK to Ctrl
setxkbmap -option ctrl:nocaps

export GTK_IM_MODULE=xim
export XCOMPOSE=~/.XCompose
