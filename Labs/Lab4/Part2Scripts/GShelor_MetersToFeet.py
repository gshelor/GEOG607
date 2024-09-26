#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Sep 25 19:35:29 2024

@author: gshelor
"""

### taking input elevation in meters
elev_m = input("Enter an elevation in meters: ")

### converting to ft
elev_ft = float(elev_m) * 3.28084

print(f'The elevation in feet is {round(elev_ft, 1)} feet.')

