# -*- coding: utf-8 -*-
"""
Created on Sun Sep 12 11:01:11 2021

@author: GEGO407607
"""

#2017 Population Estimates - ACS
NVPopulations2017 = ["Carson City: 54745","Churchill: 24230","Clark: 2204079","Douglas: 48309","Elko: 52649","Esmeralda: 850","Eureka: 1961","Humboldt: 16826","Lander: 5693","Lincoln: 5223","Lyon: 54122","Mineral: 4457","Nye: 44202","Pershing: 6508","Storey: 4006","Washoe: 460587","White Pine: 9592"]

### printing the items in the list one by one
for x in NVPopulations2017:
    print(x)
    
    
### making a copy of the list
NVPopulations_reverse = NVPopulations2017.copy()

### sorting the list in reverse
NVPopulations_reverse.sort(reverse = True)

### putting an empty string here in between printing lists so I can read it more easily
print(" ")

### printing the reverse-sorted list items one by one
for y in NVPopulations_reverse:
    print(y)
