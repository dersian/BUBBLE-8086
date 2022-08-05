# -*- coding: utf-8 -*-
"""
Created on Thu Apr  7 14:26:53 2022

@author: Seppe
"""
l=[]
j = False
counter = 0

for i in range(512):
    if (counter == 32):
        j = not j
        counter = 0
    if (j == False):
        if i % 2:
            l.append(0)
        else:
            l.append(1)
    if (j == True):
        if i % 2:
            l.append(1)
        else:
            l.append(0)
    counter += 1
print(l)
        