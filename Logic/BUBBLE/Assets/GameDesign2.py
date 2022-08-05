# -*- coding: utf-8 -*-
"""
Created on Tue Apr  5 16:07:50 2022
___________________________________________________________________________/"""

"""Offset grid"""
import matplotlib.pyplot as plt
import numpy as np
import random

def Getcolor():
    return random.choice(tuple((40,50,60,70,80)))


gridX = 32 
gridY = 20
wall = 7
counter = 0
game = np.zeros((gridY,gridX))
a = Getcolor()
for y in range(gridY):
    for x in range(gridX):              
        if(y > wall):
            pass

        else:               #offet row logic
            if(y%2 == 1):
                if(x == gridX-2 or x == gridX-1): #63+1 - 64 = 0 > should be zero in second row, 62 + 1 = 63 > should be zero in second row
                    pass
                else:
                    game[y][x+1] = a  # +1 for offset only if y%2 = 1
                    counter += 1
            else:   
                game[y][x] = a        # non ofset rows
                counter += 1        
        
        if (counter == 2):  #change color after 2 x values
            a = Getcolor()
            counter = 0

        


pixX = 320
pixY = 200
screen = np.zeros((pixY,pixX))
   
cutoff = 1
multiplier = 5
for x in range(gridX):
    for y in range(gridY - 1):
        for i in range(multiplier):
            for j in range(multiplier):
                # if(i < cutoff and j < cutoff ):
                #     pass
                # elif (i >= multiplier - cutoff  and j >= multiplier - cutoff ):
                #     pass
                # elif (i < cutoff  and j >= multiplier - cutoff ):
                #     pass
                # elif (i  >= multiplier - cutoff  and j < cutoff ):
                #     pass
                
                # else:
                screen[y*multiplier + j][x*multiplier+i] = game[y][x]

        


# screen = drawRect(screen,0,0,10, 50)


x = 3.2 * 1.327
y = 2.0 * 1.327
resolution = (x, y)
plt.figure(figsize = resolution)
# plt.plot(x,y)

plt.matshow(screen, fignum = 0)



plt.axis('off')
plt.savefig('DRAW.png',bbox_inches='tight', dpi = 100, pad_inches=0 )       
            