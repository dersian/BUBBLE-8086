# -*- coding: utf-8 -*-
"""
Created on Tue Apr  5 11:22:11 2022
___________________________________________________________________________/"""
"""Straight Gird"""
import matplotlib.pyplot as plt
import numpy as np
import random

def drawRect(arr, placeY , placeX, size , color):
    for x in range(size):
        for y in range(size):
            arr[y + placeY][x + placeX] = color         
    return arr


gridX = 32
gridY = 20
game = np.zeros((gridY,gridX))


for x in range(gridX):
    for y in range(gridY):
        a = random.choice(tuple((40,50,60,70,80,90)))
        if(y > 7):
            pass
        elif (y% 2):           
            if (x%2 == 0):
                game[y][x] = a
        elif (x%2 == 1):
                game[y][x] = a

   
pixX = 320
pixY = 200
screen = np.zeros((pixY,pixX))


cutoff = 3
multiplier = 10
for x in range(gridX):
    for y in range(gridY):
        for i in range(multiplier):
            for j in range(multiplier):
                if(i < cutoff and j < cutoff ):
                    pass
                elif (i >= multiplier - cutoff  and j >= multiplier - cutoff ):
                    pass
                elif (i < cutoff  and j >= multiplier - cutoff ):
                    pass
                elif (i  >= multiplier - cutoff  and j < cutoff ):
                    pass
                
                else:
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


