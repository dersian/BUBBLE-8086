# -*- coding: utf-8 -*-
"""
Created on Thu Apr  7 14:47:31 2022
___________________________________________________________________________/"""


x = 32
y = 20 

NUMBER = 101


spaces = "   "
print( "  ", end='')
for i in range(x):
    if(i / 100 >= 1):
        spaces = ""
    elif(i / 10 >= 1):
        spaces = " "
    else:
        spaces = "  "
    print(spaces,i , end='')
for i in range(x*y):
    if(i%x == 0):
        print(f"\n")
        print(int(i/32), end='')
        if(int(i/32)<10):
            print(" ", end='')
    if(i / 100 >= 1):
        spaces = ""
    elif(i / 10 >= 1):
        spaces = " "
    else:
        spaces = "  "
    
    if(i == NUMBER):
        print('\033[33m'+spaces, i, end='' + '\033[0m')
        
    else:print(spaces, i, end='')
    
print("")
print("")
 
print(NUMBER, " : ")  
y = int(NUMBER/32)
print("x = ", y)  

print("y = ", NUMBER - y*32)    

    