# -*- coding: utf-8 -*-
"""
Created on Wed Feb 22 00:54:26 2023

@author: Purvarth
"""

import cmath

#variables
h_cap = 1
m = 1


del_x = 0.01
x_min = 0
x_max = 10

del_E = 0.1
E_min = 1
E_max = 26
e = 9

#t = (h_cap*h_cap) / (2*m*del_x*del_x)

#grid points
div = (x_max - x_min )/ del_x
E_div = (E_max - E_min)/ del_E

#H = [[0 for i in range( int(div))] for j in range( int(div))]
v = []
psi = [0 for i in range( int(div) + 1)]
psi_value = [0 for i in range( int(div) + 1)]


for i in range( int(div) + 1):
    x_val = x_min + i*del_x
    
    if ( x_val >= 4 and x_val <= 5):
        v.append(9)
        
    else:
        v.append(0)
'''        
for i in range( int(div)):
    for j in range( int(div)):
        if ( i == j):
            H[i][j] = 2*t + v[i]
            
            if ( i == 0):
                H[i][j+1] = -t
            
            elif ( i == div):
                H[i][j-1] = -t
                
            else:
                H[i][j-1] = -t
                H[i][j-1] = -t
'''


#psi calculation
for i in range( int(div) + 1):
    E_val = E_min + i*del_x
    k = cmath.sqrt(2*m*e/ (h_cap*h_cap))
    x_val = x_min + i*del_x
    
    if ( i == 0):
        psi[i] = complex(1,0)
        
    
    elif ( i == 1):
        psi[i] = cmath.exp( complex(0,-1)*k*x_val) 
    
    else:
        psi[i] = (( 2 - (2*m*(e - v[i-1])*(del_x*del_x)/(h_cap*h_cap)))*psi[i-1]) - psi[i-2]



#Absolute value of psi
for i in range( int(div) + 1):
    psi_value[i] = abs(psi[i])    
    
#file1 for Probability density vs x             
f1 = open("psi_value.txt", "w")
for i in range( int(div) + 1): 
    x_val = x_min + i*del_x
    f1.write(str(x_val) + "\t" + str(abs(psi[i])**2) + "\n" )
f1.close()    


#file2 for T
f2 = open("T.txt", "w")

#T value
for i in range( int(E_div) + 1):
    E_val = E_min + i*del_E
    
    #wave
    for j in range( int(div) + 1):
        k = cmath.sqrt(2*m*(E_val - v[1])/ (h_cap*h_cap))
        x_val = x_min + j*del_x
    
        if ( j == 0):
            psi[j] = complex(1,0)
        
    
        elif ( j == 1):
            psi[j] = cmath.exp( complex(0,-1)*k*x_val) 
    
        else:
            psi[j] = (( 2 - (2*m*(E_val - v[j-1])*(del_x*del_x)/(h_cap*h_cap)))*psi[j-1]) - psi[j-2]
    
    #pd
    pd = []
    for l in range(601, int(div) + 1):
        pd.append(abs(psi[l])*abs(psi[l]))
    
    c_min = min(pd)
    c_max = max(pd)
    p_avg = (c_max + c_min)/2
    T = 2 / (1 + p_avg)
    f2.write(str(E_val) + "\t" + str(T) + "\n")

f2.close()
