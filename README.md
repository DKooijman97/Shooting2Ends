# Shooting2Ends
Solving the 1-Dimensional Schroedinger Equation numerically on a Grid
Author: Dennis Kooijman 
Date:   31-03-2019

Purpose: 
Program to calculate the eigenvalues and eigenvectors nummerically on a grid using a shooting algorithm. 
The shooting algorithm is initialized with the three point scheme method using a coarse grid. Input is readed in from a text file.
Output is given in a space delimited txt file, which can be used to visiualize the eigenstates. 
The eigenstate can be solved for three arbitrary potentials: 
                *Potential well with infinite walls 
                *Potential well with finite walls (On both sides 5% of the box consists of walls) 
                *Gaussian potential well

Input text file: 
An example of the layout of a text file is given below and in TestUserInput.txt.
          _______________________________________________________________________
          
          250	    <--Number of meshpoints for three point scheme 
          -1 1    <--start interval/ end interval 
          1       <--Potential type: 1: Infinite walls, 2: Finite walls, 3: Gaussian potential
          10      <--Number of energy states, starting from n=0. 
          500     <--Number of meshpoints for shooting algorithm 
         ________________________________________________________________________

Output: 
An example of the txt file output of the program is given in ExampleOutput.txt. 
Also an visualization of this data is given in: visualizationEigenStates.pdf. 

Other uses of modules: 
The three point scheme module could also be used without the shooting algorithm. 
The gridSetup module could be used for all problems requiring a uniform grid of N equidistant mesh points. 
