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

Structure: 

An schematic overview of the design, structure and flow of the program is given in Flow_and_design_program.pdf. The program consist out of 5 main modules. 

1. The first module is userInput and has a subroutine readFromFile which reads the needed parameters from a txt file. The name of the txt is given by the user in the terminal. 

2. The given parameters are then used to create a grid for the three points scheme. This is done by the gridSetup module. In this module the meshpoints, meshdistance and potential on the grid is calculated. All parameters are stored in an gridType. The variables in gridType are public and also used in the other modules. 

3. With the created grid the first trial eigenvalues are calculated with the threePointScheme module. The output of the module is an vector with the eigenvalues per energylevel. The threePointScheme main subroutine is Diagonalization. This subroutine calculates the eigenvalues by performing diagonalization on a symmetrix tridiagonal matrix L. Diagonalization in performed with DSTERF from the LAPACK library. LAPACK needs the BLAS library in order to work. L as well as the eigenValues are stored in a threePointSchemeType. The parameters in this type are private. Therefor, an getter is included to retrieve the eigenvalues from the module.

4. The eigenvalues as determined with the three point scheme method are then used as starting point for the shooting algorithm. First a new grid is setup with more gridpoints with the gridSetup module. Then the shooting module is used. The shooting module has multiple subroutines. All variables needed in multiple subroutines are stored in a shootingType. The parameters in this shootingType are private. 
The first main subroutine is energyStates, which loops over the wanted energyStates. It stores the eigenvalues per energy level in an vector and the eigenvectors in a matrix. energyStates calls the subroutine calcEigenState. This subroutine includes the main loop of the shooting algorithm, mainly testing multiple eigenvalues until difference between two consectutive eigenvalues is small enough. To calculate the needed inwards and outwards eigenvectors the subroutine InOut is called. To calculated the correction of the eigenVector the subroutine calcdLambda is used. Because the variables in the shootingType are private an getter is included to retrieve the eigenstates from the module. 

5. Finally, the results are saved in an space delimited text file by the printModule. 


Other uses of modules: 

The three point scheme module could also be used without the shooting algorithm. 
The gridSetup module could be used for all problems requiring a uniform grid of N equidistant mesh points. 

Command line to compile program:  
gfortran NumberKinds.f90 GridSetup.f90 blas.f lapack.f trapez.f90 threePointScheme.f90 shooting.f90 UserInput.f90 printModule.f90 shooting2EndsMain.f90 -o shooting2EndsMain
