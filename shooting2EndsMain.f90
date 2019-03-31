!Author:          Dennis Kooijman 
!Date:            31-3-2019
!Part of program: Shooting2Ends

!PURPOSE: 
!Program to calculate the eigenvalues and eigenvectors nummerically on a grid using a shooting algorithm. 
!The shooting algorithm is initialized with the three point scheme method using a coarse grid. Input is readed from 
!a text file. 

!OUTPUT: 
!Output is saved in an space delimited text file and can be used to visualize the eigenstates. 
!Three different potentials can be chosen: Infinite walls, Finite walls and a Gaussian potential. 


program Shooting2EndsTestGridSetup
   use GridSetup 
   use UserInput
   use NumberKinds
   use threePointScheme
   use shooting
   use printModule 
	
   implicit none 
   
   !Define needed types:
   type(gridType)              ::  GridThreePoint, gridShooting
   type(threePointSchemeType)  ::  threePointScheme
   type(shootingType)          ::  Shooting
	
   !Define needed variables: 	
   integer(KINT)               ::  startInterval, endInterval, N_threePoint 
   integer(KINT)               ::  N_shooting, potentialType, nEnergyLevels      !The needed parameters to create a grid
   
   real(KREAL), allocatable    ::  trialEigenValues(:)	                         !The first trial eigenvalues for shooting algorithm
   real(KREAL), allocatable    ::  eigenVectors(:,:), eigenValues(:)             !Final eigenvectors and eigenvalues as calculated with the shooting algorithm 	
   
   ! Getting needed variables from a input file.txt
   call readFromFile(startInterval, endInterval, N_threePoint, N_shooting, potentialType, nEnergyLevels) 
   
   ! Creating grid for three point scheme
   call GridSetupNew(GridThreePoint,startInterval, endInterval, N_threePoint, potentialType)
   call createGrid(GridThreePoint)
	
   ! Calculate first trial eigenvalues with three point scheme
   call threePointSchemeNew(threePointScheme)
   call Diagonalization(threePointScheme, GridThreePoint)	
   call getTrialEigenValues(threePointScheme,trialEigenValues)
   call gridSetupDelete(GridThreePoint) 
  
   ! Create New grid for shooting algorithm, with 500 points
   call GridSetupNew(GridShooting,startInterval, endInterval, N_shooting, potentialType)
   call createGrid(GridShooting)
   
   ! Calculate the eigenstates of the system with the shooting algorithm 
   call shootingNew(Shooting)
   call energyStates(Shooting, GridShooting, trialEigenValues, nEnergyLevels)
   call getEigenStates(Shooting, eigenVectors, eigenValues) 
   
   ! Printing results to txt file
   call printEigenStates(eigenVectors, GridShooting, trialEigenValues, eigenValues,nEnergyLevels)

end program
	
