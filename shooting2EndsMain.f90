program Shooting2EndsTestGridSetup
   use GridSetup 
   use UserInput
   use NumberKinds
   use threePointScheme
   use shooting
   use printModule 
	
   implicit none 
   
   !Define needed types:
   type(gridType)              ::  Grid, gridShooting
   type(threePointSchemeType)  ::  threePointScheme
   type(shootingType)          ::  Shooting
	
   !Define needed variables: 	
   real(KREAL), allocatable    ::  trialEigenValues(:)	               !The trial eigenvalues for shooting algorithm, calculated with the three point scheme method
   real(KREAL), allocatable    ::  eigenVectors(:,:), eigenValues(:)   !Final eigenvectors and eigenvalues as calculated with the shooting algorithm 	
   
   call readFromFile(Grid,Shooting)
   
   call GridSetupNew(Grid)
   call createGrid(Grid)
				
   call threePointSchemeNew(threePointScheme)
   call Diagonalization(threePointScheme, Grid)	
   call getTrialEigenValues(threePointScheme,trialEigenValues)
  
   !Create New grid, with 500 points 
   gridShooting%N             = 280
   gridShooting%startInterval = Grid%startInterval
   gridShooting%endInterval   = Grid%endInterval
   gridShooting%Potential     = Grid%potential
   call GridSetupNew(GridShooting)
   call createGrid(GridShooting)
   
  
   call shootingNew(Shooting)
   call energyStates(Shooting, GridShooting, trialEigenValues)
   call getEigenStates(Shooting, eigenVectors, eigenValues) 
   
   call printEigenStates(eigenVectors, GridShooting%MeshPoints,GridShooting%V, trialEigenValues &
                        ,eigenValues,shooting%energyLevels)

end program
	
