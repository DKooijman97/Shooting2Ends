program Shooting2EndsTestGridSetup
   use GridSetup 
   use UserInput
   use NumberKinds
   use threePointScheme
   use shooting
   use printModule 
	
   implicit none 
		
   type(gridType)              ::  Grid, gridShooting
   type(threePointSchemeType)  ::  threePointScheme
   type(shootingType)          ::  Shooting
	
   call readFromFile(Grid,Shooting)
   
   call GridSetupNew(Grid)
   call createGrid(Grid)
				
   call threePointSchemeNew(threePointScheme)
   call Diagonalization(threePointScheme, Grid)		
  
   !Create New grid, with 500 points 
   gridShooting%N             = 500
   gridShooting%startInterval = Grid%startInterval
   gridShooting%endInterval   = Grid%endInterval
   gridShooting%Potential     = Grid%potential
   call GridSetupNew(GridShooting)
   call createGrid(GridShooting)
	  
  
   call shootingNew(Shooting)
   call energyStates(Shooting, GridShooting, threePointScheme)
  
   call printEigenStates(Shooting%y, GridShooting%MeshPoints,GridShooting%V, Shooting%firstLambda &
                        ,shooting%LambdaVector,shooting%energyLevels)

end program
	
