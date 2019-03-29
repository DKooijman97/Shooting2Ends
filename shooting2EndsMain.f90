program Shooting2EndsTestGridSetup
   use GridSetup 
   use UserInput
   use NumberKinds
   use threePointScheme
   use shooting
   use printModule 
	
   implicit none 
		
   type(gridType)              ::  Grid 
   type(threePointSchemeType)  ::  threePointScheme
   type(shootingType)          ::  Shooting
	
   call readFromFile(Grid,Shooting)
   
   call GridSetupNew(Grid)
   call createGrid(Grid)
				
   call threePointSchemeNew(threePointScheme)
   call Diagonalization(threePointScheme, Grid)		
   
   call shootingNew(Shooting)
   call energyStates(Shooting, Grid, threePointScheme)
  
   call printEigenStates(Shooting%y, Grid%MeshPoints,Grid%V, Shooting%firstLambda, shooting%LambdaVector,shooting%energyLevels)

end program
	
