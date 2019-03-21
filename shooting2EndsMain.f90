program Shooting2EndsTestGridSetup
   use GridSetup 
   use UserInput
   use NumberKinds
   use threePointScheme
   use shooting
   use printModule 
	
   implicit none 
		
   type(gridType)  ::  Grid 
   type(threePointSchemeType)  ::  threePointScheme
   type(shootingType) ::  Shooting
		
   call readFromFile(Grid)
   call GridSetupNew(Grid)
   call createGrid(Grid)
				
   call threePointSchemeNew(threePointScheme)
   call initialize_L(threePointScheme, Grid)
   call Diagonalization(threePointScheme, Grid)		
				
   call shootingNew(Shooting)
   call calcEigenState(Shooting, Grid, threePointScheme)
   call printEigenStates(Shooting%y, Grid%MeshPoints, threePointScheme%EigenValues(1), shooting%lambda)
end program
	
