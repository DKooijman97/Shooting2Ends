program Shooting2EndsTestGridSetup
		use GridSetup 
		use UserInput
		use NumberKinds
		use threePointScheme
		
		implicit none 
		
		type(gridType)   :: Grid 
		integer(KINT)   :: i 
		type(threePointSchemeType)   :: threePointScheme
		
		call readFromFile(Grid)
		call GridSetupNew(Grid)
		call createGrid(Grid)
				
		call threePointSchemeNew(threePointScheme)
		call initialize_L(threePointScheme, Grid)
		call Diagonalization(threePointScheme, Grid)
		
		call printEigenValuesVectors(threePointScheme, Grid)
		
end program
	
