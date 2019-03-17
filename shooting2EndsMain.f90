program Shooting2EndsTestGridSetup
		use GridSetup 
		use UserInput
		use NumberKinds
		
		implicit none 
		
		type(gridType) 		:: testGrid 
		integer(KINT) 		:: i 
		
		call readFromFile(testGrid)
		call GridSetupNew(testGrid)
		call createGrid(testGrid)
		
end program
	
