program Shooting2EndsTestGridSetup
		use GridSetup 
		use UserInput
		use NumberKinds
		
		implicit none 
		
		type(gridType) 		:: testGrid 
		integer(KINT) 		:: i 
		
		
		call getUserInputGrid(testGrid)
		call GridSetupNew(testGrid)
		call createGrid(testGrid)
		
		!prints the resulting grid
		do i = 1, testgrid%numberOfPoints
			print *, testgrid%Meshpoints(i)
		end do 
end program
	