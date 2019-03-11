program Shooting2EndsTestGridSetup
		use GridSetup 
		use UserInput
		use NumberKinds
		
		implicit none 
		
		type(gridType) 		:: testGrid 
		integer(KINT) 		:: i 
		real(KREAL)			:: expectedMeshPoints(10)
		
		testGrid%numberOfPoints			= 10
		testGrid%startInterval			= 1.0
		testGrid%endInterval			= 10.0
		 
		do i = 1, 10 
			expectedMeshPoints(i) = i 
		enddo 
		
		call GridSetupNew(testGrid)
		call createGrid(testGrid)
		
		!Test if expected values match
		do i = 1, testgrid%numberOfPoints
			print *, testgrid%Meshpoints(i) == expectedMeshPoints(i)
		enddo 
end program
	