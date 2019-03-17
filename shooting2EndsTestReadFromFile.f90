program Shooting2EndsTestGridSetup
   use GridSetup 
   use UserInput
   use NumberKinds
		
   implicit none 
		
   type(gridType)  :: testGrid 
   integer(KINT)   :: i 
   		
		
   call readFromFile(testGrid)
!   call getUserInputGrid(testGrid) 
   call GridSetupNew(testGrid)
   call createGrid(testGrid)
		
   !test resulting gridType variable
   print *, "Number of Points", testGrid%numberOfPoints, testGrid%numberOfPoints == 10
   print *, "Start", testGrid%startInterval, testGrid%startInterval == 1.0
   print *, "End", testGrid%endInterval, testGrid%endInterval == 10.0

end program
	
