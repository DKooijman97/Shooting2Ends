!Author:          Dennis Kooijman 
!Date:            31-3-2019
!Part of program: Shooting2Ends

!PURPOSE:
!Module to read in needed parameters from a txt file.

!EXTERNAL MODULES:
!Kind of integers and reals is done with the numberKinds module.

module userInput 
   use NumberKinds
   
   implicit none 
   save 
   private 
	
   public   readFromFile 

contains 
	
   subroutine readFromFile(startInterval, endInterval, N_threePoint, N_shooting, potentialType, nEnergyLevels) 
      integer(KINT), intent(out)   :: startInterval, endInterval
	  integer(KINT), intent(out)   :: N_threePoint, N_shooting, potentialType, nEnergyLevels
	  character(30)                :: fileName

      ! Getting file name with input from user 
      print*, "Give datafile name"
      read(*,*) fileName
      fileName = trim(fileName)
      
	  ! Opening file and reading input parameters
	  open(UNIT=10,FILE = fileName, STATUS = "old") 	
      	  
      read(10,*) N_threePoint 					
      read(10,*) startInterval, endInterval   
      read(10,*) potentialType
	  read(10,*) nEnergyLevels
	  read(10,*) N_shooting
	
	  close(10)
	    
   end subroutine	

end module	
