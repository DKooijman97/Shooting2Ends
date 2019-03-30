module userInput 
! Contains a module to ask user directly for needed variables of the grid and a module to read information from a given text file
   use GridSetup
   use NumberKinds
   use shooting 
   
   implicit none 
   save 
   private 
	
   public   readFromFile 

!   interface getUserInputGrid 
!	module procedure getUserInputGridPrivate 
!   end interface 

!   interface readFromFile 
!        module procedure readFromFIlePrivate
!   end interface 	

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
