module userInput 
! Contains a module to ask user directly for needed variables of the grid and a module to read information from a given text file
   use GridSetup
   use NumberKinds
	
   implicit none 
   save 
   private 
	
   public   getUserInputGrid 
   public   readFromFile 

!   interface getUserInputGrid 
!	module procedure getUserInputGridPrivate 
!   end interface 

!   interface readFromFile 
!        module procedure readFromFIlePrivate
!   end interface 	

contains 
	
! 	Asking user for the wanted number of meshpoints and the corresponding interval 
   subroutine getUserInputGrid(self) 
      type(gridType), intent(out) 	:: self
      
      self%numberOfPoints = -1.0
      do while (self%numberOfPoints<0) 
         print *, 	"Give number of meshpoints"
	 read(*,*) 	self%numberOfPoints
      end do 
				
      print*, "Give start of interval"
      read(*,*)	self%startInterval

      print*,  "Give end of interval"
      read(*,*)  self%endInterval
	
   end subroutine

   subroutine readFromFile(self) 
      type(gridType), intent(inout) :: self 
      character(30)   :: fileName

      print*, "Give datafile name"
      read(*,*) fileName
      fileName = trim(fileName)
      open(UNIT=10,FILE = fileName) 	
      
      read(10,*) self%numberOfPoints
      read(10,*) self%startInterval 
      read(10,*) self%endInterval 
      
      close(10)
   end subroutine	

end module	
