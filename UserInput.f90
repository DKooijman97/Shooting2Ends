module userInput 
! Contains a module to ask user directly for needed variables of the grid and a module to read information from a given text file
   use GridSetup
   use NumberKinds
   use shooting 
   
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
	
   subroutine readFromFile(Grid,shooting) 
      type(gridType), intent(inout)     :: Grid 
      type(shootingType), intent(inout) :: shooting 
	  character(30)   :: fileName

      ! Getting file name with input from user 
      print*, "Give datafile name"
      read(*,*) fileName
      fileName = trim(fileName)
      
	  ! Opening file and reading input parameters
	  open(UNIT=10,FILE = fileName, STATUS = "old") 	
      	  
      read(10,*) Grid%N						
      read(10,*) Grid%startInterval 
      read(10,*) Grid%endInterval 
      read(10,*) Grid%Potential
	  
	  read(10,*) shooting%energyLevels
	  close(10)
	  
	  if (Grid%Potential == 1) then 
			   print*, "chosen potential = infinite walls"
			
			elseif (Grid%Potential == 2) then
			   print*, "chosen potential = finite walls"
			
			elseif (Grid%Potential == 3) then
			   print*, "chosen potential = Gausian"
			   
			else 
			   print*, "Potential not chosen correctly, please state your choose"
			   print*, "1: Infinite wall, 2:Finite walls, 3: Gaussian"
			   read(*,*) Grid%Potential 
      end if
	    
   end subroutine	

! 	Asking user for the wanted number of meshpoints and the corresponding interval 
   subroutine getUserInputGrid(Grid) 
      type(gridType), intent(out) 	:: Grid
      
      Grid%N = -1.0
      do while (Grid%N<0) 
         print *, 	"Give number of meshpoints"
	     read(*,*) 	Grid%N
      end do 
				
      print*, "Give start of interval"
      read(*,*)	Grid%startInterval

      print*,  "Give end of interval"
      read(*,*)  Grid%endInterval
	
   end subroutine


end module	
