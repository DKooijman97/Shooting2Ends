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

   subroutine readFromFile(Grid) 
      type(gridType), intent(inout) :: Grid 
      character(30)   :: fileName

      print*, "Give datafile name"
      read(*,*) fileName
      fileName = trim(fileName)
      open(UNIT=10,FILE = fileName, STATUS = "old") 	
      
      read(10,*) Grid%N
      read(10,*) Grid%startInterval 
      read(10,*) Grid%endInterval 
      read(10,*) Grid%Potential
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

end module	
