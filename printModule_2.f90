!Author:          Dennis Kooijman 
!Date:            31-3-2019
!Part of program: Shooting2Ends

!PURPOSE 
!Module to print the eigenstates per energy level of a particle in a box  

!INPUT: 
!Input is a matrix with the eigenvectors column wise, an Gridtype (created with the setupGrid module)
!the first trial eigenvalues for the shooting algorithm 
!the final eigenvalues of the shooting algorithm. 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!The number of energylevels

!OUTPUT: 
!Output is a vector with the eigenvalues per energylevel. 
!The output is a space delimited txt file. 

!EXTERNAL MODULES:
!The grid on which the calculations have been performed is created with the GridSetup module. 
!The corresponding type of this module (gridType) is also used. 
!Kind of integers and reals is done with the numberKinds module

module printModule 
   use NumberKinds
   use GridSetup
   
   implicit none 
   save 
   private 
	
   public   nameOfFile, printEigenStates, printType, newPrint
   
   type printType
       private
	   character(50)   :: fileName
   end type 
   
contains 
   
   ! Gets filename to save data 
   subroutine newPrint(self)
       type(printType), intent(inout) :: self
   end subroutine
   
   subroutine nameOfFile(self) 
	  type(printType), intent(inout) :: self
	  print *, "Give filename to save data in txt file, in format: filename.txt"
	  read(*,*) self%fileName
	  self%fileName = trim(self%fileName)
   end subroutine  
   
   ! Prints data to txt file
   subroutine printEigenStates(self, Grid, eigenValues, eigenVectors, trialEigenValues, nEnergyLevels)
      type(printType), intent(inout) ::  self
	  type(gridType), intent(in)     ::  Grid 
	  real(KREAL), intent(in)        ::  eigenValues(:), eigenVectors(:,:), trialEigenValues(:) 
      integer(KINT), intent(in)      ::  nEnergyLevels
	  real(KREAL), allocatable       ::  n(:) 
	  integer(KINT)                  ::  i 
	  
	  open(FILE = self%fileName, UNIT = 20, STATUS = "NEW")  
   																		
	  write(20,*) "energy_level Trial_eigenValues Final_EigenValues"
	  do i = 1, nEnergyLevels
	     write(20,*) i-1, trialEigenValues(i), eigenValues(i)
	  enddo
	  																			
	  write(20,*) "         "
	  
	  allocate(  n(nEnergyLevels) )  
	  n = (/ (i, i=0, (size(eigenVectors, 2)-1) ) /)
	  
	  write(20,*) "x ", "potential", n
	  do i = 1, size(grid%meshPoints,1)
	     write(20,*) grid%meshPoints(i),grid%V(i,i), eigenVectors(i,:)
	  enddo
   
   end subroutine 
	  
   
end module    
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   