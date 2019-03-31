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
   use gridSetup 
  
   implicit none 
   save 
   private 
   
   public printEigenStates
   
contains 
   subroutine printEigenStates(vectors, grid, trialEigenValue, eigenValue,nEnergyLevels)
      type(gridType), intent(in)    :: grid
	  real(KREAL), intent(in)       :: vectors(:,:)
	  real(KREAL), intent(in)       :: trialEigenValue(:)
	  real(KREAL), intent(in)       :: eigenValue(:)
	  integer(KINT)                 :: nEnergyLevels 
	  integer(KINT),allocatable     :: n(:)
	  integer(KINT)                 :: i 
	  character(50)                 :: fileName
	  
	  print*, "Give filename to save data in txt file, in format: filename.txt" 
	  read*, fileName 
	  fileName = trim(fileName) 
	  open(20, FILE = fileName, STATUS = "new")
	  
	  write(20,*) "energy_level Trial_eigenValues Final_EigenValues"
	  do i = 1, nEnergyLevels
	     write(20,*) i, trialEigenValue(i), eigenValue(i)
	  enddo
	  
	  allocate ( n(nEnergyLevels) )
	  n = (/ (i, i=1,nEnergyLevels) /)
	  
	  write(20,*) "x ", "potential", n
	  do i = 1, size(grid%meshPoints,1)
	     write(20,*) grid%meshPoints(i),grid%V(i,i), vectors(i,:)
	  enddo
	 
   end subroutine 
end module 	  
   