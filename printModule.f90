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
   subroutine printEigenStates(vectors, grid, trialEigenValue, eigenValue)
      type(gridType), intent(in)    :: grid
	  real(KREAL), intent(in)       :: vectors(:,:)
	  real(KREAL), intent(in)       :: trialEigenValue(:)
	  real(KREAL), intent(in)       :: eigenValue(:)
	  integer(KINT),allocatable     :: n(:)
	  integer(KINT)                 :: i 
	  character(50)                 :: fileName
	  
	  print*, "Give filename to save data in txt file, in format: filename.txt" 
	  read*, fileName 
	  fileName = trim(fileName) 
	  open(20, FILE = fileName, STATUS = "new")
	  
	  write(20,*) "energy_level Trial_eigenValues Final_EigenValues"
	  do i = 1, size(eigenValue,1)
	     write(20,*) i-1, trialEigenValue(i), eigenValue(i)
	  enddo
	  
	  write(20,*) "         "
	  
	  allocate ( n(size(vectors, 2) ) )
	  n = (/ (i, i=0, (size(vectors, 2)-1) ) /)
	  
	  write(20,*) "x ", "potential", n
	  do i = 1, size(grid%meshPoints,1)
	     write(20,*) grid%meshPoints(i),grid%V(i,i), vectors(i,:)
	  enddo
	 
   end subroutine 
end module 	  
   