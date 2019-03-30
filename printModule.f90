module printModule 
   use NumberKinds
   use gridSetup 
  
   implicit none 
   save 
   private 
   
   public printEigenStates
   
contains 
   subroutine printEigenStates(vector, grid, trialEigenValue, eigenValue,nEnergyLevels)
      type(gridType), intent(in)    :: grid
	  real(KREAL), intent(in)       :: vector(:,:)
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
	     write(20,*) grid%meshPoints(i),grid%V(i,i), vector(i,:)
	  enddo
	 
   end subroutine 
end module 	  
   