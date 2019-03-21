module printModule 
   use shooting 
   use threePointScheme
   use NumberKinds
   use GridSetup
   
   implicit none 
   save 
   private 
   
   public printEigenStates
   
contains 
   subroutine printEigenStates(vector, grid, trialEigenValue, eigenValue)
      real(KREAL), intent(in)    :: vector(:), grid(:)
	  real(KREAL), intent(in)    :: trialEigenValue
	  real(KREAL), intent(in)    :: eigenValue
	  integer(KINT)              :: i 
	  character(50)              :: fileName
	  
	  print*, "Give filename to save data in txt file, in format: filename.txt" 
	  read*, fileName 
	  fileName = trim(fileName) 
	  open(20, FILE = fileName, STATUS = "NEW")
	  
	  write(20,'(a30,x,f10.6)')  "First trial eigenvalue:", trialEigenValue
	  write(20,'(a30,x,f10.6)') "The final eigenvalue:", eigenValue
	  write(20,*) "The normalized eigenvector:"
	  
	  do i = 1, size(vector,1)
	     write(20,'(f10.6,x,f10.6)') grid(i), vector(i)
	  end do
   end subroutine 
end module 	  
   