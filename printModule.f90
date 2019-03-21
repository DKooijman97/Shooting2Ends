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
	  
	  print'(a30,x,f10.6)', "First trial eigenvalue:", trialEigenValue
	  print'(a30,x,f10.6)', "The final eigenvalue:", eigenValue
	  print*, "The normalized eigenvector:"
	  
	  do i = 1, size(vector,1)
	     print'(f10.6,xx,f10.6)', grid(i), vector(i)
	  end do
   end subroutine 
end module 	  
   