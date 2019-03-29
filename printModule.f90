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
   subroutine printEigenStates(vector, grid, V, trialEigenValue, eigenValue,nEnergyLevels)
      real(KREAL), intent(in)    :: vector(:,:), grid(:),V(:,:)
	  real(KREAL), intent(in)    :: trialEigenValue(:)
	  real(KREAL), intent(in)    :: eigenValue(:)
	  integer(KINT)              :: nEnergyLevels 
	  integer(KINT)              :: i 
	  character(50)              :: fileName
	  
	  print*, "Give filename to save data in txt file, in format: filename.txt" 
	  read*, fileName 
	  fileName = trim(fileName) 
	  open(20, FILE = fileName, STATUS = "NEW")
	  
	  !write(20,'(a30,x,f10.6)')  "First trial eigenvalue:", trialEigenValue
	  !write(20,'(a30,x,f10.6)') "The final eigenvalue:", eigenValue
	  !write(20,*) "The normalized eigenvectors:"
	  
	  write(20,*) "Trial_eigenValues Final_EigenValues"
	  do i = 1, nEnergyLevels
	     write(20,*) i, trialEigenValue(i), eigenValue(i)
	  enddo
	  
	  write(20,*) "eigen_vectors"
	  do i = 1, size(grid,1)
	     write(20,*) grid(i),V(i,i), vector(i,:)
	  enddo
	 
   end subroutine 
end module 	  
   