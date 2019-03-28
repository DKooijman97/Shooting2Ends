module threePointScheme
   use GridSetup
   use Diagonalization
   use NumberKinds
   use integration_module
   implicit none 
   save 
   private 
	
   public  threePointSchemeNew, threePointSchemeDelete, Diagonalization, printEigenValuesVectors
   public  threePointSchemeType
	
   type threePointSchemeType
      real(KREAL), allocatable 	::  	V(:,:), L(:,:) 
      real(KREAL), allocatable 	::  	eigenVectors(:,:)
      real(KREAL), allocatable	::  	eigenValues(:)
   end type 

contains
   subroutine threePointSchemeNew(self)
      type(threePointSchemeType), intent(inout) :: self
   end subroutine 
   
   subroutine threePointSchemeDelete(self)
      type(threePointSchemeType), intent(inout) :: self
   end subroutine 
   
! Subroutine which uses Diagonalize to calculate the eigenvalues and eigenvectors 
! which are used as first trial value in shooting algorithm
   subroutine Diagonalization(self, Grid) 
      type(threePointSchemeType), intent(inout)  ::  self 
	  type(gridType), intent(in) :: Grid
	  real(KREAL)  ::  v_int 
	  integer(KINT):: i
	  real(KREAL) :: intergral
	  
	  allocate( self%eigenVectors(Grid%N,Grid%N) ) 
	  allocate( self%eigenValues(Grid%N) ) 
	  
	  !Fill in S 
	  allocate( self%L(Grid%N,Grid%N) ) 
	  self%L = 0.0  
	  self%L(1,1) = -2.0 
	  self%L(1,2) = 1.0
	  do i = 2, Grid%N
	     self%L(i,i)   = -2.0
		 self%L(i,i-1) = 1.0
		 self%L(i,i+1) = 1.0
	  enddo 
	  
	  !Allocate and calculating L 
	  self%L = (-1.0/(Grid%h**2))*self%L+Grid%V
	  
	  call diagonalize(self%L, self%eigenVectors, eigenvalues = self%eigenValues)
   end subroutine     
	
   !Prints 1st 10 eigenValues and Vectors
   subroutine printEigenValuesVectors(self, Grid) 	
      type(threePointSchemeType), intent(inout)  ::  self 
	  type(gridType), intent(in) :: Grid
	  integer  :: i
	  
	  print*, "This are the eigenValues"
      do i = 1, 10
	     print*,  self%eigenValues(i) 
	  enddo 
		
	  print*, "___________________________________________"
		
	  print*, "This are the eigenvectors"
	  do i = 1, Grid%N
	   print'(i5,x,1000f15.8)', i, self%eigenVectors(i,1:3) 
	  enddo
   end subroutine
	
	
end module		
