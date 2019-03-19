module threePointScheme
   use GridSetup
   use Diagonalization
   use NumberKinds
   
   implicit none 
   save 
   private 
	
   public  threePointSchemeNew, initialize_L, Diagonalization, printEigenValuesVectors
   public  threePointSchemeType
	
   type threePointSchemeType
      real(KREAL), allocatable 	::  	S(:,:), V(:,:), L(:,:) 
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
   
   !Subroutine initializes S and V, and calculates L according to L = -(1/h²)*S + V
   subroutine initialize_L(self, Grid)
      type(threePointSchemeType), intent(inout)  ::  self 
	  type(gridType), intent(in) :: Grid
	  integer(KINT)  ::  i 
	  
	  !Fill in S 
	  allocate( self%S(Grid%numberOfPoints,Grid%numberOfPoints) ) 
	  self%S = 0.0  
	  self%S(1,1) = -2.0 
	  self%S(1,2) = 1.0
	  do i = 2, Grid%numberOfPoints
	     self%S(i,i)   = -2.0
		 self%S(i,i-1) = 1.0
		 self%S(i,i+1) = 1.0
	  enddo 
	  
	  !Fill in V
	  allocate( self%V(Grid%numberOfPoints,Grid%numberOfPoints) ) 
	  self%V = 0.0 
	  
	  !Allocate and calculating L 
	  allocate( self%L(Grid%numberOfPoints,Grid%numberOfPoints) ) 
	  self%L = (-1.0/Grid%numberOfPoints**2)*self%S+self%V
	  
	  deallocate(self%S) 
	  
   end subroutine 

! Subroutine which uses Diagonalize to calculate the eigenvalues and eigenvectors 
! which are used as first trial value in shooting algorithm
   subroutine Diagonalization(self, Grid) 
      type(threePointSchemeType), intent(inout)  ::  self 
	  type(gridType), intent(in) :: Grid
	  
	  allocate( self%eigenVectors(Grid%numberOfPoints,Grid%numberOfPoints) ) 
	  allocate( self%eigenValues(Grid%numberOfPoints) ) 
	  
	  call diagonalize(self%L, self%eigenVectors, eigenvalues = self%eigenValues)
   end subroutine      
   
   !Prints 1st 10 eigenValues and Vectors
   subroutine printEigenValuesVectors(self, Grid) 	
      type(threePointSchemeType), intent(inout)  ::  self 
	  type(gridType), intent(in) :: Grid
	  integer  :: i
	  
	  print*, "This are the eigenValues"
      do i = 1, 10
	     print'(1000f15.8)',  self%eigenValues(i) 
	  enddo 
		
	  print*, "___________________________________________"
		
	  print*, "This are the eigenvectors"
	  do i = 1, 10  
	  print'(1000of15.8)', self%eigenVectors(i,1) 
	  enddo
   end subroutine
	
	
end module		
