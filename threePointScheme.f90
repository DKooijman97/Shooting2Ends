module threePointScheme
   use GridSetup
   use Diagonalization
   use NumberKinds
   use integration_module
   implicit none 
   save 
   private 
	
   public  threePointSchemeNew, threePointSchemeDelete, initialize_L, Diagonalization, printEigenValuesVectors
   public  threePointSchemeType, normalize
	
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
   
   !Subroutine initializes L and V, and calculates L according to L = -(1/h²)*S + V
   subroutine initialize_L(self, Grid)
      type(threePointSchemeType), intent(inout)  ::  self 
	  type(gridType), intent(in) :: Grid
	  integer(KINT)  ::  i 
	  
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
	  
	  !Fill in V
	  allocate( self%V(Grid%N,Grid%N) ) 
	  self%V = 0.0 
	  do i = Grid%N/20, Grid%N-Grid%N/20
	     self%V(i,i) = 10
	     self%V(Grid%N-i+1,Grid%N-i+1)= 10
	  enddo
	  
	  !Allocate and calculating L 
	  self%L = (-1.0/(Grid%h**2))*self%L+self%V
	  
   end subroutine 

! Subroutine which uses Diagonalize to calculate the eigenvalues and eigenvectors 
! which are used as first trial value in shooting algorithm
   subroutine Diagonalization(self, Grid) 
      type(threePointSchemeType), intent(inout)  ::  self 
	  type(gridType), intent(in) :: Grid
	  real(KREAL)  ::  v_int 
	  allocate( self%eigenVectors(Grid%N,Grid%N) ) 
	  allocate( self%eigenValues(Grid%N) ) 
	  
	  call diagonalize(self%L, self%eigenVectors, eigenvalues = self%eigenValues)
   end subroutine      
	
   subroutine normalize(self, Grid)
      type(threePointSchemeType), intent(inout)  ::  self 
	  type(gridType), intent(in) :: Grid
	  real(KREAL) :: v_int
	  
	  call Newton_cotes(self%eigenVectors(:,1),Grid%h,1,size(self%eigenVectors,1),v_int)	
	  self%eigenValues = self%eigenValues/v_int
	  print*, "v_int", v_int 
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
		
	  !print*, "This are the eigenvectors"
	  !do i = 1, Grid%N
	  !print'(1000f15.8)', self%eigenVectors(i,1) 
	  !enddo
   end subroutine
	
	
end module		
