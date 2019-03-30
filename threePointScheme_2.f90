module threePointScheme
   use GridSetup
   use Diagonalization
   use NumberKinds
   
   implicit none 
   save 
   private 
	
   public  threePointSchemeNew, threePointSchemeDelete,getTrialEigenValues, Diagonalization
   public  threePointSchemeType
	
   type threePointSchemeType
	  private
	  real(KREAL), allocatable 	::  	L(:,:) 
      real(KREAL), allocatable	::  	eigenValues(:), eigenValuesFinal(:)
   end type 

contains
   subroutine threePointSchemeNew(self)
      type(threePointSchemeType), intent(inout) :: self
   end subroutine 
   
   subroutine threePointSchemeDelete(self)
      type(threePointSchemeType), intent(inout) :: self
   end subroutine 
   
   subroutine getTrialEigenValues(self, trialEigenValues) 
       type(threePointSchemeType), intent(inout) :: self
	   real(KREAL), intent(inout), allocatable   :: trialEigenValues(:)
       integer(KINT)                             :: length
	   
	   length = size( self%eigenValuesFinal ) 
	   allocate( trialEigenValues(length) ) 
	   trialEigenValues = self%eigenValuesFinal
   end subroutine
   
! Subroutine which uses Diagonalize to calculate the eigenvalues and eigenvectors 
! which are used as first trial value in shooting algorithm
   subroutine Diagonalization(self, Grid) 
      type(threePointSchemeType), intent(inout)  :: self 
	  type(gridType), intent(in)                 :: Grid
	  integer(KINT)                              :: i, j
	  real(KREAL)                                :: intergral
	  real(KREAL), allocatable                   :: E(:)
	  integer(KINT)                              :: INFO
	 
	  allocate( self%eigenValues(Grid%N) )   
		
	  !Fill in S 
	  allocate( self%L(Grid%N,Grid%N) ) 
	  
	  self%L = 0.0  
	  self%L(1,1) = -2.0 
	  self%L(1,2) = 1.0
	  self%L(grid%N, grid%N) = -2.0
	  self%L(Grid%N, Grid%N-1) = 1.0
	  
	  do i = 2, Grid%N-1
	     self%L(i,i)   = -2.0
		 self%L(i,i-1) = 1.0
		 self%L(i,i+1) = 1.0
	  enddo 
	   
	  ! Calculating L 
	  self%L = (-1.0/(Grid%h**2))*self%L+Grid%V
	  
	  do i = 1, Grid%N 
	     self%eigenValues(i) = self%L(i,i) 
      enddo 
		 
	  allocate ( E(Grid%N-1) ) 
	  
	  do i = 1, Grid%N-1
	     E(i) = self%L(i+1,i) 
      enddo
	  
	  call DSTERF( Grid%N, self%eigenValues, E, INFO )
   
      !If neccesary remove 1/2 energy levels and make E negative 
	  if (Grid%potential == 1) then
	     allocate( self%eigenValuesFinal( size(self%eigenValues)) )
	     self%eigenValuesFinal = self%eigenValues*(-1)
		 
	  elseif (Grid%potential == 2 .or. Grid%potential == 3) then 
	     allocate( self%eigenValuesFinal( size(self%eigenValues)/2+1) )
		 j = 1
	     do i = 2, size(self%eigenValues), 2 
	        self%eigenValuesFinal(j)  = self%eigenValues(i)*(-1) 
			j = j+1
	     enddo
      endif
	
   end subroutine     
	
end module		
