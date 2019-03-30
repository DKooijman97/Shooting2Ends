module shooting 
   use NumberKinds
   use GridSetup
   use integration_module
   
   implicit none
   save 
   private 
   
   public shootingType
   public shootingNew, shootingDelete, energyStates, getEigenStates
   
   type shootingType 
	  private
	  real(KREAL), allocatable  :: y(:,:), yOut(:), yIn(:) !Vectors in which the final eigenvector, eigenvector outwards and inwards can be stored
      real(KREAL), allocatable  :: LambdaVector(:)         !Vector containing the final eigenvalues
	  real(KREAL), allocatable  :: firstLambda(:)          !Vector containing the the first trial eigenvalues per energy level
	  real(KREAL)               :: lambda, dLambda                                     
      integer(KINT)             :: x_m                     !Matching point for the inwards and outwards eigenvectors
   end type	  

contains 
   
   subroutine shootingNew(self) 
      type(shootingType), intent(inout)  :: self
   end subroutine
   
   subroutine shootingDelete(self) 
      type(shootingType), intent(inout)  :: self
   end subroutine
   
   subroutine getEigenStates(self, eigenVectors, eigenValues) 
      type(shootingType), intent(inout)         :: self
	  real(KREAL), intent(inout), allocatable   :: eigenVectors(:,:), eigenValues(:)
      integer(KINT)                             :: matrix_size(2), length, i
	   
	  length = size(self%LambdaVector)
	  allocate( eigenValues(length) ) 
	  eigenValues = self%LambdaVector
	  
	  do i = 1, 2
	     matrix_size(i) = size(self%y, i) 
	  enddo 
	  
	  allocate( eigenVectors(matrix_size(1),matrix_size(2)) )
	  eigenVectors = self%y 
	 
   end subroutine
   
   
   subroutine energyStates(self, Grid, trialEigenValues, nEnergyLevels) 
      type(shootingType), intent(inout)      :: self
	  type(GridType), intent(inout)          :: Grid
      real(KREAL), intent(in)                :: trialEigenValues(:)
	  integer(KINT), intent(in)              :: nEnergyLevels   	        !Specification of the wanted number of energy levels, always starting with 1	 
	  integer(KINT)                          :: i
	  
	  !Calculate x_m 
	  self%x_m = Grid%N/2
      
	  !Allocate needed vectors
	  allocate( self%y(Grid%N,nEnergyLevels) )
	  allocate( self%yOut(self%x_m+1) )
      allocate( self%yIn(Grid%N) )
      allocate( self%LambdaVector(nEnergyLevels) )
	  allocate( self%firstLambda(nEnergyLevels) ) 
	  
	  self%x_m = Grid%N/2
      print*, "xm", self%x_m
      
	  !Initialize boundary
	  self%yOut(:2) = 1d-10
      self%yIn(Grid%N-1:) = 1d-10
	
	  !Loop over the different energy levels:
	  do i = 1, nEnergyLevels 
	     call calcEigenState(self, Grid, trialEigenValues(i), self%y(:,i) )  
	     print*, "Progress", int(real(i)/nEnergyLevels*100.0), "%" 
	     self%LambdaVector(i) = self%Lambda
	  end do 
   end subroutine
   
   
   subroutine calcEigenState(self, Grid, firstLambda, y)
      type(shootingType), intent(inout)      :: self
      type(GridType), intent(in)             :: Grid
      real(KREAL)                            :: Intergral 
	  real(KREAL), intent(in)                :: firstLambda
      real(KREAL)                            :: maxDifference, difference
	  real(KREAL), intent(inout)             :: y(:) 
	  integer(KINT)                          :: i=0
	  
      ! Initialize lambda with the first trial eigenvalue
	  self%Lambda = firstLambda
	  
	  !Set min difference and initialize difference 
      maxDifference = 1d-10		! Loop continues until calculated alteration (dLambda)<maxDifference
      difference = 10000
	  
	  !Loop which tests different lambdas until difference is small enough 
      do while (difference>maxDifference)
         i = i + 1
         call InOut(self%yOut, 3, self%x_m, Grid%h, self%lambda,Grid%V, 0)
         call InOut(self%yIn, Grid%N-2, self%x_m, Grid%h, self%lambda,Grid%V, 1)  
		 
         call calcDLambda(self, Grid) 
         self%Lambda = self%Lambda + self%dLambda
		 
         difference = abs(self%dLambda)
      enddo 
	   
      !Combine the inwards and outwards: 
      y(:2) = 1d-3
      y(Grid%N-1:) = 1d-3
	  call InOut(y, 3, Grid%N-3, Grid%h, self%lambda,Grid%V, 0)
      y = (y-y(1))**2
   end subroutine 
   
   subroutine InOut(y, startPoint, x_m, h, lambda,V, IN_OUT)
      real(KREAL), intent(out)  :: y(:)                      !Vector in which eigenVector should be stored
      real(KREAL), intent(in)   :: h                         !Mesh-spacing 
      real(KREAL), intent(in)   :: lambda                    !Eigenvalue
      real(KREAL), intent(in)   :: V(:,:)                    !Rank 2 matrix, with potential on the diagonal 
      integer(KINT), intent(in) :: startPoint, x_m           !Start- end matching point of calculation
      integer(KINT), intent(in) :: IN_OUT                    !Outwards: IN = 0, Inwards: IN = 1; default = Outwards

      real(KREAL)   :: intergral = 0 
      integer(KINT) :: i 
	   
      select case (IN_OUT) 
      case (0)
         do i = startPoint-1, x_m+1	     
            y(i+1) = (-y(i-1)) + (h**2)*( lambda-V(i,i)+ 2/(h**2) ) * y(i)
         enddo 
	      
         !Normalize
		 call Newton_cotes(y**2,h,1,size(y,1),intergral)
	     y = y/sqrt(intergral)
      case (1) 
         do i = startPoint+1, x_m-1, -1      
            y(i-1) = (-y(i+1)) + (h**2)*(lambda-V(i,i)+2/(h**2))*y(i) 
         enddo 
	      
         !Normalize
	     call Newton_cotes(y**2,h,1,size(y,1),intergral)
	     y = y/intergral	  
      end select 
   end subroutine
   
   subroutine calcDLambda(self, Grid) 
      type(shootingType), intent(inout)  :: self
      type(GridType), intent(in)         :: Grid
      real(KREAL)                        :: derivativeOut, derivativeIn 
      real(KREAL)                        :: intergralOut2, intergralIn2
	  
	  derivativeOut = ( self%yOut(self%x_m+1)-self%yOut(self%x_m-1) ) / 2*Grid%h
      derivativeIn  = ( self%yIn(self%x_m+1)-self%yIn(self%x_m-1) ) / 2*Grid%h
	  
      call Newton_cotes(self%yOut**2,Grid%h,1,self%x_m,intergralOut2)
      call Newton_cotes(self%yIn**2,Grid%h,self%x_m,Grid%N,intergralIn2)
	   
      self%dLambda = 0.5*( derivativeIn/self%yIn(self%x_m) - derivativeOut/self%yOut(self%x_m) )* &
	                 ( (1/(self%yOut(self%x_m)**2)) * intergralOut2 + (1/(self%yIn(self%x_m)**2)) * intergralIn2)**(-1)
   end subroutine 

   
end module
	  
	  
