!Author:          Dennis Kooijman 
!Date:            31-3-2019
!Part of program: Shooting2Ends

!Purpose: 
!Module to calculate the eigenvalues and eigenstates nummerically on a grid using a shooting algorithm. 

!Input:
!The input is the grid on which the calculations should be performed (created with the GridSetup module), the number
!of wanted eigenstates and the first trial eigenvalues to make certain the shooting algorithm converges correctly. 
!The first trial eigenstates can be calculated with the threePointScheme module. 

!Output: 
!The output is a matrix with the eigenvector per energylevel columns wise and the eigenvalues per energylevel in a vector

!Use of external modules: 
!Normalization is performed with the integration_module (trapez.f90) which is utilizing Newton-Cotes formulas to 
!integrate a function on a grid. 
!The grid on which the calculation should take place is created with the GridSetup module. The corresponding type of this module (gridType) 
!is also used. Type of integers and reals is done with the numberKinds module

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
	  real(KREAL)               :: lambda, dLambda         !EigenValue and the correction of the eigenvalue during the shooting loop                            
      integer(KINT)             :: x_m                     !Matching point for the inwards and outwards eigenvectors
   end type	  

contains 
   !Standard ADT subroutines: 
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
   
   !Subroutine which loops over the different energylevels:
   subroutine energyStates(self, Grid, trialEigenValues, nEnergyLevels) 
      type(shootingType), intent(inout)      :: self
	  type(GridType), intent(inout)          :: Grid                        !Grid on which calculations should take place
      real(KREAL), intent(in)                :: trialEigenValues(:)         !The first trial eigenvalues per energy level
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
	  self%yOut(:2) = -1d-10
      self%yIn(Grid%N-1:) = 1d-10
	
	  !Loop over the different energy levels:
	  do i = 1, nEnergyLevels 
		 call calcEigenState(self, Grid, trialEigenValues(i), self%y(:,i) )  
	     print*, "Progress", int(real(i)/nEnergyLevels*100.0), "%" 
		 self%LambdaVector(i) = self%Lambda
	  end do 
   end subroutine
   
   !Subroutine which calculates eigenstate per energylevel, using the inOut & calcDLambda subroutines
   subroutine calcEigenState(self, Grid, firstLambda, y)
      type(shootingType), intent(inout)      :: self
      type(GridType), intent(in)             :: Grid
      real(KREAL)                            :: Intergral 
	  real(KREAL), intent(in)                :: firstLambda
      real(KREAL)                            :: maxDifference, difference   ! Loop continues until calculated correction (dLambda)<maxDifference
	  real(KREAL), intent(inout)             :: y(:) 
	  integer(KINT)                          :: i=0
	  
      ! Initialize lambda with the first trial eigenvalue
	  self%Lambda = firstLambda
	  
	  !Set min difference and initialize difference 
      maxDifference = 1d-8	
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
   
   !Subroutine which calculates the inwards and outwards eigenvector
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
	      
      case (1) 
         do i = startPoint+1, x_m-1, -1      
            y(i-1) = (-y(i+1)) + (h**2)*(lambda-V(i,i)+2/(h**2))*y(i) 
         enddo 
	  end select
      
	  !Normalize
	  call Newton_cotes(y**2,h,1,size(y,1),Intergral)
	  y = y/sqrt(intergral)	  
      
   end subroutine
   
   !Subroutine which calculates the correction of the eigenvalue
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
	  
	  
