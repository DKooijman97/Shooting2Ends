module shooting 
   use GridSetup
   use Diagonalization
   use NumberKinds
   use integration_module
   use threePointScheme
   use integration_module
   
   implicit none
   save 
   private 
   
   public shootingType
   public shootingNew, shootingDelete, calcEigenState
   
   type shootingType 
      real(KREAL), allocatable  :: y(:) 
      real(KREAL), allocatable  :: yIn(:)    
      real(KREAL), allocatable  :: yOut(:)
      real(KREAL)               :: lambda  
      real(KREAL)               :: dLambda                             
      integer(KINT)             :: x_m
   end type	  

contains 
   
   subroutine shootingNew(self) 
      type(shootingType), intent(inout)  :: self
   end subroutine
   
   subroutine shootingDelete(self) 
      type(shootingType), intent(inout)  :: self
   end subroutine
   
   subroutine calcEigenState(self, Grid, threePoint)
      type(shootingType), intent(inout)      :: self
      type(GridType), intent(in)             :: Grid
      type(threePointSchemeType), intent(in) :: ThreePoint
      real(KREAL)                            :: minDifference, difference
      real(KREAL)                            :: Intergral  
      integer(KINT)                          :: i=0
	  
      self%x_m = Grid%N/2
      print*, "xm", self%x_m
      !Initialize lambda with first threePointScheme
      self%lambda = threePoint%eigenValues(1)
      
      !Set min difference and initialize difference 
      print*, "Set min difference"
      read(*,*) minDifference
      difference = 10000
	  
      allocate( self%yOut(self%x_m+1) )
      allocate( self%yIn(Grid%N) )
      self%yOut(:2) = 1d-3
      self%yIn(Grid%N-1:) = 1d-3

      !Loop which tests different lambdas until difference is small enough 
      do while (difference>minDifference)
         i = i + 1
         call InOut(self%yOut, 3, self%x_m, Grid%h, self%lambda,Grid%V, 0)
         call InOut(self%yIn, Grid%N-2, self%x_m, Grid%h, self%lambda,Grid%V, 1)  
		 
		
         call calcDLambda(self, Grid) 
	     !1print*, "dLambda",self%dLambda
         self%Lambda = self%Lambda - self%dLambda
		 
         difference = abs(self%dLambda)
      enddo 
	   
      !Combine the two 
      allocate( self%y(grid%N) ) 
      self%y(:2) = 1d-3
      self%y(Grid%N-1:) = 1d-3
	  call InOut(self%y, 3, Grid%N-3, Grid%h, self%lambda,Grid%V, 0)
      self%y = self%y-self%y(1)	   
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
         	
		 call Newton_cotes(y,h,1,size(y,1),intergral)
	     y = y/intergral
      case (1) 
         do i = startPoint+1, x_m-1, -1      
            y(i-1) = (-y(i+1)) + (h**2)*(lambda-V(i,i)+2/(h**2))*y(i) 
         enddo 
	      
         !Normalize
	     call Newton_cotes(y,h,1,size(y,1),intergral)
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
	   
      self%dLambda = -0.5*( derivativeIn/self%yIn(self%x_m) - derivativeOut/self%yOut(self%x_m) )* &
	                 ( (1/(self%yOut(self%x_m)**2)) * intergralOut2 + (1/(self%yIn(self%x_m)**2)) * intergralIn2)**(-1)
   end subroutine 

   
end module
	  
	  
