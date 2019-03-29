module GridSetup 
   use NumberKinds

   implicit none 
   save 
   private 
	
   public   gridType
   public   GridSetupNew, createGrid
	
   type gridType 
      integer(KINT)             ::  N
      integer(KINT)             ::  startInterval, endInterval
      real(KREAL)               ::  h 
	  real(KREAL), allocatable  ::  V(:,:)
	  Integer(KINT)             ::  Potential              !1 = Infinite walls, 2 = finite walls, 3 = Gaussian Potential              
      real(KREAL), allocatable  ::  Meshpoints(:)   
   end type
	
   interface gridSetupNew
      module procedure gridSetupNewPrivate
   end interface
	
   interface createGrid 
      module procedure createGridPrivate 
   end interface
	
	
	
contains 

	! Initializes self
	subroutine gridSetupNewPrivate(self)
			type(gridType), intent(inout)	:: self
	end subroutine 
	
	! Creates grid and stores it in Meshpoints%self
	subroutine createGridPrivate(self)
			type(gridType), intent(inout)	:: self
			integer(KINT)                   :: i
			
			allocate( self%Meshpoints(self%N) ) 
			self%Meshpoints(1) 		     		= self%startInterval
			
			self%h		= real(self%endInterval-self%startInterval)/(self%N-1)
						
			do i = 2, self%N
				self%Meshpoints(i) = self%startInterval + (i-1)*self%h
			enddo
			print *, "start/end", self%startInterval, self%endInterval
			print *, "n", real(self%N)
	        print *, "h", self%h
			call potential(self)
            
   end subroutine
   
   !Fill in V
   subroutine potential(self) 
      type(gridType), intent(inout)	:: self
      integer(KINT)                 :: i
      real(KREAL)                   :: V0, alpha
	  
	  allocate( self%V(self%N,self%N) ) 
	  
	  select case (self%potential) 
	  case (1)
	     self%V = 0.0
		 self%V(:2,:2)               = 1000000000
		 self%V(self%N-1:,self%N-1:) = 1000000000
		
      case(2) 
	     self%V = 0.0
		 do i = 1, self%N/20
			self%V(i,i) = 1
		 enddo

		 do i = self%N, self%N-self%N/20,-1
			self%V(i,i) = 1
		 enddo
	  
      case(3) 
	     V0 = 10
		 alpha = 10
		 do i = 1, self%N
		    self%V(i,i) = (-V0)*exp(-alpha*self%meshpoints(i)**2)
         enddo
	  end select
   end subroutine
		
end module
			
			
	
	
	
