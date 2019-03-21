module GridSetup 
   use NumberKinds

   implicit none 
   save 
   private 
	
   public   gridType
   public   GridSetupNew, createGrid
	
   type gridType 
      integer(KINT)  ::  N
      integer(KINT)  ::  startInterval, endInterval
      real(KREAL)  ::  h 
      real(KREAL), allocatable  ::  meshpoints(:)   
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
			integer(KINT)					:: i
			
			allocate( self%Meshpoints(self%N) ) 
			self%Meshpoints(1) 		     		= self%startInterval
			
			self%h		= real(self%endInterval-self%startInterval)/(self%N-1)
						
			do i = 2, self%N
				self%Meshpoints(i) = self%startInterval + (i-1)*self%h
			enddo
			
			print*, "n", real(self%N)
			print *,"h", self%h
			print*, "start/end", self%startInterval, self%endInterval
			
	end subroutine

end module
			
			
	
	
	
