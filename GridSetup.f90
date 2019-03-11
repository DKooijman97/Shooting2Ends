module GridSetup 
	use NumberKinds

	implicit none 
	save 
	private 
	
	public 		gridType
	public		GridSetupNew, createGrid
	
	type gridType 
		Integer(KINT)				:: 	numberOfPoints
		Real(KREAL)					::	startInterval, endInterval
		Real (KREAL)					:: 	distanceBetweenPoints 
		Real(KREAL), allocatable 	:: 	Meshpoints(:)   
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
			
			allocate( self%Meshpoints(self%numberOfPoints) ) 
			self%Meshpoints(1) 		     		= self%startInterval
			self%distanceBetweenPoints		= (self%endInterval-self%startInterval+1)/self%numberOfPoints
			
			do i = 2, self%numberOfPoints
				self%Meshpoints(i) = self%startInterval + (i-1)*self%distanceBetweenPoints
			enddo 
	end subroutine

end module
			
			
	
	
	
