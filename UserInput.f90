module userInput 
	use GridSetup
	use NumberKinds
	
	implicit none 
	save 
	private 
	
	public  	getUserInputGrid
	
	interface getUserInputGrid 
			module procedure getUserInputGridPrivate
	end interface
	
	contains 
	
	! 	Asking user for the wanted number of meshpoints and the corresponding interval 
	subroutine getUserInputGridPrivate(self) 
		type(gridType), intent(out) 	:: self
		self%numberOfPoints = -1.0
			do while (self%numberOfPoints<0) 
				print *, 	"Give number of meshpoints"
				read(*,*) 	self%numberOfPoints
			end do 
				
		print*, 		"Give start of interval"
		read(*,*)	self%startInterval

		print*, 		"Give end of interval"
		read(*,*)	self%endInterval
		
	end subroutine

end module	