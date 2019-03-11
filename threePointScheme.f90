module threePointScheme
	use GridSetup
	use Diagonalization
	use NumberKinds
	
	implicit none 
	save 
	private 
	
	!public 
	public  threePointSchemeType
	
	type threePointSchemeType
		Real(KREAL), allocatable 	::  	S(:,:), V(:,:), L(:,:) 
		Real(KREAL), allocatable 	::  	eigenVectors(:,:)
		Real(KREAL), allocatable 	::  	eigenValues(:)
	End type 

contains
	
	
	
	
	
	
end module		