! ############### CONST ###################
MODULE CONST
	COMPLEX, PARAMETER :: Z0 = (0.,0.), Z1 = (1.,0.), ZI = (0.,1.)
	REAL, PARAMETER :: PI = 4*ATAN(1.)
END MODULE CONST
! ############### MODEL ###################
MODULE MODEL
	INTEGER :: L = 4 ! lattice size LxL, L should be even
END MODULE MODEL
! ############### GRID ####################
MODULE GRID
	INTEGER, ALLOCATABLE :: XS(:,:)
	REAL,    ALLOCATABLE :: KS(:,:)
CONTAINS
! make grid
SUBROUTINE MAKE_GRID()
	USE CONST
	USE MODEL
	INTEGER :: N, I, I1, I2
	
	N = L**2/2 ! number of unit cells
	ALLOCATE(XS(2*N,2), KS(N,2)) ! allocate x and k grid
	! filling in x grid (x1 runs faster)
	I = 0
	DO I2 = 0, L-1
	DO I1 = 0, L-1
		I = I + 1 ! I inc
		XS(I,1) = I1
		XS(I,2) = I2
	END DO !I1
	END DO !I2
	! filling in k grid (k1 runs faster)
	I = 0
	DO I2 = 0, L-1
	DO I1 = 0, L/2-1
		I = I + 1 ! I inc
		KS(I,1) = I1
		KS(I,2) = I2
	END DO !I1
	END DO !I2
	KS = KS*(2*PI/L)
END SUBROUTINE MAKE_GRID
! end of module GRID
END MODULE GRID
! ############# PHYSICS ###################
MODULE PHYSICS
	
CONTAINS

! end of module PHYSICS
END MODULE PHYSICS
! ############### TASK ####################
MODULE TASK
CONTAINS
! ----------- init ------------------
! global initialization
SUBROUTINE INIT()
	USE GRID
	
	CALL MAKE_GRID() ! make grid
END SUBROUTINE INIT
! ----------- data ------------------
! ----------- tests -----------------
! test
SUBROUTINE TEST()
	
END SUBROUTINE TEST
! test grid
SUBROUTINE TEST_GRID()
	USE GRID
	USE MATHIO
	
	CALL EXPORT('XS',XS)
	CALL EXPORT('KS',KS)
END SUBROUTINE TEST_GRID
! end of module task
END MODULE TASK
! ############## PROGRAM ##################
PROGRAM MAIN
	USE TASK
	PRINT *, '------------ VMC -------------'
	CALL INIT()
	
!	CALL TEST()
	CALL TEST_GRID()
END PROGRAM MAIN