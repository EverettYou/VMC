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
	INTEGER :: N ! number of sites
	INTEGER, ALLOCATABLE :: XS(:,:) ! x grid (N,2)
	REAL,    ALLOCATABLE :: KS(:,:) ! k grid (N,2)
CONTAINS
! make grid
SUBROUTINE MAKE_GRID()
	USE CONST
	USE MODEL
	INTEGER :: I, I1, I2
	
	N = L**2 ! number of sites
	ALLOCATE(XS(N,2), KS(N,2)) ! allocate x and k grid
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
	DO I1 = 0, L-1
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
	COMPLEX, ALLOCATABLE :: SLATER(:,:) ! Slater matrix (2*N,N)
CONTAINS
! ---------- Slater det --------------
! set Slater matrix
SUBROUTINE SET_SLATER(M)
! M - mass
	USE CONST
	USE GRID
	REAL, INTENT(IN) :: M
	! local variables
	INTEGER :: IX, IK
	
	ALLOCATE(SLATER(2*N, N)) ! allocate Slater matrix
	FORALL (IX = 1:N, IK = 1:N)
		SLATER(2*IX-1:2*IX, IK) = UFUN(KS(IK,:),M)*EXP(ZI*SUM(XS(IX,:)*KS(IK,:)))
	END FORALL
END SUBROUTINE SET_SLATER
! u function
PURE FUNCTION UFUN(K, M) RESULT(U)
! K - momentum vector, M - mass
! U - state vector
	USE CONST
	REAL, INTENT(IN) :: K(2), M
	COMPLEX :: U(2)
	! local variables
	REAL :: H(3)
	
	! set bare h vector
	H(1) = SIN(K(1))
	H(2) = 2.-COS(K(1))-COS(K(2))-M
	H(3) = SIN(K(2))
	! normalize h vector (must insure fully gap)
	H = H/SQRT(SUM(H**2))
	! construct state vector
	IF (H(3) == 1.) THEN
		U(1) = Z0
		U(2) = Z1
	ELSE
		U(1) = CMPLX(1.-H(3))
		U(2) = -H(1)+ZI*H(2)
		U = U/SQRT(SUM(CONJG(U)*U))
	END IF
END FUNCTION UFUN
! make
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
	USE PHYSICS
	USE MATHIO
	
	CALL SET_SLATER(1.)
END SUBROUTINE TEST
! test grid
SUBROUTINE TEST_GRID()
	USE GRID
	USE MATHIO
	
	CALL EXPORT('XS',XS)
	CALL EXPORT('KS',KS)
END SUBROUTINE TEST_GRID
! test Slater
SUBROUTINE TEST_SLATER()
	USE PHYSICS
	USE MATHIO
	
	CALL SET_SLATER(1.)
	CALL EXPORT('SLATER',SLATER)
END SUBROUTINE TEST_SLATER
! end of module task
END MODULE TASK
! ############## PROGRAM ##################
PROGRAM MAIN
	USE TASK
	PRINT *, '------------ VMC -------------'
	CALL INIT()
	
!	CALL TEST()
	CALL TEST_GRID()
	CALL TEST_SLATER()
END PROGRAM MAIN