! ############### CONST ###################
MODULE CONST
	COMPLEX, PARAMETER :: Z0 = (0.,0.), Z1 = (1.,0.), ZI = (0.,1.)
	REAL, PARAMETER :: PI = 4*ATAN(1.)
END MODULE CONST
! ############### MODEL ###################
MODULE MODEL
END MODULE MODEL
! ############# PHYSICS ###################
MODULE PHYSICS
CONTAINS

! end of module PHYSICS
END MODULE PHYSICS
! ############### TASK ####################
MODULE TASK
CONTAINS
! ----------- data -----------------
! ----------- tests -----------------
! test
SUBROUTINE TEST()
	
END SUBROUTINE TEST
! end of module task
END MODULE TASK
! ############## PROGRAM ##################
PROGRAM MAIN
	USE TASK
	PRINT *, '------------ VMC -------------'
	
END PROGRAM MAIN