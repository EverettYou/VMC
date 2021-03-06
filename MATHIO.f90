MODULE MATHIO
IMPLICIT NONE
INTERFACE EXPORT
	MODULE PROCEDURE EXPORT_IVEC
	MODULE PROCEDURE EXPORT_SVEC
	MODULE PROCEDURE EXPORT_DVEC
	MODULE PROCEDURE EXPORT_CVEC
	MODULE PROCEDURE EXPORT_ZVEC
	MODULE PROCEDURE EXPORT_IMAT
	MODULE PROCEDURE EXPORT_SMAT
	MODULE PROCEDURE EXPORT_DMAT
	MODULE PROCEDURE EXPORT_CMAT
	MODULE PROCEDURE EXPORT_ZMAT
	MODULE PROCEDURE EXPORT_ITEN
	MODULE PROCEDURE EXPORT_STEN
	MODULE PROCEDURE EXPORT_DTEN
	MODULE PROCEDURE EXPORT_CTEN
	MODULE PROCEDURE EXPORT_ZTEN
	MODULE PROCEDURE EXPORT_ITEN4
	MODULE PROCEDURE EXPORT_STEN4
	MODULE PROCEDURE EXPORT_DTEN4
	MODULE PROCEDURE EXPORT_CTEN4
	MODULE PROCEDURE EXPORT_ZTEN4
END INTERFACE
INTERFACE IMPORT
	MODULE PROCEDURE IMPORT_IVEC
	MODULE PROCEDURE IMPORT_DVEC
	MODULE PROCEDURE IMPORT_ZVEC
	MODULE PROCEDURE IMPORT_IMAT
	MODULE PROCEDURE IMPORT_DMAT
	MODULE PROCEDURE IMPORT_ZMAT
	MODULE PROCEDURE IMPORT_ITEN
	MODULE PROCEDURE IMPORT_DTEN
	MODULE PROCEDURE IMPORT_ZTEN
	MODULE PROCEDURE IMPORT_ITEN4
	MODULE PROCEDURE IMPORT_DTEN4
	MODULE PROCEDURE IMPORT_ZTEN4
END INTERFACE
LOGICAL :: MATHIO_MESSAGE = .TRUE.
CONTAINS

SUBROUTINE EXPORT_IVEC(FILENAME, VEC)
	IMPLICIT NONE
	INTENT(IN)   :: FILENAME, VEC
	INTEGER      :: VEC(:)
	CHARACTER(*) :: FILENAME
    INTEGER*1, PARAMETER :: BINARY_TYPE = 9, ARRAY_DEPTH = 1
    INTEGER*1    :: I
	
	IF (MATHIO_MESSAGE) PRINT *, ">>", FILENAME, ".dat"
    OPEN (UNIT = 99, FILE = FILENAME//".dat", &
        STATUS = "REPLACE", ACCESS = "stream")
    WRITE(99) BINARY_TYPE
    WRITE(99) ARRAY_DEPTH
    WRITE(99) (/(SIZE(VEC,I),I = 1, ARRAY_DEPTH)/)
    WRITE(99) VEC
    CLOSE(99)
END SUBROUTINE EXPORT_IVEC

SUBROUTINE EXPORT_SVEC(FILENAME, VEC)
	IMPLICIT NONE
	INTENT(IN)   :: FILENAME, VEC
	REAL*4       :: VEC(:)
	CHARACTER(*) :: FILENAME
    INTEGER*1, PARAMETER :: BINARY_TYPE = 12, ARRAY_DEPTH = 1
    INTEGER*1    :: I
	
	IF (MATHIO_MESSAGE) PRINT *, ">>", FILENAME, ".dat"
    OPEN (UNIT = 99, FILE = FILENAME//".dat", &
        STATUS = "REPLACE", ACCESS = "stream")
    WRITE(99) BINARY_TYPE
    WRITE(99) ARRAY_DEPTH
    WRITE(99) (/(SIZE(VEC,I),I = 1, ARRAY_DEPTH)/)
    WRITE(99) VEC
    CLOSE(99)
END SUBROUTINE EXPORT_SVEC

SUBROUTINE EXPORT_DVEC(FILENAME, VEC)
	IMPLICIT NONE
	INTENT(IN)   :: FILENAME, VEC
	REAL*8       :: VEC(:)
	CHARACTER(*) :: FILENAME
    INTEGER*1, PARAMETER :: BINARY_TYPE = 13, ARRAY_DEPTH = 1
    INTEGER*1    :: I
	
	IF (MATHIO_MESSAGE) PRINT *, ">>", FILENAME, ".dat"
    OPEN (UNIT = 99, FILE = FILENAME//".dat", &
        STATUS = "REPLACE", ACCESS = "stream")
    WRITE(99) BINARY_TYPE
    WRITE(99) ARRAY_DEPTH
    WRITE(99) (/(SIZE(VEC,I),I = 1, ARRAY_DEPTH)/)
    WRITE(99) VEC
    CLOSE(99)
END SUBROUTINE EXPORT_DVEC

SUBROUTINE EXPORT_CVEC(FILENAME, VEC)
	IMPLICIT NONE
	INTENT(IN)   :: FILENAME, VEC
	COMPLEX*8    :: VEC(:)
	CHARACTER(*) :: FILENAME
    INTEGER*1, PARAMETER :: BINARY_TYPE = 4, ARRAY_DEPTH = 1
    INTEGER*1    :: I
	
	IF (MATHIO_MESSAGE) PRINT *, ">>", FILENAME, ".dat"
    OPEN (UNIT = 99, FILE = FILENAME//".dat", &
        STATUS = "REPLACE", ACCESS = "stream")
    WRITE(99) BINARY_TYPE
    WRITE(99) ARRAY_DEPTH
    WRITE(99) (/(SIZE(VEC,I),I = 1, ARRAY_DEPTH)/)
    WRITE(99) VEC
    CLOSE(99)
END SUBROUTINE EXPORT_CVEC

SUBROUTINE EXPORT_ZVEC(FILENAME, VEC)
	IMPLICIT NONE
	INTENT(IN)   :: FILENAME, VEC
	COMPLEX*16   :: VEC(:)
	CHARACTER(*) :: FILENAME
    INTEGER*1, PARAMETER :: BINARY_TYPE = 5, ARRAY_DEPTH = 1
    INTEGER*1    :: I
	
	IF (MATHIO_MESSAGE) PRINT *, ">>", FILENAME, ".dat"
    OPEN (UNIT = 99, FILE = FILENAME//".dat", &
        STATUS = "REPLACE", ACCESS = "stream")
    WRITE(99) BINARY_TYPE
    WRITE(99) ARRAY_DEPTH
    WRITE(99) (/(SIZE(VEC,I),I = 1, ARRAY_DEPTH)/)
    WRITE(99) VEC
    CLOSE(99)
END SUBROUTINE EXPORT_ZVEC

SUBROUTINE EXPORT_IMAT(FILENAME, MAT)
	IMPLICIT NONE
	INTENT(IN)   :: FILENAME, MAT
	INTEGER      :: MAT(:,:)
	CHARACTER(*) :: FILENAME
    INTEGER*1, PARAMETER :: BINARY_TYPE = 9, ARRAY_DEPTH = 2
    INTEGER*1    :: I
	
	IF (MATHIO_MESSAGE) PRINT *, ">>", FILENAME, ".dat"
    OPEN (UNIT = 99, FILE = FILENAME//".dat", &
        STATUS = "REPLACE", ACCESS = "stream")
    WRITE(99) BINARY_TYPE
    WRITE(99) ARRAY_DEPTH
    WRITE(99) (/(SIZE(MAT,I),I = 1, ARRAY_DEPTH)/)
    WRITE(99) MAT
    CLOSE(99)
END SUBROUTINE EXPORT_IMAT

SUBROUTINE EXPORT_SMAT(FILENAME, MAT)
	IMPLICIT NONE
	INTENT(IN)   :: FILENAME, MAT
	REAL*4       :: MAT(:,:)
	CHARACTER(*) :: FILENAME
    INTEGER*1, PARAMETER :: BINARY_TYPE = 12, ARRAY_DEPTH = 2
    INTEGER*1    :: I
	
	IF (MATHIO_MESSAGE) PRINT *, ">>", FILENAME, ".dat"
    OPEN (UNIT = 99, FILE = FILENAME//".dat", &
        STATUS = "REPLACE", ACCESS = "stream")
    WRITE(99) BINARY_TYPE
    WRITE(99) ARRAY_DEPTH
    WRITE(99) (/(SIZE(MAT,I),I = 1, ARRAY_DEPTH)/)
    WRITE(99) MAT
    CLOSE(99)
END SUBROUTINE EXPORT_SMAT

SUBROUTINE EXPORT_DMAT(FILENAME, MAT)
	IMPLICIT NONE
	INTENT(IN)   :: FILENAME, MAT
	REAL*8       :: MAT(:,:)
	CHARACTER(*) :: FILENAME
    INTEGER*1, PARAMETER :: BINARY_TYPE = 13, ARRAY_DEPTH = 2
    INTEGER*1    :: I
	
	IF (MATHIO_MESSAGE) PRINT *, ">>", FILENAME, ".dat"
    OPEN (UNIT = 99, FILE = FILENAME//".dat", &
        STATUS = "REPLACE", ACCESS = "stream")
    WRITE(99) BINARY_TYPE
    WRITE(99) ARRAY_DEPTH
    WRITE(99) (/(SIZE(MAT,I),I = 1, ARRAY_DEPTH)/)
    WRITE(99) MAT
    CLOSE(99)
END SUBROUTINE EXPORT_DMAT

SUBROUTINE EXPORT_CMAT(FILENAME, MAT)
	IMPLICIT NONE
	INTENT(IN)   :: FILENAME, MAT
	COMPLEX*8    :: MAT(:,:)
	CHARACTER(*) :: FILENAME
    INTEGER*1, PARAMETER :: BINARY_TYPE = 4, ARRAY_DEPTH = 2
    INTEGER*1    :: I
	
	IF (MATHIO_MESSAGE) PRINT *, ">>", FILENAME, ".dat"
    OPEN (UNIT = 99, FILE = FILENAME//".dat", &
        STATUS = "REPLACE", ACCESS = "stream")
    WRITE(99) BINARY_TYPE
    WRITE(99) ARRAY_DEPTH
    WRITE(99) (/(SIZE(MAT,I),I = 1, ARRAY_DEPTH)/)
    WRITE(99) MAT
    CLOSE(99)
END SUBROUTINE EXPORT_CMAT

SUBROUTINE EXPORT_ZMAT(FILENAME, MAT)
	IMPLICIT NONE
	INTENT(IN)   :: FILENAME, MAT
	COMPLEX*16   :: MAT(:,:)
	CHARACTER(*) :: FILENAME
    INTEGER*1, PARAMETER :: BINARY_TYPE = 5, ARRAY_DEPTH = 2
    INTEGER*1    :: I
	
	IF (MATHIO_MESSAGE) PRINT *, ">>", FILENAME, ".dat"
    OPEN (UNIT = 99, FILE = FILENAME//".dat", &
        STATUS = "REPLACE", ACCESS = "stream")
    WRITE(99) BINARY_TYPE
    WRITE(99) ARRAY_DEPTH
    WRITE(99) (/(SIZE(MAT,I),I = 1, ARRAY_DEPTH)/)
    WRITE(99) MAT
    CLOSE(99)
END SUBROUTINE EXPORT_ZMAT

SUBROUTINE EXPORT_ITEN(FILENAME, TEN)
	IMPLICIT NONE
	INTENT(IN)   :: FILENAME, TEN
	INTEGER      :: TEN(:,:,:)
	CHARACTER(*) :: FILENAME
    INTEGER*1, PARAMETER :: BINARY_TYPE = 9, ARRAY_DEPTH = 3
    INTEGER*1    :: I
	
	IF (MATHIO_MESSAGE) PRINT *, ">>", FILENAME, ".dat"
    OPEN (UNIT = 99, FILE = FILENAME//".dat", &
        STATUS = "REPLACE", ACCESS = "stream")
    WRITE(99) BINARY_TYPE
    WRITE(99) ARRAY_DEPTH
    WRITE(99) (/(SIZE(TEN,I),I = 1, ARRAY_DEPTH)/)
    WRITE(99) TEN
    CLOSE(99)
END SUBROUTINE EXPORT_ITEN

SUBROUTINE EXPORT_STEN(FILENAME, TEN)
	IMPLICIT NONE
	INTENT(IN)   :: FILENAME, TEN
	REAL*4       :: TEN(:,:,:)
	CHARACTER(*) :: FILENAME
    INTEGER*1, PARAMETER :: BINARY_TYPE = 12, ARRAY_DEPTH = 3
    INTEGER*1    :: I
	
	IF (MATHIO_MESSAGE) PRINT *, ">>", FILENAME, ".dat"
    OPEN (UNIT = 99, FILE = FILENAME//".dat", &
        STATUS = "REPLACE", ACCESS = "stream")
    WRITE(99) BINARY_TYPE
    WRITE(99) ARRAY_DEPTH
    WRITE(99) (/(SIZE(TEN,I),I = 1, ARRAY_DEPTH)/)
    WRITE(99) TEN
    CLOSE(99)
END SUBROUTINE EXPORT_STEN

SUBROUTINE EXPORT_DTEN(FILENAME, TEN)
	IMPLICIT NONE
	INTENT(IN)   :: FILENAME, TEN
	REAL*8       :: TEN(:,:,:)
	CHARACTER(*) :: FILENAME
    INTEGER*1, PARAMETER :: BINARY_TYPE = 13, ARRAY_DEPTH = 3
    INTEGER*1    :: I
	
	IF (MATHIO_MESSAGE) PRINT *, ">>", FILENAME, ".dat"
    OPEN (UNIT = 99, FILE = FILENAME//".dat", &
        STATUS = "REPLACE", ACCESS = "stream")
    WRITE(99) BINARY_TYPE
    WRITE(99) ARRAY_DEPTH
    WRITE(99) (/(SIZE(TEN,I),I = 1, ARRAY_DEPTH)/)
    WRITE(99) TEN
    CLOSE(99)
END SUBROUTINE EXPORT_DTEN

SUBROUTINE EXPORT_CTEN(FILENAME, TEN)
	IMPLICIT NONE
	INTENT(IN)   :: FILENAME, TEN
	COMPLEX*8    :: TEN(:,:,:)
	CHARACTER(*) :: FILENAME
    INTEGER*1, PARAMETER :: BINARY_TYPE = 4, ARRAY_DEPTH = 3
    INTEGER*1    :: I
	
	IF (MATHIO_MESSAGE) PRINT *, ">>", FILENAME, ".dat"
    OPEN (UNIT = 99, FILE = FILENAME//".dat", &
        STATUS = "REPLACE", ACCESS = "stream")
    WRITE(99) BINARY_TYPE
    WRITE(99) ARRAY_DEPTH
    WRITE(99) (/(SIZE(TEN,I),I = 1, ARRAY_DEPTH)/)
    WRITE(99) TEN
    CLOSE(99)
END SUBROUTINE EXPORT_CTEN

SUBROUTINE EXPORT_ZTEN(FILENAME, TEN)
	IMPLICIT NONE
	INTENT(IN)   :: FILENAME, TEN
	COMPLEX*16   :: TEN(:,:,:)
	CHARACTER(*) :: FILENAME
    INTEGER*1, PARAMETER :: BINARY_TYPE = 5, ARRAY_DEPTH = 3
    INTEGER*1    :: I
	
	IF (MATHIO_MESSAGE) PRINT *, ">>", FILENAME, ".dat"
    OPEN (UNIT = 99, FILE = FILENAME//".dat", &
        STATUS = "REPLACE", ACCESS = "stream")
    WRITE(99) BINARY_TYPE
    WRITE(99) ARRAY_DEPTH
    WRITE(99) (/(SIZE(TEN,I),I = 1, ARRAY_DEPTH)/)
    WRITE(99) TEN
    CLOSE(99)
END SUBROUTINE EXPORT_ZTEN

SUBROUTINE EXPORT_ITEN4(FILENAME, TEN)
	IMPLICIT NONE
	INTENT(IN)   :: FILENAME, TEN
	INTEGER      :: TEN(:,:,:,:)
	CHARACTER(*) :: FILENAME
    INTEGER*1, PARAMETER :: BINARY_TYPE = 9, ARRAY_DEPTH = 4
    INTEGER*1    :: I
	
	IF (MATHIO_MESSAGE) PRINT *, ">>", FILENAME, ".dat"
    OPEN (UNIT = 99, FILE = FILENAME//".dat", &
        STATUS = "REPLACE", ACCESS = "stream")
    WRITE(99) BINARY_TYPE
    WRITE(99) ARRAY_DEPTH
    WRITE(99) (/(SIZE(TEN,I),I = 1, ARRAY_DEPTH)/)
    WRITE(99) TEN
    CLOSE(99)
END SUBROUTINE EXPORT_ITEN4

SUBROUTINE EXPORT_STEN4(FILENAME, TEN)
	IMPLICIT NONE
	INTENT(IN)   :: FILENAME, TEN
	REAL*4       :: TEN(:,:,:,:)
	CHARACTER(*) :: FILENAME
    INTEGER*1, PARAMETER :: BINARY_TYPE = 12, ARRAY_DEPTH = 4
    INTEGER*1    :: I
	
	IF (MATHIO_MESSAGE) PRINT *, ">>", FILENAME, ".dat"
    OPEN (UNIT = 99, FILE = FILENAME//".dat", &
        STATUS = "REPLACE", ACCESS = "stream")
    WRITE(99) BINARY_TYPE
    WRITE(99) ARRAY_DEPTH
    WRITE(99) (/(SIZE(TEN,I),I = 1, ARRAY_DEPTH)/)
    WRITE(99) TEN
    CLOSE(99)
END SUBROUTINE EXPORT_STEN4

SUBROUTINE EXPORT_DTEN4(FILENAME, TEN)
	IMPLICIT NONE
	INTENT(IN)   :: FILENAME, TEN
	REAL*8       :: TEN(:,:,:,:)
	CHARACTER(*) :: FILENAME
    INTEGER*1, PARAMETER :: BINARY_TYPE = 13, ARRAY_DEPTH = 4
    INTEGER*1    :: I
	
	IF (MATHIO_MESSAGE) PRINT *, ">>", FILENAME, ".dat"
    OPEN (UNIT = 99, FILE = FILENAME//".dat", &
        STATUS = "REPLACE", ACCESS = "stream")
    WRITE(99) BINARY_TYPE
    WRITE(99) ARRAY_DEPTH
    WRITE(99) (/(SIZE(TEN,I),I = 1, ARRAY_DEPTH)/)
    WRITE(99) TEN
    CLOSE(99)
END SUBROUTINE EXPORT_DTEN4

SUBROUTINE EXPORT_CTEN4(FILENAME, TEN)
	IMPLICIT NONE
	INTENT(IN)   :: FILENAME, TEN
	COMPLEX*8    :: TEN(:,:,:,:)
	CHARACTER(*) :: FILENAME
    INTEGER*1, PARAMETER :: BINARY_TYPE = 4, ARRAY_DEPTH = 4
    INTEGER*1    :: I
	
	IF (MATHIO_MESSAGE) PRINT *, ">>", FILENAME, ".dat"
    OPEN (UNIT = 99, FILE = FILENAME//".dat", &
        STATUS = "REPLACE", ACCESS = "stream")
    WRITE(99) BINARY_TYPE
    WRITE(99) ARRAY_DEPTH
    WRITE(99) (/(SIZE(TEN,I),I = 1, ARRAY_DEPTH)/)
    WRITE(99) TEN
    CLOSE(99)
END SUBROUTINE EXPORT_CTEN4

SUBROUTINE EXPORT_ZTEN4(FILENAME, TEN)
	IMPLICIT NONE
	INTENT(IN)   :: FILENAME, TEN
	COMPLEX*16   :: TEN(:,:,:,:)
	CHARACTER(*) :: FILENAME
    INTEGER*1, PARAMETER :: BINARY_TYPE = 5, ARRAY_DEPTH = 4
    INTEGER*1    :: I
	
	IF (MATHIO_MESSAGE) PRINT *, ">>", FILENAME, ".dat"
    OPEN (UNIT = 99, FILE = FILENAME//".dat", &
        STATUS = "REPLACE", ACCESS = "stream")
    WRITE(99) BINARY_TYPE
    WRITE(99) ARRAY_DEPTH
    WRITE(99) (/(SIZE(TEN,I),I = 1, ARRAY_DEPTH)/)
    WRITE(99) TEN
    CLOSE(99)
END SUBROUTINE EXPORT_ZTEN4


SUBROUTINE IMPORT_IVEC(FILENAME, VEC)
    IMPLICIT NONE
    INTENT(IN)    :: FILENAME
    INTENT(OUT)   :: VEC
    INTEGER       :: VEC(:)
	CHARACTER(*)  :: FILENAME
    INTEGER       :: N
    INTEGER*1     :: BINARY_TYPE, ARRAY_DEPTH
    
	IF (MATHIO_MESSAGE) PRINT *, "<<", FILENAME, ".dat"
    OPEN (UNIT = 99, FILE = FILENAME//".dat", &
        STATUS = "UNKNOWN", ACCESS = "STREAM")
    READ(99) BINARY_TYPE
    READ(99) ARRAY_DEPTH
    IF (BINARY_TYPE == 9 .AND. ARRAY_DEPTH == 1) THEN
        READ(99) N
        READ(99) VEC(1:N)
    ELSE
        IF (MATHIO_MESSAGE) PRINT *, "Warning: IMPORT::data file inconsistent with &
            target variable."
    END IF
    CLOSE(99)
END SUBROUTINE IMPORT_IVEC

SUBROUTINE IMPORT_DVEC(FILENAME, VEC)
    IMPLICIT NONE
    INTENT(IN)    :: FILENAME
    INTENT(OUT)   :: VEC
    REAL*8        :: VEC(:)
	CHARACTER(*)  :: FILENAME
    INTEGER       :: N
    INTEGER*1     :: BINARY_TYPE, ARRAY_DEPTH
    
	IF (MATHIO_MESSAGE) PRINT *, "<<", FILENAME, ".dat"
    OPEN (UNIT = 99, FILE = FILENAME//".dat", &
        STATUS = "UNKNOWN", ACCESS = "STREAM")
    READ(99) BINARY_TYPE
    READ(99) ARRAY_DEPTH
    IF (BINARY_TYPE == 13 .AND. ARRAY_DEPTH == 1) THEN
        READ(99) N
        READ(99) VEC(1:N)
    ELSE
        IF (MATHIO_MESSAGE) PRINT *, "Warning: IMPORT::data file inconsistent with &
            target variable."
    END IF
    CLOSE(99)
END SUBROUTINE IMPORT_DVEC

SUBROUTINE IMPORT_ZVEC(FILENAME, VEC)
    IMPLICIT NONE
    INTENT(IN)    :: FILENAME
    INTENT(OUT)   :: VEC
    COMPLEX*16    :: VEC(:)
	CHARACTER(*)  :: FILENAME
    INTEGER       :: N
    INTEGER*1     :: BINARY_TYPE, ARRAY_DEPTH
    
	IF (MATHIO_MESSAGE) PRINT *, "<<", FILENAME, ".dat"
    OPEN (UNIT = 99, FILE = FILENAME//".dat", &
        STATUS = "UNKNOWN", ACCESS = "STREAM")
    READ(99) BINARY_TYPE
    READ(99) ARRAY_DEPTH
    IF (BINARY_TYPE == 5 .AND. ARRAY_DEPTH == 1) THEN
        READ(99) N
        READ(99) VEC(1:N)
    ELSE
        IF (MATHIO_MESSAGE) PRINT *, "Warning: IMPORT::data file inconsistent with &
            target variable."
    END IF
    CLOSE(99)
END SUBROUTINE IMPORT_ZVEC

SUBROUTINE IMPORT_IMAT(FILENAME, MAT)
    IMPLICIT NONE
    INTENT(IN)    :: FILENAME
    INTENT(OUT)   :: MAT
    INTEGER       :: MAT(:,:)
	CHARACTER(*)  :: FILENAME
    INTEGER       :: N1, N2
    INTEGER*1     :: BINARY_TYPE, ARRAY_DEPTH
    
	IF (MATHIO_MESSAGE) PRINT *, "<<", FILENAME, ".dat"
    OPEN (UNIT = 99, FILE = FILENAME//".dat", &
        STATUS = "UNKNOWN", ACCESS = "STREAM")
    READ(99) BINARY_TYPE
    READ(99) ARRAY_DEPTH
    IF (BINARY_TYPE == 9 .AND. ARRAY_DEPTH == 2) THEN
        READ(99) N1, N2
        READ(99) MAT(1:N1, 1:N2)
    ELSE
        IF (MATHIO_MESSAGE) PRINT *, "Warning: IMPORT::data file inconsistent with &
            target variable."
    END IF
    CLOSE(99)
END SUBROUTINE IMPORT_IMAT

SUBROUTINE IMPORT_DMAT(FILENAME, MAT)
    IMPLICIT NONE
    INTENT(IN)    :: FILENAME
    INTENT(OUT)   :: MAT
    REAL*8        :: MAT(:,:)
	CHARACTER(*)  :: FILENAME
    INTEGER       :: N1, N2
    INTEGER*1     :: BINARY_TYPE, ARRAY_DEPTH
    
	IF (MATHIO_MESSAGE) PRINT *, "<<", FILENAME, ".dat"
    OPEN (UNIT = 99, FILE = FILENAME//".dat", &
        STATUS = "UNKNOWN", ACCESS = "STREAM")
    READ(99) BINARY_TYPE
    READ(99) ARRAY_DEPTH
    IF (BINARY_TYPE == 13 .AND. ARRAY_DEPTH == 2) THEN
        READ(99) N1, N2
        READ(99) MAT(1:N1, 1:N2)
    ELSE
        IF (MATHIO_MESSAGE) PRINT *, "Warning: IMPORT::data file inconsistent with &
            target variable."
    END IF
    CLOSE(99)
END SUBROUTINE IMPORT_DMAT

SUBROUTINE IMPORT_ZMAT(FILENAME, MAT)
    IMPLICIT NONE
    INTENT(IN)    :: FILENAME
    INTENT(OUT)   :: MAT
    COMPLEX*16    :: MAT(:,:)
	CHARACTER(*)  :: FILENAME
    INTEGER       :: N1, N2
    INTEGER*1     :: BINARY_TYPE, ARRAY_DEPTH
    
	IF (MATHIO_MESSAGE) PRINT *, "<<", FILENAME, ".dat"
    OPEN (UNIT = 99, FILE = FILENAME//".dat", &
        STATUS = "UNKNOWN", ACCESS = "STREAM")
    READ(99) BINARY_TYPE
    READ(99) ARRAY_DEPTH
    IF (BINARY_TYPE == 5 .AND. ARRAY_DEPTH == 2) THEN
        READ(99) N1, N2
        READ(99) MAT(1:N1, 1:N2)
    ELSE
        IF (MATHIO_MESSAGE) PRINT *, "Warning: IMPORT::data file inconsistent with &
            target variable."
    END IF
    CLOSE(99)
END SUBROUTINE IMPORT_ZMAT

SUBROUTINE IMPORT_ITEN(FILENAME, TEN)
    IMPLICIT NONE
    INTENT(IN)    :: FILENAME
    INTENT(OUT)   :: TEN
    INTEGER       :: TEN(:,:,:)
	CHARACTER(*)  :: FILENAME
    INTEGER       :: N1, N2, N3
    INTEGER*1     :: BINARY_TYPE, ARRAY_DEPTH
    
	IF (MATHIO_MESSAGE) PRINT *, "<<", FILENAME, ".dat"
    OPEN (UNIT = 99, FILE = FILENAME//".dat", &
        STATUS = "UNKNOWN", ACCESS = "STREAM")
    READ(99) BINARY_TYPE
    READ(99) ARRAY_DEPTH
    IF (BINARY_TYPE == 9 .AND. ARRAY_DEPTH == 3) THEN
        READ(99) N1, N2, N3
        READ(99) TEN(1:N1, 1:N2, 1:N3)
    ELSE
        IF (MATHIO_MESSAGE) PRINT *, "Warning: IMPORT::data file inconsistent with &
            target variable."
    END IF
    CLOSE(99)
END SUBROUTINE IMPORT_ITEN

SUBROUTINE IMPORT_DTEN(FILENAME, TEN)
    IMPLICIT NONE
    INTENT(IN)    :: FILENAME
    INTENT(OUT)   :: TEN
    REAL*8        :: TEN(:,:,:)
	CHARACTER(*)  :: FILENAME
    INTEGER       :: N1, N2, N3
    INTEGER*1     :: BINARY_TYPE, ARRAY_DEPTH
    
	IF (MATHIO_MESSAGE) PRINT *, "<<", FILENAME, ".dat"
    OPEN (UNIT = 99, FILE = FILENAME//".dat", &
        STATUS = "UNKNOWN", ACCESS = "STREAM")
    READ(99) BINARY_TYPE
    READ(99) ARRAY_DEPTH
    IF (BINARY_TYPE == 13 .AND. ARRAY_DEPTH == 3) THEN
        READ(99) N1, N2, N3
        READ(99) TEN(1:N1, 1:N2, 1:N3)
    ELSE
        IF (MATHIO_MESSAGE) PRINT *, "Warning: IMPORT::data file inconsistent with &
            target variable."
    END IF
    CLOSE(99)
END SUBROUTINE IMPORT_DTEN

SUBROUTINE IMPORT_ZTEN(FILENAME, TEN)
    IMPLICIT NONE
    INTENT(IN)    :: FILENAME
    INTENT(OUT)   :: TEN
    COMPLEX*16    :: TEN(:,:,:)
	CHARACTER(*)  :: FILENAME
    INTEGER       :: N1, N2, N3
    INTEGER*1     :: BINARY_TYPE, ARRAY_DEPTH
    
	IF (MATHIO_MESSAGE) PRINT *, "<<", FILENAME, ".dat"
    OPEN (UNIT = 99, FILE = FILENAME//".dat", &
        STATUS = "UNKNOWN", ACCESS = "STREAM")
    READ(99) BINARY_TYPE
    READ(99) ARRAY_DEPTH
    IF (BINARY_TYPE == 5 .AND. ARRAY_DEPTH == 3) THEN
        READ(99) N1, N2, N3
        READ(99) TEN(1:N1, 1:N2, 1:N3)
    ELSE
        IF (MATHIO_MESSAGE) PRINT *, "Warning: IMPORT::data file inconsistent with &
            target variable."
    END IF
    CLOSE(99)
END SUBROUTINE IMPORT_ZTEN

SUBROUTINE IMPORT_ITEN4(FILENAME, TEN)
    IMPLICIT NONE
    INTENT(IN)    :: FILENAME
    INTENT(OUT)   :: TEN
    INTEGER       :: TEN(:,:,:,:)
	CHARACTER(*)  :: FILENAME
    INTEGER       :: N1, N2, N3, N4
    INTEGER*1     :: BINARY_TYPE, ARRAY_DEPTH
    
	IF (MATHIO_MESSAGE) PRINT *, "<<", FILENAME, ".dat"
    OPEN (UNIT = 99, FILE = FILENAME//".dat", &
        STATUS = "UNKNOWN", ACCESS = "STREAM")
    READ(99) BINARY_TYPE
    READ(99) ARRAY_DEPTH
    IF (BINARY_TYPE == 9 .AND. ARRAY_DEPTH == 4) THEN
        READ(99) N1, N2, N3, N4
        READ(99) TEN(1:N1, 1:N2, 1:N3, 1:N4)
    ELSE
        IF (MATHIO_MESSAGE) PRINT *, "Warning: IMPORT::data file inconsistent with &
            target variable."
    END IF
    CLOSE(99)
END SUBROUTINE IMPORT_ITEN4

SUBROUTINE IMPORT_DTEN4(FILENAME, TEN)
    IMPLICIT NONE
    INTENT(IN)    :: FILENAME
    INTENT(OUT)   :: TEN
    REAL*8        :: TEN(:,:,:,:)
	CHARACTER(*)  :: FILENAME
    INTEGER       :: N1, N2, N3, N4
    INTEGER*1     :: BINARY_TYPE, ARRAY_DEPTH
    
	IF (MATHIO_MESSAGE) PRINT *, "<<", FILENAME, ".dat"
    OPEN (UNIT = 99, FILE = FILENAME//".dat", &
        STATUS = "UNKNOWN", ACCESS = "STREAM")
    READ(99) BINARY_TYPE
    READ(99) ARRAY_DEPTH
    IF (BINARY_TYPE == 13 .AND. ARRAY_DEPTH == 4) THEN
        READ(99) N1, N2, N3, N4
        READ(99) TEN(1:N1, 1:N2, 1:N3, 1:N4)
    ELSE
        IF (MATHIO_MESSAGE) PRINT *, "Warning: IMPORT::data file inconsistent with &
            target variable."
    END IF
    CLOSE(99)
END SUBROUTINE IMPORT_DTEN4

SUBROUTINE IMPORT_ZTEN4(FILENAME, TEN)
    IMPLICIT NONE
    INTENT(IN)    :: FILENAME
    INTENT(OUT)   :: TEN
    COMPLEX*16    :: TEN(:,:,:,:)
	CHARACTER(*)  :: FILENAME
    INTEGER       :: N1, N2, N3, N4
    INTEGER*1     :: BINARY_TYPE, ARRAY_DEPTH
    
	IF (MATHIO_MESSAGE) PRINT *, "<<", FILENAME, ".dat"
    OPEN (UNIT = 99, FILE = FILENAME//".dat", &
        STATUS = "UNKNOWN", ACCESS = "STREAM")
    READ(99) BINARY_TYPE
    READ(99) ARRAY_DEPTH
    IF (BINARY_TYPE == 5 .AND. ARRAY_DEPTH == 4) THEN
        READ(99) N1, N2, N3, N4
        READ(99) TEN(1:N1, 1:N2, 1:N3, 1:N4)
    ELSE
        IF (MATHIO_MESSAGE) PRINT *, "Warning: IMPORT::data file inconsistent with &
            target variable."
    END IF
    CLOSE(99)
END SUBROUTINE IMPORT_ZTEN4

! end of module MATHIO
END MODULE MATHIO