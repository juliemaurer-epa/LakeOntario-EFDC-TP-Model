PROGRAM IJ_Depth_NZ

USE NETCDF

IMPLICIT NONE

CHARACTER(LEN=30) :: OUTPUT_FILE  = 'IJ_Depth_NZ.txt'
CHARACTER(LEN=30) :: FILE_NZ = 'nz_IJ.dat'
CHARACTER(LEN=30) :: FILE_DEPTH = 'IJ_depth.txt'

INTEGER(kind=4) :: I, J, IX, JY
INTEGER(kind=4), PARAMETER :: IC = 256  ! Number of columns
INTEGER(kind=4), PARAMETER :: JC = 133  ! Number of rows
INTEGER(kind=4), PARAMETER :: LC = 13778  ! Number of surface cells
INTEGER(kind=4), PARAMETER :: KC = 10   ! Maximum number of vertical layers
INTEGER(kind=4), PARAMETER :: KCP1 = 11

INTEGER(kind=4), DIMENSION(IC,JC) :: NZ_ARR

REAL :: flag, dx, dy, depth
REAL, PARAMETER :: epsilon = 1.0E-06
REAL, DIMENSION(IC, JC) :: DEPTH_ARR

DEPTH_ARR = 0.0
NZ_ARR = 0

OPEN(UNIT = 10, FILE = FILE_DEPTH, ACTION = 'READ')
DO J = 1, JC
   DO I = 1, IC
      READ(10,*) IX, JY, depth, dx, dy
      READ(10,*) flag
      IF (ABS(flag) > epsilon) THEN
          DEPTH_ARR(IX,JY) = depth
      ENDIF
   ENDDO
ENDDO

CLOSE(UNIT = 10)

OPEN(UNIT = 20, FILE = FILE_NZ, ACTION = 'READ')
DO I = 1, IC
   DO J = 1, JC
      READ(20,*) IX, JY, NZ_ARR(IX,JY)
   ENDDO
ENDDO

CLOSE(UNIT = 20)

OPEN(UNIT = 30, FILE = OUTPUT_FILE, ACTION = 'WRITE')
DO J = 1, JC
   DO I = 1, IC
      WRITE(30,*) I, J, DEPTH_ARR(I,J), NZ_ARR(I,J)
   ENDDO
ENDDO

CLOSE(UNIT = 30)

END PROGRAM
