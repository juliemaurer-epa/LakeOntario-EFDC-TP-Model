!------------------------------------------------------------
Program Read_LatLon
!------------------------------------------------------------
! This program reads latitudes and longitudes from a  
! text file. 

IMPLICIT NONE

INTEGER(kind=4) :: I, J, IX, JY
INTEGER(kind=4), PARAMETER :: IC = 256  ! Number of columns
INTEGER(kind=4), PARAMETER :: JC = 133  ! Number of rows
INTEGER(kind=4), PARAMETER :: LC = 13778  ! Number of surface cells
INTEGER(kind=4), PARAMETER :: KC = 10   ! Maximum number of layers

REAL, DIMENSION(IC, JC) :: LAT, LON, TRUE_NORTH, TRUE_EAST
REAL, DIMENSION(IC, JC) :: SQRTG
REAL, DIMENSION(LC) :: latitude, longitude, tre, trn, g11, g12, g21, g22, wnd
INTEGER, DIMENSION(LC) :: II
INTEGER, DIMENSION(LC) :: JJ

LAT = -9999
LON = -9999
TRUE_NORTH = -9999 
TRUE_EAST = -9999
II = 0
JJ = 0

OPEN(32, FILE = 'lxly.txt', ACTION = 'READ')

READ(32,*)    ! Header line

DO I = 1, LC
   READ(32,*) II(I), JJ(I), tre(I), trn(I), g11(I), g12(I), g21(I), g22(I), wnd(I), latitude(I), longitude(I)
ENDDO

CLOSE(32)

OPEN(33, FILE = 'latlon.dat', STATUS = 'UNKNOWN')
OPEN(34, FILE = 'lxly.dat', STATUS = 'UNKNOWN')

DO I = 1, LC
   IX = II(I)
   JY = JJ(I)
   LAT(IX,JY) = latitude(I)
   LON(IX,JY) = longitude(I)
   TRUE_EAST(IX,JY) = tre(I)
   TRUE_NORTH(IX,JY) = trn(I)
   SQRTG(IX,JY) = SQRT(g11(I) * g22(I) - g12(I) * g21(I))
ENDDO

WRITE(33,*) 'lat'
WRITE(34,*) 'sqrtg'

DO J = 1, JC
   WRITE(33,*) LAT(:,J)
ENDDO

WRITE(33,*) 'lon'

DO J = 1, JC
   WRITE(33,*) LON(:,J)
ENDDO

WRITE(33,*) 'true_north'

DO J = 1, JC
   WRITE(33,*) TRUE_NORTH(:,J)
ENDDO

WRITE(33,*) 'true_east'

DO J = 1, JC
   WRITE(33,*) TRUE_EAST(:,J)
ENDDO

DO I = 1, IC
   WRITE(34,*) SQRTG(I,:)
ENDDO

CLOSE(33)
CLOSE(34)

END PROGRAM



