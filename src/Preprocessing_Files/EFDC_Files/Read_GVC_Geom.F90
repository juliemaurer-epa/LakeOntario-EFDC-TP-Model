!------------------------------------------------------------
Program Read_GVC_Geom
!------------------------------------------------------------
! This program extract Generalized Vertical Coordinate (GVC) 
! for individual horizontal grid locations from
! a binary file. 

IMPLICIT NONE

INTEGER(kind=4) :: I, J, K, ISURF, IX, JY
INTEGER(kind=4), PARAMETER :: IC = 256  ! Number of columns
INTEGER(kind=4), PARAMETER :: JC = 133  ! Number of rows
INTEGER(kind=4), PARAMETER :: LC = 13778  ! Number of surface cells
INTEGER(kind=4), PARAMETER :: KC = 10   ! Maximum number of layers
INTEGER(kind=4), PARAMETER :: KCP1 = 11
INTEGER, DIMENSION(LC) :: ZGVC, ZGVCU, ZGVCV
LOGICAL, DIMENSION(LC,KC) :: LGVC, LGVCU, LGVCV, LGVCW

INTEGER, DIMENSION(IC, JC) :: NZ
INTEGER, DIMENSION(LC) :: II
INTEGER, DIMENSION(LC) :: JJ

NZ = 0
II = 0
JJ = 0

OPEN(32, FILE = 'gcm_grid.txt', ACTION = 'READ')

DO I = 1, LC
   READ(32,*) II(I), JJ(I), ISURF
ENDDO

CLOSE(32)

!--------------------------------------------------------------------------------------------------------
! ZGVC maximum layer index (i.e., if horizontal grid location has 5 layers in GVC scheme, 
!      this will be = 5)
!
! ZGVCU maximum layer index at psuedo-west interface
! ZGVCV maximum layer index at psuedo-south interface
!
! LGVC layer mask at cell center (TRUE means layer is active, FALSE means layer is not active 
!      in GVC scheme; same applies for below arrays)
!
! LGVCU layer mask at psuedo-west interface
! LGVCV layer mask at psuedo-south interface
! LGVCW layer mask for vertical interface above layer
!---------------------------------------------------------------------------------------------------------

OPEN(33,FILE = 'gvc_geom.bin', FORM = 'UNFORMATTED', CONVERT = 'LITTLE_ENDIAN')

READ(33) ZGVC   !Max active layer for cell center
READ(33) ZGVCU  !Max active layer at pseudo-west cell face
READ(33) ZGVCV  !Max active layer at pseudo-south cell face

READ(33) LGVC   !Layer mask at cell center
READ(33) LGVCU  !Layer mask at pseudo-west cell face
READ(33) LGVCV  !Layer mask at pseudo-south cell face
READ(33) LGVCW  !Layer mask for interface above layer

CLOSE(33)



OPEN(34, FILE = 'gvc_geom.txt', ACTION = 'WRITE', STATUS = 'UNKNOWN')

DO I = 1, LC
   WRITE(34,*) I, ZGVC(I), ZGVCU(I), ZGVCV(I)
   IX = II(I)
   JY = JJ(I)
   NZ(IX,JY) = ZGVC(I)
ENDDO

write(34,*)

DO I = 1, LC
   DO K = 1, KC
      WRITE(34,*) I, K, LGVC(I,K), LGVCU(I,K), LGVCV(I,K), LGVCW(I,K)
   ENDDO
ENDDO

CLOSE(34)

OPEN(35, FILE = 'nz.dat', ACTION = 'WRITE', STATUS = 'UNKNOWN')
OPEN(36, FILE = 'nz_IJ.dat', ACTION = 'WRITE', STATUS = 'UNKNOWN')

DO J = 1, JC
   WRITE(35,*) NZ(:,J)
ENDDO

CLOSE(35)

DO I = 1, IC
   DO J = 1, JC
      WRITE(36,*) I, J, NZ(I,J)
   ENDDO
ENDDO

CLOSE(36)

END
