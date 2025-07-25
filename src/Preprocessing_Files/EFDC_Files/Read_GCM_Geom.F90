!------------------------------------------------------------
Program Read_GCM_Geom
!------------------------------------------------------------
! This program extract horizontal and vertical (sigma) 
! grid dimensions, plus masking for boundary conditions from
! a binary file. 

IMPLICIT NONE

INTEGER(kind=4) :: I, J, K
INTEGER(kind=4), PARAMETER :: IC = 256  ! Number of columns
INTEGER(kind=4), PARAMETER :: JC = 133  ! Number of rows
INTEGER(kind=4), PARAMETER :: KC = 10   ! Maximum number of layers
INTEGER(kind=4), PARAMETER :: KCP1 = 11
REAL, DIMENSION(IC,JC) :: FSM, H, DX, DY
REAL, DIMENSION(KCP1) :: DZ, DZZ

!--------------------------------------------------------------------------------------------------------
! DZ sigma thickness of each layer (i.e., as a fraction, 
!      with sum of all fractions = 1.0) array index of (KC+1) should always be equal to zero.
! DZZ average sigma thickness across adjacent layers 
!       (array indices of [KC] and [KC+1] will always be set to zero).
! H water depth in meters relative to datum used in model 
!     linkage (i.e., difference b/w datum elevation and cell-centered bed elevation).
! DX cell dimension in pseudo-west/east direction at cell center (meters)
! DY cell dimension in pseudo-north/south direction at cell center (meters)
! FSM - land/water mask (1.0 = water, 0.0 = land, -1 = flow boundary cell, -2 = pressure boundary cell)
!---------------------------------------------------------------------------------------------------------

OPEN(32,FILE='gcm_geom.bin',FORM='UNFORMATTED', CONVERT='LITTLE_ENDIAN')
READ(32) DZ, DZZ
READ(32) H, DX, DY, FSM
CLOSE(32)


OPEN(33, FILE = 'gcm_geom.txt', ACTION= 'WRITE', STATUS = 'UNKNOWN')

OPEN(34, FILE = 'dxdy.dat', ACTION= 'WRITE', STATUS = 'UNKNOWN')

DO K = 1, KCP1
   WRITE(33,*) K, DZ(K), DZZ(K)
ENDDO

write(33,*)

DO J = 1, JC
   DO I = 1, IC
      WRITE(33,*) I, J, H(I,J), DX(I,J), DY(I,J), FSM(I,J)
   ENDDO
ENDDO

WRITE(34,*) 'dx'
DO J = 1, JC
   WRITE(34,*) DX(:,J)
ENDDO

WRITE(34,*) 'dy'
DO J = 1, JC
   WRITE(34,*) DY(:,J)
ENDDO

CLOSE(33)
CLOSE(34)

END
