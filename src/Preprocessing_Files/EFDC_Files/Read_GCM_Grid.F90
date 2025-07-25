!------------------------------------------------------------
Program Read_GCM_Grid
!------------------------------------------------------------
! This program provides I/J mapping for active model grids.

IMPLICIT NONE

INTEGER(kind=4) :: LC, L, NX, NY, NZ
INTEGER(kind=4) :: IC = 256, JC = 133
INTEGER(kind=4), DIMENSION(:), ALLOCATABLE :: IL, JL
INTEGER(kind=4), DIMENSION(:,:), ALLOCATABLE :: LIJ

OPEN(31,FILE = 'gcm_grid.bin', FORM = 'UNFORMATTED')
READ(31) LC, NX, NY, NZ   !Number of active/wet horizontal grids
PRINT*, LC, NX, NY, NZ

ALLOCATE(IL(LC))
ALLOCATE(JL(LC))
ALLOCATE(LIJ(IC,JC))

OPEN(32, FILE = 'gcm_grid.txt', ACTION= 'WRITE', STATUS = 'UNKNOWN')

DO L = 1, LC
   READ(31) IL(L), JL(L)
   LIJ(IL(L),JL(L)) = L
   WRITE(32,*) IL(L), JL(L), LIJ(IL(L),JL(L))
ENDDO



CLOSE(31)
CLOSE(32)

END
