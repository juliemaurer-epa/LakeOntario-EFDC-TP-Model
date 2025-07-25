!------------------------------------------------------------
Program Read_EFDC_Tran
!------------------------------------------------------------
! This program reads hydrodynamic information 
! from a binary file. 

IMPLICIT NONE

INTEGER(kind=4) :: K, L
INTEGER(kind=4), PARAMETER :: IC = 256  ! Number of columns
INTEGER(kind=4), PARAMETER :: JC = 133  ! Number of rows
INTEGER(kind=4), PARAMETER :: LC = 13778  ! Number of surface cells
INTEGER(kind=4), PARAMETER :: KC = 10   ! Maximum number of layers
INTEGER(kind=4), PARAMETER :: KCP1 = 11
INTEGER(kind=4), PARAMETER :: ICOLLOPT = 0
INTEGER(kind=4) :: Reason

REAL :: TMID
REAL, DIMENSION(LC, KC) :: QX, QY, EX, EY
REAL, DIMENSION(LC, KCP1) :: QZ, EZ


REAL, DIMENSION(LC, KC) :: BVOL, VDER, HYDTEMP
REAL, DIMENSION(LC) :: HBAR, DETA, ETA

CHARACTER(LEN = 50) :: FILE_TRANSP = '/work/GLFBREEZ/Lake_Ontario/EFDC_2018/EFDC_tran.bin'

!--------------------------------------------------------------------------------------------------------
! TMID = midpoint time (in days) of linkage interval, relative to zero date/time used in EFDC.
!
! ICOLLOPT = an input variable used to indicate whether the grid and linkage file is 
!            collapsed (i.e., coarsened) relative to the original model grid / linkage file.
!
! QX  flow rate in pseudo-east direction at the cell pseudo-west face (m3/s)
! QY  flow rate in pseudo-north direction at the cell pseudo-south face (m3/s)
! QZ  flow rate in vertical direction at bottom (lower) interface for each layer (m3/s)
!
! Note that the convention used for QZ is that a positive flow rates indicates movement of water 
! from the below layer to the above layer. For the QZ (and EZ) array, an index of 1 refers 
! to the air-water interface and an index of [KC+1] refers to the water-bed interface 
! (an index of 2 refers to interface between layer 1 (surface) and layer 2). Therefore, it is 
! always the case that QZ(1) = QZ(KC+1) = 0.
!
! EX  dispersive exchange rate at the cell pseudo-west face (m2/s)
! EY  dispersive exchange rate at the cell pseudo-south face (m2/s)
! EZ  dispersive exchange rate at the bottom (lower) interface for each layer (m2/s)
!
! For uncollapsed grids only (ICOLLOPT = 0):
! ETA  water level relative to datum at the beginning of linkage interval (in meters).
! DETA  average rate of water level (or depth) change during the linkage interval (m/s).
!
! For collapsed grids only (ICOLLOPT = 1):
! HBAR  mean water depth over linkage interval (in meters).
! BVOL  water volume in cell at beginning of linkage interval (m3)
! VDER  volume derivative over linkage interval (m3/day)
!
! HYDTEMP  water temperatures reported EFDC, averaged over the linkage interval (deg. C)
!
!---------------------------------------------------------------------------------------------------------

OPEN(30,FILE = FILE_TRANSP, FORM = 'UNFORMATTED', CONVERT = 'LITTLE_ENDIAN')
OPEN(31, FILE = 'EFDC_tran_UVflows.txt', ACTION = 'WRITE', STATUS = 'UNKNOWN')
OPEN(32, FILE = 'EFDC_tran_Wflows.txt', ACTION = 'WRITE', STATUS = 'UNKNOWN')
OPEN(33, FILE = 'EFDC_tran_XYdispersion.txt', ACTION = 'WRITE', STATUS = 'UNKNOWN')
OPEN(34, FILE = 'EFDC_tran_Zdispersion.txt', ACTION = 'WRITE', STATUS = 'UNKNOWN')
OPEN(35, FILE = 'EFDC_tran_elevations.txt', ACTION = 'WRITE', STATUS = 'UNKNOWN')
OPEN(36, FILE = 'Temperature.txt', ACTION = 'WRITE', STATUS = 'UNKNOWN')
OPEN(37, FILE = 'Time_intervals.txt', ACTION = 'WRITE', STATUS = 'UNKNOWN')

DO 
! Read the time mid-point of each linkage interval (representing simulation day    
! relative to day zero used in the EFDC simulation:
READ(30,IOSTAT = Reason) TMID

IF (Reason > 0) THEN
    PRINT*, 'Check input. Something went wrong: stopping execution of program.'
    EXIT
ELSE IF (Reason < 0) THEN
     PRINT*, 'End of file has been reached: ending execution of program.'
     EXIT
ELSE
     ! Read x/y/z advection and dispersion rates:
     READ(30) QX
     READ(30) QY
     IF(KC > 1) READ(30) QZ

     READ(30) EX
     READ(30) EY
     IF(KC > 1) READ(30) EZ

     ! Read depth/volume/derivative arrays:

     IF (ICOLLOPT == 0) THEN  !Linkage file based on raw/uncollapsed grid
         READ(30) ETA
         READ(30) DETA
     ELSE                     !Linkage file based on collapsed grid
         READ(30) HBAR
         READ(30) BVOL
         READ(30) VDER
     ENDIF
        
     !Read water temperature simulated by EFDC: 
     READ(30) HYDTEMP

     WRITE(31,*) TMID
     WRITE(32,*) TMID
     WRITE(33,*) TMID
     WRITE(34,*) TMID
     WRITE(35,*) TMID
     WRITE(36,*) TMID
     WRITE(37,*) TMID

     DO L = 1, LC
        DO K = 1, KC
           WRITE(31,*) L, K, QX(L,K), QY(L,K)
        ENDDO
     ENDDO 

     DO L = 1, LC
        DO K = 1, KCP1
           WRITE(32,*) L, K, QZ(L,K)
        ENDDO
     ENDDO

     DO L = 1, LC
        DO K = 1, KC
           WRITE(33,*) L, K, EX(L,K), EY(L,K)
        ENDDO
     ENDDO

     WRITE(32,*) 

     DO L = 1, LC
        DO K = 1, KCP1
           WRITE(34,*) L, K, EZ(L,K)
        ENDDO
     ENDDO

     DO L = 1, LC 
        WRITE(35,*) L, ETA(L), DETA(L)  
     ENDDO

     DO L = 1, LC
        DO K = 1, KC
           WRITE(36,*) L, K, HYDTEMP(L,K)
        ENDDO
     ENDDO

END IF

END DO

CLOSE(30)
CLOSE(31)
CLOSE(32)
CLOSE(33)
CLOSE(34)
CLOSE(35)
CLOSE(36)
CLOSE(37)

END
