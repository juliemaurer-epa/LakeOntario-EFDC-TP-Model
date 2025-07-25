PROGRAM EFDC_TRANSPORT

USE NETCDF

IMPLICIT NONE

CHARACTER(LEN=30), DIMENSION(11) :: VAR_FILE
CHARACTER(LEN=20), DIMENSION(11) :: VAR_NAME
CHARACTER(LEN=30), DIMENSION(11) :: VAR_LONGNAME
CHARACTER(LEN=10), DIMENSION(11) :: VAR_UNITS

INTEGER(kind=4) :: K, L, I, LSC
INTEGER(kind=4), PARAMETER :: IC = 256  ! Number of columns
INTEGER(kind=4), PARAMETER :: JC = 133  ! Number of rows
INTEGER(kind=4), PARAMETER :: LC = 13778
INTEGER(kind=4), PARAMETER :: KC = 10   ! Maximum number of layers
INTEGER(kind=4), PARAMETER :: KCP1 = 11
INTEGER(kind=4), PARAMETER :: ICOLLOPT = 0
INTEGER(kind=4), DIMENSION(256) :: IC_ARR
INTEGER(kind=4), DIMENSION(133) :: JC_ARR
INTEGER(kind=4), DIMENSION(10) :: KC_ARR
INTEGER(kind=4) :: Reason, STATUS, ISTATUS

INTEGER(kind=4), DIMENSION(11) :: NCIDN
INTEGER(kind=4), DIMENSION(11) :: X_ID, Y_ID, Z_ID, TIME_ID, TimeVar_ID
INTEGER(kind=4), DIMENSION(11) :: XVar_ID, YVar_ID, ZVar_ID
INTEGER(kind=4), DIMENSION(11) :: VAR_ID

INTEGER(kind=4) :: REC_NUM = 0
INTEGER(kind=4) :: IYEAR = 2018, IMONTH = 4, IDAY = 1

REAL :: TIME_INIT = 1522540800
REAL :: TIME_SECONDS = 0

REAL :: TMID
REAL, DIMENSION(LC, KC) :: QX, QY, EX, EY
REAL, DIMENSION(IC, JC, KC) :: FLOWX, FLOWY
REAL, DIMENSION(IC, JC, KC) :: UDisp, VDisp
REAL, DIMENSION(IC, JC, KC) :: Layer_Depth
REAL, DIMENSION(LC, KCP1) :: QZ, EZ
REAL, DIMENSION(IC, JC, KC) :: FLOWZ, WDisp

REAL, DIMENSION(LC, KC) :: BVOL, VDER, HYDTEMP
REAL, DIMENSION(IC, JC, KC) :: TEMP
REAL, DIMENSION(LC) :: HBAR, DETA, ETA
REAL, DIMENSION(IC, JC) :: ELEV
REAL, DIMENSION(IC, JC) :: Water_Depth

REAL, DIMENSION(IC, JC, KC) :: SALT

REAL, DIMENSION(IC, JC) :: FSM, H, DX, DY
REAL, DIMENSION(KCP1) :: DZ, DZZ
REAL, DIMENSION(LC) :: Deltaz

REAL, PARAMETER :: fillValue = 0.0
REAL, PARAMETER :: missValue = -9999.0

INTEGER, DIMENSION(LC) :: ZGVC, ZGVCU, ZGVCV
LOGICAL, DIMENSION(LC, KC) :: LGVC, LGVCU, LGVCV, LGVCW

INTEGER(kind=4), DIMENSION(:), ALLOCATABLE :: IL, JL
INTEGER(kind=4), DIMENSION(:,:), ALLOCATABLE :: LIJ

! Set arrays to the fill or missing value
FLOWX = fillValue
FLOWY = fillValue
FLOWZ = fillValue
UDisp = fillValue
VDisp = fillValue
WDisp = fillValue
TEMP = missValue
ELEV = missValue
SALT = fillValue
Layer_Depth = missValue
Water_Depth = missValue

DO I = 1, IC
   IC_ARR(I) = I
ENDDO

DO I = 1, JC
   JC_ARR(I) = I
ENDDO

DO I = 1, KC
   KC_ARR(I) = I
ENDDO

VAR_FILE(1) = 'UFlow.nc'
VAR_FILE(2) = 'VFlow.nc'
VAR_FILE(3) = 'WFlow.nc'
VAR_FILE(4) = 'UDisp.nc'
VAR_FILE(5) = 'VDisp.nc'
VAR_FILE(6) = 'Ev.nc'
VAR_FILE(7) = 'Temp.nc'
VAR_FILE(8) = 'Salt.nc'
VAR_FILE(9) = 'LayerDepth.nc'
VAR_FILE(10) = 'SurfaceElev.nc'
VAR_FILE(11) = 'WaterDepth.nc'

VAR_NAME(1) = 'UFlow'
VAR_NAME(2) = 'VFlow'
VAR_NAME(3) = 'WFlow'
VAR_NAME(4) = 'UDisP'
VAR_NAME(5) = 'VDisp'
VAR_NAME(6) = 'Ev'
VAR_NAME(7) = 'Temp'
VAR_NAME(8) = 'Salt'
VAR_NAME(9) = 'LayerDepth'
VAR_NAME(10) = 'SurfaceElev'
VAR_NAME(11) = 'WaterDepth'

VAR_LONGNAME(1) = 'U FlowS'
VAR_LONGNAME(2) = 'V FlowS'
VAR_LONGNAME(3) = 'W FlowS'
VAR_LONGNAME(4) = 'U Dispersion'
VAR_LONGNAME(5) = 'V Dispersion'
VAR_LONGNAME(6) = 'Vertical Mixing Coefficients'
VAR_LONGNAME(7) = 'Water Temperature'
VAR_LONGNAME(8) = 'Salinity'
VAR_LONGNAME(9) = 'Layer Depth'
VAR_LONGNAME(10) = 'Surface Elevation'
VAR_LONGNAME(11) = 'Water Depth'

VAR_UNITS(1) = 'm3/s'
VAR_UNITS(2) = 'm3/s'
VAR_UNITS(3) = 'm3/s'
VAR_UNITS(4) = 'm2/s'
VAR_UNITS(5) = 'm2/s'
VAR_UNITS(6) = 'm2/s'
VAR_UNITS(7) = 'Celsius'
VAR_UNITS(8) = 'PSU'
VAR_UNITS(9) = 'm'
VAR_UNITS(10) = 'm'
VAR_UNITS(11) = 'm'


!-----------------------------------------------------------------------------
!   Create NetCDF files
!-----------------------------------------------------------------------------
DO I = 1, 11
   STATUS = NF90_CREATE(PATH = VAR_FILE(I), CMODE = NF90_CLOBBER, NCID = NCIDN(I))
   IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)
ENDDO

!-----------------------------------------------------------------------------
!   Define dimensions
!-----------------------------------------------------------------------------
DO I = 1, 11
   STATUS = NF90_DEF_DIM(NCIDN(I), "X", IC, X_ID(I))
   IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)
ENDDO

DO I = 1, 11
   STATUS = NF90_DEF_DIM(NCIDN(I), "Y", JC, Y_ID(I))
   IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)
ENDDO

DO I = 1, 9
   STATUS = NF90_DEF_DIM(NCIDN(I), "Z", KC, Z_ID(I))
   IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)
ENDDO

DO I = 1, 11
   STATUS = NF90_DEF_DIM(NCIDN(I), "Time", NF90_UNLIMITED, TIME_ID(I))
   IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)
ENDDO

!-----------------------------------------------------------------------------
!   Define variables
!-----------------------------------------------------------------------------
DO I = 1, 11
   STATUS = NF90_DEF_VAR(NCIDN(I), "X", NF90_INT, (/ X_ID(I) /), XVar_ID(I))
   IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)
ENDDO

DO I = 1, 11
   STATUS = NF90_DEF_VAR(NCIDN(I), "Y", NF90_INT, (/ Y_ID(I) /), YVar_ID(I))
   IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)
ENDDO

DO I = 1, 9
   STATUS = NF90_DEF_VAR(NCIDN(I), "Z", NF90_INT, (/ Z_ID(I) /), ZVar_ID(I))
   IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)
ENDDO

DO I = 1, 11
   STATUS = NF90_DEF_VAR(NCIDN(I), "Time", NF90_DOUBLE, (/ TIME_ID(I) /), TimeVar_ID(I))
   IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)
ENDDO

DO I = 1, 9
   STATUS = NF90_DEF_VAR(NCIDN(I), VAR_NAME(I), NF90_FLOAT, (/ X_ID(I), Y_ID(I), Z_ID(I), TIME_ID(I) /), VAR_ID(I))
   IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)
ENDDO

DO I = 10, 11
   STATUS = NF90_DEF_VAR(NCIDN(I), VAR_NAME(I), NF90_FLOAT, (/ X_ID(I), Y_ID(I), TIME_ID(I) /), VAR_ID(I))
   IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)
ENDDO


!------------------------------------------------------------------------------
!   Variable Attributes
!------------------------------------------------------------------------------
DO I = 1, 11

   STATUS = NF90_PUT_ATT(NCIDN(I),TimeVar_ID(I),"Long_Name", "Simulation time")
   IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

   STATUS = NF90_PUT_ATT(NCIDN(I),TimeVar_ID(I),"Units", "Seconds since 01-01-1970.")
   IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

   STATUS = NF90_PUT_ATT(NCIDN(I),XVar_ID(I),"Long_Name", "X")
   IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

   STATUS = NF90_PUT_ATT(NCIDN(I),XVar_ID(I),"Units", "None")
   IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

   STATUS = NF90_PUT_ATT(NCIDN(I),YVar_ID(I),"Long_Name", "Y")
   IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

   STATUS = NF90_PUT_ATT(NCIDN(I),YVar_ID(I),"Units", "None")
   IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)
   
   IF (I <= 9) THEN
       STATUS = NF90_PUT_ATT(NCIDN(I),ZVar_ID(I),"Long_Name", "Z")
       IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

       STATUS = NF90_PUT_ATT(NCIDN(I),ZVar_ID(I),"Units", "None")
       IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)
   ENDIF

   STATUS = NF90_PUT_ATT(NCIDN(I), VAR_ID(I), "Long_Name", VAR_LONGNAME(I))
   IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

   STATUS = NF90_PUT_ATT(NCIDN(I), VAR_ID(I), "Units", VAR_UNITS(I))
   IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

   STATUS = NF90_PUT_ATT(NCIDN(I), VAR_ID(I), "_FillValue", missValue)
   IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)
   
ENDDO

!------------------------------------------------------------------------------

PRINT*, 'Finished creation of attributes.'

DO I = 1, 11
   STATUS = NF90_ENDDEF(NCIDN(I))
   IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)
ENDDO

!------------------------------------------------------------------------------
!   Assign values
!------------------------------------------------------------------------------
DO I = 1, 11

   STATUS = NF90_PUT_VAR(NCIDN(I), XVar_ID(I), IC_ARR)
   IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

   STATUS = NF90_PUT_VAR(NCIDN(I), YVar_ID(I), JC_ARR)
   IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

   IF (I <= 9) THEN
       STATUS = NF90_PUT_VAR(NCIDN(I), ZVar_ID(I), KC_ARR)
       IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)
   ENDIF

ENDDO

PRINT*, 'Finished assignment of values.'

OPEN(31,FILE = 'gcm_grid.bin', FORM = 'UNFORMATTED')
READ(31) LSC   !Number of active/wet horizontal grid cells

ALLOCATE(IL(LSC))
ALLOCATE(JL(LSC))
ALLOCATE(LIJ(IC,JC))

DO L = 1, LSC
   READ(31) IL(L), JL(L)
   LIJ(IL(L),JL(L)) = L
ENDDO

CLOSE(31)

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
OPEN(32, FILE = 'gcm_geom.bin', FORM = 'UNFORMATTED', CONVERT = 'LITTLE_ENDIAN')
READ(32) DZ, DZZ
READ(32) H, DX, DY, FSM
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

DO I = 1, LC
   Deltaz(I) = (KC / ZGVC(I)) * DZ(1)
ENDDO

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

OPEN(30, FILE = 'EFDC_tran.bin', FORM = 'UNFORMATTED', CONVERT = 'LITTLE_ENDIAN')

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
     REC_NUM = REC_NUM + 1

     TIME_SECONDS = TIME_INIT + TMID * 86400
     DO I = 1, 11
        STATUS = NF90_PUT_VAR(NCIDN(I), TimeVar_ID(I), TIME_SECONDS, START = (/ REC_NUM /))
        IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)
     ENDDO

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
        
     ! Read water temperature simulated by EFDC: 
     READ(30) HYDTEMP

     DO L = 1, LC
        FLOWX(IL(L),JL(L),:) = QX(L,:)
        FLOWY(IL(L),JL(L),:) = QY(L,:)
     ENDDO 
     
     STATUS = NF90_PUT_VAR(NCIDN(1), VAR_ID(1), FLOWX, START = (/ 1, 1, 1, REC_NUM /), COUNT = (/ IC, JC, KC, 1 /))
     IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

     STATUS = NF90_PUT_VAR(NCIDN(2), VAR_ID(2), FLOWY, START = (/ 1, 1, 1, REC_NUM /), COUNT = (/ IC, JC, KC, 1 /))
     IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

     DO L = 1, LC
        FLOWZ(IL(L),JL(L),:) = QZ(L,1:KC)
     ENDDO

     STATUS = NF90_PUT_VAR(NCIDN(3), VAR_ID(3), FLOWZ, START = (/ 1, 1, 1, REC_NUM /), COUNT = (/ IC, JC, KC, 1 /))
     IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

     DO L = 1, LC
        UDisp(IL(L),JL(L),:) = EX(L,:) 
        VDisp(IL(L),JL(L),:) = EY(L,:)       
     ENDDO

     STATUS = NF90_PUT_VAR(NCIDN(4), VAR_ID(4), UDisp, START = (/ 1, 1, 1, REC_NUM /), COUNT = (/ IC, JC, KC, 1 /))
     IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

     STATUS = NF90_PUT_VAR(NCIDN(5), VAR_ID(5), VDisp, START = (/ 1, 1, 1, REC_NUM /), COUNT = (/ IC, JC, KC, 1 /))
     IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

     DO L = 1, LC
        WDisp(IL(L),JL(L),:) = EZ(L,1:KC) 
     ENDDO

     STATUS = NF90_PUT_VAR(NCIDN(6), VAR_ID(6), WDisp, START = (/ 1, 1, 1, REC_NUM /), COUNT = (/ IC, JC, KC, 1 /))
     IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)
     
     DO L = 1, LC
        ELEV(IL(L),JL(L)) = ETA(L)
        Water_Depth(IL(L),JL(L)) = H(IL(L),JL(L)) + ELEV(IL(L),JL(L)) 
        Layer_Depth(IL(L),JL(L),1:ZGVC(L)) = Deltaz(L) * Water_Depth(IL(L),JL(L))
     ENDDO

     STATUS = NF90_PUT_VAR(NCIDN(10), VAR_ID(10), ELEV, START = (/ 1, 1, REC_NUM /), COUNT = (/ IC, JC, 1 /))
     IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

     STATUS = NF90_PUT_VAR(NCIDN(11), VAR_ID(11), Water_Depth, START = (/ 1, 1, REC_NUM /), COUNT = (/ IC, JC, 1 /))
     IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

     STATUS = NF90_PUT_VAR(NCIDN(9), VAR_ID(9), Layer_Depth, START = (/ 1, 1, 1, REC_NUM /), COUNT = (/ IC, JC, KC, 1 /))
     IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

     DO L = 1, LC
        TEMP(IL(L),JL(L),:) = HYDTEMP(L,:)
     ENDDO

     STATUS = NF90_PUT_VAR(NCIDN(7), VAR_ID(7), TEMP, START = (/ 1, 1, 1, REC_NUM /), COUNT = (/ IC, JC, KC, 1 /))
     IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

     DO L = 1, LC
        SALT(IL(L),JL(L),:) = 0.0
     ENDDO

     STATUS = NF90_PUT_VAR(NCIDN(8), VAR_ID(8), SALT, START = (/ 1, 1, 1, REC_NUM /), COUNT = (/ IC, JC, KC, 1 /))
     IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

END IF

END DO

CLOSE(30)

DO I = 1, 11

   STATUS = NF90_CLOSE(NCIDN(I))
   IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

ENDDO


END
