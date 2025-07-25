PROGRAM INITIAL_CONDITIONS

USE NETCDF

IMPLICIT NONE

CHARACTER(LEN=40) :: TP_FILE  = 'Initial_Conditions_2018x1_25.nc'

INTEGER(kind=4) :: I, J, K, ISURF, IX, JY, N, NSCELLS, NZ
INTEGER(kind=4), PARAMETER :: IC = 256  ! Number of columns
INTEGER(kind=4), PARAMETER :: JC = 133  ! Number of rows
INTEGER(kind=4), PARAMETER :: LC = 13778  ! Number of surface cells
INTEGER(kind=4), PARAMETER :: KC = 10   ! Maximum number of vertical layers
INTEGER(kind=4), PARAMETER :: KCP1 = 11

INTEGER(kind=4) :: STATUS, ISTATUS
INTEGER(kind=4) :: NCID_TP, TP_ID
INTEGER(kind=4) :: X_ID, Y_ID, Z_ID
INTEGER(kind=4) :: XVar_ID, YVar_ID, ZVar_ID
INTEGER(kind=4), DIMENSION(IC) :: IC_ARR
INTEGER(kind=4), DIMENSION(JC) :: JC_ARR
INTEGER(kind=4), DIMENSION(KC) :: KC_ARR
INTEGER(kind=4), DIMENSION(LC) :: IX_ARR
INTEGER(kind=4), DIMENSION(LC) :: JY_ARR
INTEGER(kind=4), DIMENSION(IC, JC) :: NZ_ARR

CHARACTER(LEN=40) :: IC_FILE

REAL :: TIME_SECONDS = 0
REAL :: CONC
REAL, DIMENSION(IC, JC, KC) :: TP

DO I = 1, IC
   IC_ARR(I) = I
ENDDO

DO I = 1, JC
   JC_ARR(I) = I
ENDDO

DO I = 1, KC
   KC_ARR(I) = I
ENDDO

NSCELLS = IC * JC

OPEN(UNIT = 10, FILE = 'Initial_Conditions_TP_2018.txt', ACTION = 'READ')
OPEN(UNIT = 20, FILE = 'nz_IJ.dat', ACTION = 'READ')

TP = 0.0
DO I = 1, LC
   READ(10,*) N, IX, JY, CONC
   IX_ARR(I) = IX
   JY_ARR(I) = JY
   TP(IX,JY,1) = CONC * 1.0E-06 * 1.25
ENDDO

NZ_ARR = 0
DO I = 1, NSCELLS
   READ(20,*) IX, JY, NZ
   NZ_ARR(IX,JY) = NZ
ENDDO

CLOSE(UNIT = 10)
CLOSE(UNIT = 20)

DO I = 1, LC
   IX = IX_ARR(I)
   JY = JY_ARR(I)
   NZ = NZ_ARR(IX,JY)
   DO K = 1, NZ
      TP(IX,JY,K) = TP(IX,JY,1)
   ENDDO
ENDDO

!-----------------------------------------------------------------------------
!   Create NetCDF file
!-----------------------------------------------------------------------------
STATUS = NF90_CREATE(PATH = TP_FILE, CMODE = NF90_CLOBBER, NCID = NCID_TP)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

!-----------------------------------------------------------------------------
!   Define dimensions
!-----------------------------------------------------------------------------
STATUS = NF90_DEF_DIM(NCID_TP, "X", IC, X_ID)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_DEF_DIM(NCID_TP, "Y", JC, Y_ID)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_DEF_DIM(NCID_TP, "Z", KC, Z_ID)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

!-----------------------------------------------------------------------------
!   Define variables
!-----------------------------------------------------------------------------
STATUS = NF90_DEF_VAR(NCID_TP, "X", NF90_INT, (/ X_ID /), XVar_ID)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_DEF_VAR(NCID_TP, "Y", NF90_INT, (/ Y_ID /), YVar_ID)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_DEF_VAR(NCID_TP, "Z", NF90_INT, (/ Z_ID /), ZVar_ID)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_DEF_VAR(NCID_TP, "TP", NF90_FLOAT, (/ X_ID, Y_ID, Z_ID /), TP_ID)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

!------------------------------------------------------------------------------
!   Variable Attributes
!------------------------------------------------------------------------------
STATUS = NF90_PUT_ATT(NCID_TP, XVar_ID, "long_name", "X")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID_TP, XVar_ID, "units", "None")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID_TP, YVar_ID, "long_name", "Y")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID_TP, YVar_ID, "units", "None")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID_TP, ZVar_ID, "long_name", "Z")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID_TP, ZVar_ID, "units", "None")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID_TP, TP_ID, "long_name", "TP initial concentrations")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_ATT(NCID_TP, TP_ID, "units", "kg/m3")
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

!------------------------------------------------------------------------------
!   End definitions
!------------------------------------------------------------------------------
STATUS = NF90_ENDDEF(NCID_TP)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

!------------------------------------------------------------------------------
!   Assign values
!------------------------------------------------------------------------------
STATUS = NF90_PUT_VAR(NCID_TP, XVar_ID, IC_ARR)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_VAR(NCID_TP, YVar_ID, JC_ARR)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

STATUS = NF90_PUT_VAR(NCID_TP, ZVar_ID, KC_ARR)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)
   
STATUS = NF90_PUT_VAR(NCID_TP, TP_ID, TP, START = (/ 1, 1, 1 /), COUNT = (/ IC, JC, KC /))
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)


STATUS = NF90_CLOSE(NCID_TP)
IF (STATUS /= NF90_NOERR) CALL HANDLE_ERR(STATUS)

END PROGRAM
