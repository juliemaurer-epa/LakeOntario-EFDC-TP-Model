SUBROUTINE HANDLE_ERR(STATUS)
!------------------------------------------------------------------------------
!-
!-   $Id: $
!-
!-   Description :  This program a subroutine that handles errors messages
!-                  using the function NF90_STRERROR.
!-
!-   Calls: NONE     
!-   Called by: HYDRO_INTERFACE
!-
!-   Created: 06/10/08  W. Melendez
!-
!- Revised:
!-
!------------------------------------------------------------------------------

USE NETCDF

IMPLICIT NONE

INTEGER, INTENT(IN) :: STATUS

IF (STATUS /= NF90_NOERR) THEN
    PRINT*, TRIM(NF90_STRERROR(STATUS))
    STOP "Stopped"
ENDIF

END SUBROUTINE HANDLE_ERR
