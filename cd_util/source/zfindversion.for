      FUNCTION ZFINDVERSION (T,LCAL,LINE)
C----------------------------------------------------------------------
C-
C-   Purpose and methods : finds the valid version at the time T
C-                         according to a calendar pointed to by LCAL
C-
C-   Returned value : ZFINDVERSION (integer) valid version number 
C-   Inputs         : T (integer(2)) time in vax 64 bits standard format
C-                    LCAL (integer) link to the first bank of the calendar
C-                    LINE (integer) word of the calendar bank containing
C-                                   the version number 
C-   Outputs        : T    unchanged
C-                    LCAL link to the valid calendar bank
C-                    LINE unchanged   
C-   Controls       : none
C-
C-   Created  17-jul-1991   Alain PLUQUET (FINDVERSION)
C-   Updated   9-SEP-1992   Robert E. Avery  Change name to ZFINDVERSION,
C-                              for move CD_UTIL. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER ZFINDVERSION,LCAL,LINE,T(2),T_MIN(2),T_MAX(2),ZVAXTIMECOMP
      LOGICAL LAST,FOUND
C-------------------------------------------------------------------------------
C     INITIALIZATION
C-------------------------------------------------------------------------------
      LAST=.FALSE.
      FOUND=.FALSE.
      ZFINDVERSION=IC(LCAL+LINE)         ! DEFAULT = VERSION IN THE FIRST
                                        ! CALENDAR BANK, I.E. IN THE MOST
                                        ! RECENT ONE
C-------------------------------------------------------------------------------
C     SEARCH FOR THE VALID CALENDAR BANK 
C-------------------------------------------------------------------------------
      DO WHILE (.NOT.(LAST.OR.FOUND))
        T_MIN(1)=IC(LCAL+1)                     ! LOWEST VALID TIME
        T_MIN(2)=IC(LCAL+2)                     ! (STANDARD VAX 64 BITS FORMAT)
        T_MAX(1)=IC(LCAL+3)                     ! HIGHEST VALID TIME
        T_MAX(2)=IC(LCAL+4)                     ! (STANDARD VAX 64 BITS FORMAT)
        FOUND=(ZVAXTIMECOMP(T_MIN,T).GE.0)       ! DOUBLE COMPARISON 
     +   .AND.(ZVAXTIMECOMP(T,T_MAX).GT.0)
        IF (FOUND) ZFINDVERSION=IC(LCAL+LINE)          
        LCAL=LC(LCAL)                          
        LAST=LCAL.LE.0                         
      END DO
      RETURN
      END
