      FUNCTION CZLINI
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize link area ZLINKC. This is done
C-   once only.
C-
C-   Returned value  : TRUE
C-   Inputs  : None
C-   Outputs : None
C-   Controls: NOne
C-
C-   Created  10-OCT-1989   Harrison B. Prosper
C-   Updated  13-JAN-1994   Sailesh Chopra add ZLINKC_INI flag 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      LOGICAL FIRST,CZLINI
      LOGICAL FLGCHK,FLGVAL
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      CZLINI = .TRUE.
C
      IF( FIRST ) THEN
        FIRST = .FALSE.
C
C ****  Declare Calorimeter Link area in /ZLINKC/
C
        IF (.NOT. FLGCHK('ZLINKC_INI'))THEN
          CALL FLGBK('ZLINKC_INI',1)
        ENDIF
        IF (.NOT. FLGVAL('ZLINKC_INI'))THEN
          CALL MZLINK(IXCOM,'/ZLINKC/',CSTLNK,CRFLNK,CRFLNK(LNKMX))
          CALL FLGSET('ZLINKC_INI',.TRUE.)
        ENDIF
      ENDIF
C
  999 RETURN
      END
