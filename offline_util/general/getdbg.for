      LOGICAL FUNCTION GETDBG(IUDBG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : flag for debug printout
C-
C-   Outputs :
C-    IU= unit for debug printout (DEBUG.OUT)
C-
C-    ENTRY SETDBG(FL)
C-   Inputs  : 
C-    FL=.true.  File DEBUG.OUT opened and GETDBG returns as true
C-      =.false.  no file opened and GETDBG returns as false
C-
C-   Created  10-AUG-1987   Serban D. Protopopescu
C-   Updated  11-Mar-1992   Herbert B. Greenlee
C-      UNIX compatible version (use D0OPEN)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IU,IUDBG,ERR
      LOGICAL FL,FLDBG,SETDBG,OK
C
      IUDBG=IU
      GETDBG=FLDBG
      RETURN
C
      ENTRY SETDBG(FL)
      FLDBG=FL
      IF (FL ) THEN
        CALL GTUNIT(11,IU,ERR)
        CALL D0OPEN(IU,'DEBUG.OUT','OFL',OK)
        IF(.NOT.OK)GO TO 11
        CALL INTMSG(' Debug printout is ON')
      ELSE
        IF(IU.NE.0) THEN
          CLOSE(IU)
          CALL RLUNIT(1,IU,ERR)
        ENDIF
        CALL INTMSG(' Debug printout is OFF')
      ENDIF
      GOTO 999
   11 CALL INTMSG(' Cannot open DEBUG.OUT')      
C----------------------------------------------------------------------
  999 RETURN
      END
