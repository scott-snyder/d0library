      LOGICAL FUNCTION TOP_LEPTONS_UTIL_MRBS_LOSS()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check to see if MRBS loss flag is set
C-
C-   Returned value  : 
C-                    .true./.false. = MRBS flag is set/not set
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  26-JAN-1993   Stephen J. Wimpenny
C-   Modified 22-Mar-1993   Routine name changed for library compatibility
C-                          ( was Test_MRBS )
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      LOGICAL MRBS_LOSS
      INTEGER IRET
C
      TOP_LEPTONS_UTIL_MRBS_LOSS=.FALSE.
C
C *** go to TRGR bank and test for MRBS veto
C
      IF(MRBS_LOSS(IRET)) THEN
        GO TO 100
      ELSE
        GO to 999
      ENDIF
C
  100 CONTINUE
      TOP_LEPTONS_UTIL_MRBS_LOSS=.TRUE.
C----------------------------------------------------------------------
  999 RETURN
      END
