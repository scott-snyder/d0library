      LOGICAL FUNCTION TOP_LEPTONS_UTIL_MICRO_BLANK()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check to see if Micro Blank flag is set
C-
C-   Returned value  : 
C-                    .true./.false. = micro_blank is set/not set
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  26-JAN-1993   Stephen J. Wimpenny
C-   Modified 22-Mar-1993   Routine name changed for library compatibility
C-                          ( was Test_Micro_Blank )
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      LOGICAL MICRO_BLANK
      INTEGER GZHEAD,HEAD_FLAG,ITEMP,JBIT,IRET
C
      TOP_LEPTONS_UTIL_MICRO_BLANK=.FALSE.
      HEAD_FLAG=-1
      LHEAD=GZHEAD()
      IF(LHEAD.NE.0) THEN
C
C *** Test word 30 for micro-blank flag
C
        ITEMP=JBIT(IQ(LHEAD+30),1)
        IF(ITEMP.GT.0) HEAD_FLAG=1
      ENDIF
      IF(HEAD_FLAG.EQ.1) GO TO 100
C
C *** Data seems OK but....... beware of old RECO versions !!!
C *** next go to TRGR bank and re-check
C
      IF(MICRO_BLANK(IRET)) THEN
        GO TO 100
      ELSE
        GO TO 999
      ENDIF
C
  100 CONTINUE
      TOP_LEPTONS_UTIL_MICRO_BLANK=.TRUE.
C----------------------------------------------------------------------
  999 RETURN
      END
