      SUBROUTINE GTL2EM_COUNT(CURRENT,NFOUND,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get number of candidates from L2EM bank.  
C-                         For use with GTL2EM_VALUE.
C-
C-   Inputs  : CURRENT  = count only candidates for the current parameter set.
C-
C-   Outputs : NFOUND   = number of candidates for the current parameter set 
C-                        (dependent on CURRENT).
C-             IER      = -1 = no bank for this event
C-                      =  0 = OK 
C-
C-   Controls: No control!
C-
C-   Created  14-JUL-1993   James T. McKinley
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LL2EM,GZL2EM,NFOUND,IER,PS
      LOGICAL CURRENT,BANKEXISTS
C----------------------------------------------------------------------
      NFOUND = 0
      IER = 0
C
      LL2EM = GZL2EM()
      BANKEXISTS = LL2EM.GT.0
C
      IF(.NOT.BANKEXISTS)THEN
        IER = -1
        GOTO 999
      ENDIF
C
      PS = IQ(LL2EM+29)
      NFOUND = 1
C
      DOWHILE(BANKEXISTS)                           ! infinite loop, must break
        LL2EM = LQ(LL2EM)                           ! out via GOTO.
        IF(LL2EM.LE.0) GOTO 999                     
        IF((IQ(LL2EM+29).NE.PS).AND.CURRENT) GOTO 999 
        NFOUND = NFOUND + 1
      ENDDO
C
  999 RETURN
      END
