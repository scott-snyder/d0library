      SUBROUTINE TIMER
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : ROUTINE TO DO SOME TIMING IN CALIB
C-
C-   Inputs  : NONE
C-   Outputs : NONE
C-
C-   Created  17-OCT-1987   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:TIMES.INC'
      INTEGER IENT
      REAL DELT
      DATA IENT/0/
      INTEGER ICUMUL,I
      DATA ICUMUL/0/                    ! IF NON ZERO NO RESET
C----------------------------------------------------------------------
      IF(IENT.EQ.0)THEN
        WRITE(6,122)
  122   FORMAT(' ************ TIMER INITIALIZED ************')
        CALL TIMEST(TLIM)
        CALL TIMEX(TCPU(1))      !STARTING CPU
        CALL DATIMH(NDSTART,NTSTART)  !STARTING DATE, TIME IN INTEGER
        IENT=1
        RETURN
      ENDIF
C
      CALL DATIMH(NDEND,NTEND)
      CALL TIMEX(TCPU(2))
      DELT=TCPU(2)-TCPU(1)
      WRITE(6,123)NDSTART,NTSTART,NDEND,NTEND,DELT
  123 FORMAT(' STARTING DATE(DD/MM/YY),TIME(HH:MM:SS) : ',2A4,2X,2A4,/,
     +       ' ENDING   DATE(DD/MM/YY),TIME(HH:MM:SS) : ',2A4,2X,2A4,/,
     +       ' CPU TIME TAKEN     : ',F12.4,' SECONDS')
      IF(ICUMUL.EQ.0)THEN
        DO 125 I = 1 , 2 
        NDSTART(I) = NDEND(I)
        NTSTART(I) = NTEND(I)
  125   CONTINUE
        TCPU(1) = TCPU(2)
        WRITE(6,124)
  124   FORMAT(' ***** TIMER RESET ********')
      ENDIF
  999 RETURN
      END
