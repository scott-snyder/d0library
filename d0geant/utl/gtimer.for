      SUBROUTINE GTIMER(MSG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : ROUTINE TO DO SOME TIMING IN GEANT\
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
      CHARACTER*(*) MSG
C----------------------------------------------------------------------
      IF(IENT.EQ.0)THEN
        WRITE(6,122)MSG
  122   FORMAT(' ************ TIMER INITIALIZED ************'/,2X,A)
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
      WRITE(6,123)MSG,NDSTART,NTSTART,NDEND,NTEND,DELT
  123 FORMAT(' TIMER MESSAGE: ',A,/,
     &  ' STARTING DATE(DD/MM/YY),TIME(HH:MM:SS) : ',2A4,2X,2A4,/,
     +       ' ENDING   DATE(DD/MM/YY),TIME(HH:MM:SS) : ',2A4,2X,2A4,/,
     +       ' CPU TIME TAKEN     : ',F12.4,' SECONDS')
      RETURN
C
C ****  ENTRY RGTIMER - Reset GTIMER
C
      ENTRY RGTIMER(MSG)
C
      DO 125 I = 1 , 2
        NDSTART(I) = NDEND(I)
        NTSTART(I) = NTEND(I)
  125 CONTINUE
      TCPU(1) = TCPU(2)
      WRITE(6,124)MSG
  124 FORMAT(' ***** TIMER RESET ********',/,2X,A)
C
  999 RETURN
      END
