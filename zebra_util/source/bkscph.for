      SUBROUTINE BKSCPH (LSCPH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create and fill the bank SCPH. Designed for
C-                         offline use. Replaces ZEB_SCPH.FOR. This
C-                         routine is called by CONSTP, which in turn
C-                         is called by INIZEB. The parameter RUN is 
C-                         initialized to zero.
C-
C-   Inputs : None
C-   Outputs: 
C-            LSCPH        Bank address
C-                          0 --> SCPH bank not created
C-                         -1 --> SCPH bank not created because
C-                                bank STPH does not exist
C-
C-   Created  17-JUN-1988   Harrison B. Prosper
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER       RUN,I,J,K,JDATE,JTIME,LSCPH
      CHARACTER*8   STIME
      PARAMETER( RUN = 0 )
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSCPH.LINK'
C----------------------------------------------------------------------
C
C ****  Check if STPH bank exists
C
      IF ( LSTPH .EQ. 0 ) THEN
        LSCPH = -1
        GOTO 999
      ENDIF
C
C ****  Build Static Constants Parameter Header bank SCPH
C
      CALL MZBOOK(IDVSTP,LSCPH,LSTPH,-IZSCPH,'SCPH',7,7,10,2,0)
      LSCPH = LC(LSTPH-IZSCPH)
C
C ****  Get date and time and pack into head bank SCPH
C
      CALL IDATE (I,J,K)
      CALL TIME (STIME)
      JDATE = K + J*100 + I*10000
      READ (UNIT=STIME,FMT='(I2,2(1X,I2))') I,J,K
      JTIME = K + J*100 + I*10000
C
      IC(LSCPH+4) = RUN
      IC(LSCPH+5) = 9999
      IC(LSCPH+6) = RUN
      IC(LSCPH+7) = JDATE
      IC(LSCPH+8) = JTIME
  999 RETURN
      END      
