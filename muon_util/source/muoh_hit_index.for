      SUBROUTINE MUOH_HIT_INDEX(ITHMOD,IHIT,IMUOH,NMUOH,OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : This routine returns hit index into MUOH. 
C-
C-   Inputs  : ITHMOD, the pointer to MUOF.
C-             IHIT, the IHITth MUOH for the PDT.
C-                   
C-   Outputs : IMUOH is suitable for inputs into GTMUOH.
C-             NMUOH tells you how many MUOH there are for the PDT.
C-             OK is a logical informing whether it worked.
C-   Controls: None.
C-
C-   Created  22-APR-1993   Tom Diehl
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      INCLUDE 'D0$INC:ZEBCOM.INC'  

      INTEGER ITHMOD,IHIT,IMUOH,NMUOH
      LOGICAL OK

      INTEGER LMUOH,GZMUOH,LMUHT,GZMUHT        
      INTEGER NMURAW,NMUSRT,NMODH,LPMUOF(310),NWD,L,MUNMOD2
      INTEGER NMOD,NRAW,JMUD1,JMUOH,NHPL0,NHPL1,NHPL2,NHPL3   ! GTMUOH

      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C-
C- Here I am checking the number of MUOH words per hit.
C- I think that MUOH was once a different size.
C-
      IF(FIRST) THEN   
        LMUOH = GZMUOH(0)
        LMUHT = GZMUHT(0)
        CALL GTMUHT(NMURAW,NMUSRT,NMODH,LPMUOF)
        IF(LMUHT.NE.0.AND.NMUSRT.NE.0.AND.LMUOH.NE.0) THEN
          NWD=28
          L=IQ(LMUOH-1)/NMUSRT
          IF(L.EQ.25) NWD=25
          FIRST=.FALSE.
        ENDIF
      ENDIF
C-
C- Event setup.
C-
      OK = .TRUE.
      
      LMUOH = GZMUOH(0)
      IF(LMUOH .EQ. 0) THEN 
        OK = .FALSE.
        GOTO 999
      ENDIF
C-
C- Make up index for MUOH for the hit.
C-
      
      CALL GTMUOF(ITHMOD,NMOD,NRAW,JMUD1,NMUOH,JMUOH,NHPL0,
     $ NHPL1,NHPL2,NHPL3)
      IMUOH = ((JMUOH-1)/NWD) + IHIT

  999 RETURN
      END
