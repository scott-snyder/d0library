      SUBROUTINE L1_MUON_FILL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill the TRGR bank with the level 1.5 muon
C-                         trigger data
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created   9-NOV-1991   Kamel A. Bazizi
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER NWORD
      PARAMETER (NWORD=500)
      INTEGER LTRGR, GZTRGR
      INTEGER SUBSYSTEM_OFFSET
      INTEGER IWORD, DELTA_WORDS, TRGR_MUON_DATA(NWORD)
C-
C- Get the data calculated by MUON_SUPERVISOR
C-  
      CALL MU_TRIG_DATA(DELTA_WORDS,TRGR_MUON_DATA)
C-
C- if number of words is non zero then fill TRGR bank
C
      IF(DELTA_WORDS .GT. 0) THEN
C
C- get TRGR bank address
C
        LTRGR = GZTRGR()
        CALL L1UTIL_TRGR_EXTENSION(LTRGR, DELTA_WORDS, SUBSYSTEM_OFFSET)
C- fill TRGR bank
        DO IWORD=1,DELTA_WORDS
          IQ(LTRGR+SUBSYSTEM_OFFSET+IWORD)=TRGR_MUON_DATA(IWORD)
        ENDDO
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
