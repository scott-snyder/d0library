      SUBROUTINE PFGET(OUTSTR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get current PF-key labels from bottom line of 
C-                         display
C-
C-   Inputs  : None
C-   Outputs : OUTSTR: Array of PF-key labels
C-   Controls: None
C-
C-   Documented 22-SEP-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) OUTSTR(4)
      CHARACTER*64 OUTTXT
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER ISTAT,LINGET,I,K
C----------------------------------------------------------------------
      ISTAT=LINGET(PBROWS,OUTTXT)
      IF(MOD(ISTAT,2).NE.0) THEN
        DO I=1,4
          OUTSTR(I)=' '
          K=0
          DO WHILE (OUTTXT(1:1).EQ.' '.AND.K.LT.12)
            OUTTXT=OUTTXT(2:)
            K=K+1
          ENDDO
          IF(K.LE.7) THEN
            K=INDEX(OUTTXT,' ')
            OUTSTR(I)=OUTTXT(1:K-1)
            OUTTXT=OUTTXT(K+1:)
          ENDIF
        ENDDO
      ENDIF
      RETURN
      END
