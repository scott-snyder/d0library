      SUBROUTINE DBL3INIT_RSRVR(DET,CHOPT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Initialize parameters for each detector
C-
C-   Inputs  : DET     Detector name (CAL, MUO, ...)
C-             CHOPT   Charcter option;
C-                        ' '   non reading-server mode (defalult)
C-                        'A'   non reading-server mode (Set all at once)
C-                        'C'   Client for reading server
C-                        'S'   Reading server itself
C-                        'Z'   Force reinitialize with CHOPT(2:2) option
C-                              (i.e 'ZC' means reinitialize with 'C' option,
C-                               if Z is not present no reinit will be made.)
C-
C-   Outputs :
C-   Controls:
C-
C-   Created  27-JAN-1994   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) CHOPT
      CHARACTER*(*) DET
      INCLUDE 'D0$INC:DBSTP.INC'
      INTEGER I,ID,LENC,TRULEN,NI,J
      CHARACTER*3  CHROP,CHOP,DTCTR(NDET),DET1
      LOGICAL FIRST(NDET)
      CHARACTER*80 MSG
      DATA FIRST /NDET * .TRUE./
      DATA DTCTR /'CAL','MUO','SAM','CDC','FDC','VTX','TRD','LV0'/
C
      CHROP = ' '
      DET1 = ' '
      CHOP = ' '
      CHROP = CHOPT
      DET1 = DET
      LENC = TRULEN(CHOPT)
      CALL UPCASE(CHROP,CHROP)
      CALL UPCASE(DET1,DET1)
      IF(CHROP(1:1) .EQ. 'Z') THEN
        CHOP(2:2) = 'Z'
        IF(LENC .GT. 1) CHOP(1:1) = CHROP(2:2)
      ELSE
        CHOP = CHROP
      ENDIF
C
      NI = 0
      ID = 0
      IF(CHOP(1:1) .NE. 'A') THEN
        NI = 1
        DO I=1,NDET
          IF(DET1(1:3) .EQ. DTCTR(I)(1:3)) ID = I
        ENDDO
        IF(ID .EQ. 0) THEN
          WRITE(MSG,10)
   10     FORMAT
     &      (' DBL3INIT_RSRVR: Error in detector name.
     &      Initialization did not complete.')
          CALL INTMSG(MSG)
          GOTO 999
        ENDIF
      ELSE
        NI = NDET
      ENDIF
C
      J = ID
      DO I=1,NI
        IF(NI .GT. 1) J = I
        IF(FIRST(J) .OR. CHOP(2:2) .EQ. 'Z') THEN
          FIRST(J) = .FALSE.
          TOPN = 'D0STP'
          LFORCE = .FALSE.
          LVSN = .FALSE.
C
          IF(CHOP(1:1) .EQ. 'S') THEN
            RSERVER = .TRUE.
            RCSERVER = .FALSE.
          ELSEIF(CHOP(1:1) .EQ. 'C') THEN
            RSERVER = .FALSE.
            RCSERVER = .TRUE.
          ENDIF
        ENDIF
      ENDDO
C----------------------------------------------------------------------
  999 RETURN
      END
