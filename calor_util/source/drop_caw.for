      SUBROUTINE DROP_CAW(LPELC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Drop CAW3,CAW5,CAW7,CAWC banks; only CAWX is
C-                         to be kept.
C-
C-   Inputs  : LPELC [I]  first link in linear chain of PELC/PPHO
C-   Outputs : NONE
C-   Controls: NONE
C-
C-   Created  23-AUG-1995   Ian Adam
C-   Updated  13-SEP-1995   Qizhong Li-Demarteau Corrected LCACL address 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCACL.LINK'
      INCLUDE 'D0$LINKS:IZCASH.LINK'
      INCLUDE 'D0$LINKS:IZCAW3.LINK'
      INCLUDE 'D0$LINKS:IZCAW5.LINK'
      INCLUDE 'D0$LINKS:IZCAW7.LINK'
      INCLUDE 'D0$LINKS:IZCAWC.LINK'
      INTEGER LPELC,LCACL,LCASH,LCAW
C----------------------------------------------------------------------
      IF (LPELC.LE.0) THEN
        CALL ERRMSG('BAD LINK','DROP_CAW',' ','W')
        GOTO 999
      ENDIF

      DO WHILE (LPELC.GT.0) 
        LCACL = LQ(LPELC-2)
        IF (LCACL.GT.0) THEN
          LCASH = LQ(LCACL-IZCASH)
          IF (LCASH.GT.0) THEN
            LCAW = LQ(LCASH-IZCAW3)
            IF (LCAW.GT.0) CALL MZDROP(IXCOM,LCAW,' ')
            LCAW = LQ(LCASH-IZCAW5)
            IF (LCAW.GT.0) CALL MZDROP(IXCOM,LCAW,' ')
            LCAW = LQ(LCASH-IZCAW7)
            IF (LCAW.GT.0) CALL MZDROP(IXCOM,LCAW,' ')
            LCAW = LQ(LCASH-IZCAWC)
            IF (LCAW.GT.0) CALL MZDROP(IXCOM,LCAW,' ')  
          ELSE
            CALL ERRMSG('NO CASH','DROP_CAW',' ','W')
          ENDIF
        ELSE
          CALL ERRMSG('NO CACL','DROP_CAW',' ','W')
        ENDIF
        LPELC = LQ(LPELC)
      ENDDO

  999 RETURN
      END
