C VAX/DEC CMS REPLACEMENT HISTORY, Element FDC_TZERO.FOR
C *1     4-NOV-1993 10:53:13 AVERY "FDC changes for v12 RECO"
C VAX/DEC CMS REPLACEMENT HISTORY, Element FDC_TZERO.FOR
      SUBROUTINE FDC_TZERO(T0, T0_ERR, PHI, THETA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Steering routine to calculates t0 from FDC
C-                         track segments.
C-
C-   Inputs  :  none
C-   Outputs :  T0      T0(ns) obtained from FDC track with lowest t0_err.
C-              T0_ERR  (ns)
C-              PHI     of FDC track (radians)
C-              THETA   of FDC track (radians)
C-
C-   Created  28-MAY-1993   Robert E. Avery
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
C
C Output:
      REAL    T0, T0_ERR, PHI, THETA
C
C Local:
C
      INTEGER LRCP,ERR
      INTEGER ITRK,NTRK
      INTEGER LFTRH,GZFTRH
      INTEGER LFDCT,GZFDCT
C
      REAL    TZERO_0
      REAL    T0_ERR_TRK 
      REAL    MIN_T0_ERR
      REAL    T0_ERR_FACT
C
      LOGICAL FIRST
C
      DATA FIRST /.TRUE./
      DATA MIN_T0_ERR  / 100. /
      DATA T0_ERR_FACT / 2.0 /
C----------------------------------------------------------------------
      T0 = 0.0
      T0_ERR = 0.0
      PHI = 0.0
      THETA = 0.0
C
      IF (FIRST) THEN
        CALL EZLOC('FTRAKS_RCP',LRCP)
        IF(LRCP.GT.0) THEN
          CALL EZPICK('FTRAKS_RCP')
          CALL EZGET('MIN_T0_ERR',MIN_T0_ERR,ERR)
          CALL EZGET('T0_ERR_FACT',T0_ERR_FACT,ERR)
          CALL EZRSET
        END IF
        FIRST = .FALSE.
      END IF
C
C First, do FDC cosmic ray tracking:
C
      CALL FDC_COSMIC_TRACKS(TZERO_0)
C
C If no tracks, then no tzero
C
      LFTRH=GZFTRH()
      NTRK=0
      IF ( LFTRH.GT.0 ) NTRK = IQ(LFTRH+2)
      IF ( NTRK.EQ.0 ) GOTO 999
C
C Use track with best error (in rare case where there are two).
C
      T0_ERR = MIN_T0_ERR
      LFDCT = GZFDCT(0)
      DO WHILE (LFDCT.GT.0)
C
        T0_ERR_TRK = T0_ERR_FACT * Q(LFDCT+27)
        IF ( T0_ERR_TRK.LT.T0_ERR  ) THEN
          T0_ERR = T0_ERR_TRK
          T0 = TZERO_0 + Q(LFDCT+26)
          PHI = Q(LFDCT+6)
          THETA = Q(LFDCT+22)
        ENDIF
C
        LFDCT = LQ(LFDCT)
      ENDDO
C
      IF ( T0_ERR.LT.MIN_T0_ERR ) THEN
        CALL FDC_T0SHIFT( T0, 1 )
      ELSE
        T0_ERR = 0.0
      ENDIF
C
  999 RETURN
      END
