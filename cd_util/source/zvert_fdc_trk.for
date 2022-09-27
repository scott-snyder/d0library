      SUBROUTINE ZVERT_FDC_TRK(ZVERTX,ZERROR,WEIGHT,NUSED)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Simple FDC vertex finding routine.
C-      Assumes Full FDC tracking has been done.
C-
C-   Inputs  : (FDC track banks)
C-   Outputs : ZVERTX: vertex's Z position
C-             ZERROR: error on vertex's Z position
C-             WEIGHT: percentage of the number of tracks used for this vertex
C-             NUSED: number of tracks used for this vertex
C-
C-   Created  21-MAY-1992   Robert E. Avery
C-   Updated  19-FEB-1993   Robert E. Avery   added "WEIGHT" and "NUSED"
C-   Updated   8-APR-1993   Robert E. Avery  Use beam(3) for initial guess. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL FVERTEX
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
C  Output:
      REAL    ZVERTX, ZERROR
      INTEGER WEIGHT, NUSED
C
C  Local:
      INTEGER MAX_TRACK
      PARAMETER( MAX_TRACK =  200)
C
      INTEGER IER
      INTEGER LFTRH,GZFTRH
      INTEGER LISV1 ,GZISV1
      INTEGER TRACK,NTRACK
      INTEGER I
      INTEGER ITER
C
      REAL    Z_DIFF
      REAL    X,Y,Z,R
      REAL    Z_TRACK(MAX_TRACK)
      REAL    Z_SUM
      REAL    Z_SUM_SQ
      REAL    ZMEAN, ZSIGMA 
      REAL    OLDSIGMA
      REAL    ZCUT
      REAL    Z_ISAJT 
      REAL    BEAM_POS(3)
C
      REAL    ZSIGMA_CUT,TOLDST
      REAL    ITRLMT,BIGSGM,SGMFCT,SGMFC2
C
      LOGICAL HSTFLG
      LOGICAL FIRST
C                            
      SAVE TOLDST,ZSIGMA_CUT,SGMFCT,SGMFC2,ITRLMT,BIGSGM,HSTFLG
      SAVE BEAM_POS
C
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('VERTEX_RCP')       ! Get FDC vertex-finding params
        CALL EZGET('FTOLDST',TOLDST,IER)
        CALL EZGET('FZSIGMA',ZSIGMA_CUT,IER)
        CALL EZGET('FSGMFCT',SGMFCT,IER)
        CALL EZGET('FSGMFC2',SGMFC2,IER)
        CALL EZGET('FITRLMT',ITRLMT,IER)
        CALL EZGET('FBIGSGM',BIGSGM,IER) 
        CALL EZGET_l('FHSTFLG',HSTFLG,IER) 
        CALL EZGET_rarr('BEAM_POS',BEAM_POS,IER)
        CALL EZRSET               
        IF (HSTFLG) THEN           
          CALL ZVERT_FDC_TRKBK
        ENDIF
      END IF
      CALL DHDIR('VERTEX_RCP','HBOOK_DIRECTORY',IER,' ')
C
      ZVERTX = 9999.
      ZERROR = 9999.
C
C Get number of fdc tracks
C
      LFTRH = GZFTRH()
      NTRACK = IQ(LFTRH+2)
      IF (NTRACK.GT.MAX_TRACK) NTRACK = MAX_TRACK
C
C Fill array of z positions:
C
      I = 0
      DO TRACK =  1, NTRACK 
        CALL FGET_CLOSE(TRACK,X,Y,Z,R)
        CALL HF1(503,R,1.0)
        IF ( R .LT. TOLDST ) THEN
          I = I + 1
          Z_TRACK(I) = Z
        ENDIF
      ENDDO
      NUSED = I
      IF ( NUSED.LE.1 ) GOTO 999
      NTRACK = NUSED 
C
C Iteration to find vertex:
C
      ZMEAN = BEAM_POS(3)
      ZCUT = 100.                       ! cm
      ZSIGMA = 9999.
      OLDSIGMA = ZSIGMA 
      ITER = 0
      DO WHILE (ZSIGMA .GT. ZSIGMA_CUT)
        Z_SUM = 0.0
        Z_SUM_SQ = 0.0
        I = 0
        DO TRACK =  1, NUSED
          Z_DIFF = Z_TRACK(TRACK)-ZMEAN
          IF ( ABS(Z_DIFF).LT.ZCUT ) THEN
            I = I + 1
            Z_TRACK(I) =  Z_TRACK(TRACK) 
            Z_SUM = Z_SUM + Z_TRACK(I)
            Z_SUM_SQ = Z_SUM_SQ + Z_TRACK(I)**2.
          ENDIF
        ENDDO
        IF ( I.LE.1 ) GOTO 100
        ITER = ITER + 1
        NUSED = I
        ZMEAN = Z_SUM/NUSED
        ZSIGMA = MAX(1.0, (Z_SUM_SQ/NUSED - ZMEAN**2.) )
        ZSIGMA = SQRT( ZSIGMA  )
        IF (ABS(ZSIGMA - OLDSIGMA) .LE. ITRLMT) THEN
          IF (SGMFCT .EQ. SGMFC2) GOTO 100
          SGMFCT = SGMFC2
        ENDIF
        OLDSIGMA = ZSIGMA 
        IF (ZSIGMA .LE. BIGSGM) THEN
          ZCUT = SGMFCT * ZSIGMA
        ELSE
          ZCUT = ZSIGMA
        ENDIF
      ENDDO
  100 CONTINUE
C
C Vertex found:
C
      IF ( ITER.LE.1 ) GOTO 999
      ZVERTX = ZMEAN
      ZERROR = ZSIGMA/ SQRT( FLOAT(NUSED-1) )
      WEIGHT = NINT ( (100. * NUSED) / NTRACK )
C
C Optionally fill histograms
C
      IF (HSTFLG) THEN
        CALL HF1(510,ZVERTX,1.0)
        CALL HF1(511,FLOAT(NUSED),1.0)
        CALL HF1(516,FLOAT(ITER),1.0)
        DO TRACK =  1, NUSED
          Z_DIFF = Z_TRACK(TRACK)-ZVERTX
          CALL HF1(512,Z_DIFF,1.0)
          CALL HF2(514,FLOAT(NUSED),Z_DIFF,1.0)
        ENDDO
        CALL HF2(515,FLOAT(NUSED),ZERROR,1.0)
C
C   Compare isajet track information.
C
        LISV1 = GZISV1()
        IF (LISV1 .GT. 0) THEN
          Z_ISAJT = Q(LISV1+9)
          Z_DIFF = Z_ISAJT - ZVERTX
          CALL HF2(550,FLOAT(NUSED),Z_DIFF,1.0)
          CALL HF2(551,ZERROR,Z_DIFF,1.0)
          CALL HF2(552,Z_ISAJT,Z_DIFF,1.0)
        ENDIF
      ENDIF
C
C----------------------------------------------------------------------
  999 RETURN
      END
