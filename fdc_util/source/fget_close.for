      SUBROUTINE FGET_CLOSE(TRACK,X,Y,Z,R)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : For FDCT track, get X,Y,Z and R of point of 
C-   closest approach to beam line. X and Y are in the D0 coordinate 
C-   frame, while R is the impact parameter.
C-
C-   Inputs  : TRACK
C-   Outputs : X,Y,Z,R 
C-
C-   Created  21-MAY-1992   Robert E. Avery
C-   Updated   2-MAR-1993   Robert E. Avery  Use BEAM_POS, but return
C-                              X,Y in D0 coordinates.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
C  Input:
      INTEGER TRACK
C
C  Output:
      REAL    X,Y,Z,R
C
C  Local:
      INTEGER LFDCT,GZFDCT
      INTEGER IER,LRCP
      REAL    X_0,Y_0
      REAL    DX_0,DY_0
      REAL    Z0
      REAL    BEAM_POS(3)
      LOGICAL FIRST
C
      SAVE BEAM_POS,FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        BEAM_POS(1) = 0.0
        BEAM_POS(2) = 0.0
        CALL EZLOC('VERTEX_RCP',LRCP)                        
        IF(LRCP.GT.0) THEN                                   
          CALL EZPICK('VERTEX_RCP')
          CALL EZGET('BEAM_POS',BEAM_POS,IER)
          CALL EZRSET
        ELSE
          CALL EZLOC('FDC_RCP',LRCP)                        
          IF(LRCP.GT.0) THEN                                   
            CALL EZPICK('FDC_RCP')
            CALL EZGET('BEAM_POS',BEAM_POS,IER)
            CALL EZRSET
          ENDIF
        ENDIF
      END IF
C
      Z = 999.
      X = 999.
      Y = 999.
      R = 999.
      LFDCT = GZFDCT(TRACK)
      IF ( LFDCT.LE.0 ) GOTO 999
C
C X_0, Y_0 AT Z = 0, relative to beam position.
C
      CALL FGETZ0(TRACK,Z0)
      DX_0 = Q(LFDCT + 7)
      DY_0 = Q(LFDCT + 8)
      X_0 = Q(LFDCT + 4) + DX_0*(0.0-Z0) - BEAM_POS(1)
      Y_0 = Q(LFDCT + 5) + DY_0*(0.0-Z0) - BEAM_POS(2)
C
C X,Y,Z for closest aproach to beam:
C
      IF ( ABS(DX_0*DY_0).GT.0 )  THEN
        Z = -(DX_0*X_0 +DY_0*Y_0) / (DX_0**2. + DY_0**2.)
        X = X_0 + Z * DX_0
        Y = Y_0 + Z * DY_0
        R = SQRT( X**2. + Y**2. )
C
C Tranlate X,Y back to D0 coordinate system:
C
        X = X + BEAM_POS(1)
        Y = Y + BEAM_POS(2)
      ENDIF
  999 RETURN
      END
