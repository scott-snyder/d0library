      SUBROUTINE FDC_VERTEX_FIT(LFDCT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Do fit of FDC track with z-vertex. 
C-   The z-vertex used is the constrained z-vertex. For multiple vertices, the
C-   z-vertex which gives the smallest impact parameter to this track is picked.
C-
C-   Inputs  : LFDCT = Link to FDCT bank
C-   Outputs : stored in FDCT bank
C-   Controls: none
C-
C-   Created  10-DEC-1993   Srini Rajagopalan (Formatted after FITFDC_VERTEX)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER LFDCT
      INTEGER HALF,TRACK,DUM,IBITS
      INTEGER WANTED,FOUND,IFOUND,IVERT
      INTEGER IQTRAK(26),IQHSEC(3,34)
      INTEGER LADDER(0:2)
C
      REAL X,Y,Z,R
      REAL QTRAK(26),QHSEC(3,34)
      REAL DIFF,CHINORM
C
      PARAMETER (WANTED = 10)
      REAL INFO(3,WANTED)
C
      LOGICAL OK
C----------------------------------------------------------------------
C
      TRACK = IQ(LFDCT-5)
      CALL GTFDCT(TRACK,QTRAK,QHSEC,LADDER)
C
      CALL UCOPY(QTRAK(1),DUM,1)
      HALF = IBITS(DUM,0,1)
C
      CALL FGET_CLOSE(TRACK,X,Y,Z,R)
      CALL VERTEX_INFO(WANTED, FOUND, INFO, OK)
      IF (.NOT.OK .OR. FOUND.LE.0) GO TO 999
C
      DIFF = 99999.
      DO IFOUND = 1,FOUND
        IF (ABS(Z-INFO(1,IFOUND)) .LT. DIFF) THEN
          DIFF = ABS(Z - INFO(1,IFOUND))
          IVERT = IFOUND
        ENDIF
      ENDDO
C
      CALL FTFDCT(HALF,LADDER,QTRAK,IQTRAK,QHSEC,IQHSEC,
     &            CHINORM,IVERT)
C
C Update FDCT banks
C
      Q(LFDCT+26) = QTRAK(22)         ! Theta
      Q(LFDCT+27) = QTRAK(24)         ! Error - Theta
      Q(LFDCT+28) = QTRAK(6)          ! Phi
      Q(LFDCT+29) = QTRAK(23)         ! Error - Phi
      Q(LFDCT+30) = QTRAK(19)         ! Chi**2 of fit
      IQ(LFDCT+31) = IVERT            ! Vertex associated with
C
  999 RETURN
      END
