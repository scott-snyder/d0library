      REAL FUNCTION MNETOF( ITRAK,TXYZ ) 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return expected Time of flight for muon
C-                         at scintillator plane. 
C-
C-   Returned value  : expected T.O.F
C-                     if no scinti hit associate with track, -9999.0
C-   Inputs  : ITRAK  [I] : MUOT track id
C_             TXYZ(3)[R] : X,Y,Z coordinates of Track @ Scintillator
C-   Outputs : none
C-   Controls: none
C-
C-   Created   3-MAR-1994   Atsushi Taketani
C-   Modified 31-JAN-1995   R. Markeloff  Now works for SSQ files
C-   Modified  8-FEB-1995   R. Markeloff  Change GTVERT to VERXYZ
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER  ITRAK
C
      REAL     TXYZ(3)
      INTEGER  K, MODE, LDUMMY, IER, IVER, NV
      REAL     VXYZ(3), ERR(3)
      INTEGER  LMUOT, GZMUOT
      REAL     MXYZ(3), RM , RS, PATH, ETOF, C
      PARAMETER (C=29979E-3) ! speed of light in cm/nsec
C
C----------------------------------------------------------------------
      ETOF = -9999.0
C
      CALL VERXYZ(IVER,VXYZ,NV)
C
C     If z_vertex is 0.0 for real data, then assume SSQ data
C
      IF (VXYZ(3) .EQ. 0. .AND. IQ(LHEAD+1) .LT. 1000) THEN
        IF (IQ(LHEAD+6) .LT. 70000) THEN    ! Run 1a
          MODE = 1
        ELSE
          MODE = 2                          ! Run 1b
        ENDIF
        CALL MUGET_VERT(MODE,LDUMMY,VXYZ,ERR,IER)
      ENDIF
C
      LMUOT = GZMUOT( ITRAK ) ! get mag center point
      DO 200 K=1,3
  200   MXYZ(K) = Q(LMUOT+10+K)
C
C path length to scint
C
      RM= 0.0          
      RS = 0.0
      DO 300 K=1,3
        RM = RM + (VXYZ(K)-MXYZ(K))**2
        RS = RS + (TXYZ(K)-MXYZ(K))**2
  300 CONTINUE
      PATH = SQRT(RM) + SQRT(RS)
C
      ETOF = PATH/C
C
  999 MNETOF = ETOF
      RETURN
      END
