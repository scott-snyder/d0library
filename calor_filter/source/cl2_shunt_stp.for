      SUBROUTINE CL2_SHUNT_STP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Move level 2 CL2 STP banks under the
C-      level 2 header for downloading
C-
C-   Inputs  : CAGS,CADT banks
C-   Outputs : the same, moved to under SL2H
C-   Controls: none
C-
C-   Created  27-APR-1991   James T. Linnemann
C-   Updated  14-NOV-1991   James T. Linnemann  don't move 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCADT.LINK'
      INCLUDE 'D0$LINKS:IZCAGS.LINK'
      INCLUDE 'D0$LINKS:IZ2CADT.LINK'
      INCLUDE 'D0$LINKS:IZ2CAGS.LINK'
      INTEGER L2CADT,L2CAGS
      INTEGER LCADT,LCAGS,LSL2H
      INTEGER GZCADT,GZCAGS,GZCGEH,GZSL2H
C----------------------------------------------------------------------
C
C...prepare for downloading: copy (not shunt, to allow other INITs)
C...    from CGEH to SL2H
C
C...assumes both CADT and CAGS are available
      LSL2H = GZSL2H()
      IF (LSL2H.LE.0) CALL BKSL2H(LSL2H)
      LCADT = GZCADT()
      IF (LCADT.LE.0.) THEN
        CALL ERRMSG('CALORIMETER','CL2_SHUNT_STP',
     &          'CADT banks not found','F')
      ELSE
        L2CADT = LC(LSL2H-IZ2CADT)        ! see if already there
        IF (L2CADT.GT.0) CALL MZDROP(IXSTP,L2CADT,'L')    ! drop old data
        CALL MZCOPY(IDVSTP,LCADT,IDVSTP,LSL2H,-IZ2CADT,'L')
      ENDIF
      LCAGS = GZCAGS()
      IF (LCAGS.LE.0) THEN
        CALL ERRMSG('CALORIMTER','CL2_SHUNT_STP',
     &        'CAGS bank not found','F')
      ELSE
        L2CAGS = LC(LSL2H-IZ2CAGS)          ! see if already there
        IF (L2CAGS.GT.0) CALL MZDROP(IXSTP,L2CADT,'L')      ! drop old data
        CALL MZCOPY(IDVSTP,LCAGS,IDVSTP,LSL2H,-IZ2CAGS,' ')
      ENDIF
  999 RETURN
      END
