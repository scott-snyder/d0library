      SUBROUTINE CL2_PUT_GEOM(MONTE_CARLO)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : copy the offline geometry under SL2H for downloading
C-        use only from CL2_MAKE_TABLES
C-
C-   Inputs  : CGEH and dependents under offline position
C-   Outputs : offline geometry under SL2H      (post condition)
C-   Controls: MONTE_CARLO  .TRUE. do not download alignment banks
C-
C-   Created  27-JUL-1992   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CL2_STP_LINK.INC'
      INCLUDE 'D0$LINKS:IZCADT.LINK'
      INCLUDE 'D0$LINKS:IZCAGS.LINK'
      INCLUDE 'D0$LINKS:IZCLIN.LINK'
      INCLUDE 'D0$LINKS:IZ2CGEH.LINK'
      LOGICAL MONTE_CARLO
      INTEGER L2CGEH,L2CUCL,L2CECL,L2CRST,L2CADT2,L2CAGS2,L2CLIN
C----------------------------------------------------------------------
C
C...prepare for downloading: copy (not shunt, to allow other INITs)
C...    from CGEH to SL2H
C
C...are the download banks already in place?
      L2CGEH = LC(LSL2H-IZ2CGEH)          
      IF (L2CGEH.LE.0) THEN               ! no.  Something to do
        IF (LCGEH.LE.0) THEN              ! but missing the input
        CALL ERRMSG('NO CGEH FOUND','CL2_PUT_GEOM',
     &        'CGEH bank not found','F')
        ELSE
C
C...the CGEH bank tree is copied then trimmed
          CALL MZCOPY(IDVSTP,LCGEH,IDVSTP,LSL2H,-IZ2CGEH,' ')
          L2CGEH = LC(LSL2H-IZ2CGEH)
          IF (L2CGEH.LE.0) THEN
            CALL ERRMSG('CGEH LOST','CL2_PUT_GEOM',
     &        'CGEH bank not copied correctly to SL2H','F')
          ENDIF
          L2CLIN = LC(L2CGEH-IZCLIN)          ! Alignment banks
          IF(MONTE_CARLO.AND.L2CLIN.GT.0) CALL MZDROP(IXSTP,L2CLIN,'L')
          L2CUCL = LC(L2CGEH-5)          ! SRCP bank for CC
C          IF (L2CUCL.GT.0) CALL MZDROP (IXSTP,L2CUCL,' ')
          L2CECL = LC(L2CGEH-6)          ! SRCP bank for EC
C          IF (L2CECL.GT.0) CALL MZDROP (IXSTP,L2CECL,' ')
          L2CRST = LC(L2CGEH-7)          ! SRCP bank for rest
          IF (L2CRST.GT.0) CALL MZDROP (IXSTP,L2CRST,' ')
          L2CADT2 = LC(L2CGEH-IZCADT)        ! kill extra copy of CADT
          IF (L2CADT2.GT.0) CALL MZDROP (IXSTP,L2CADT2,'L')
          L2CAGS2 = LC(L2CGEH-IZCAGS)        ! kill extra copy of CAGS
          IF (L2CAGS2.GT.0) CALL MZDROP (IXSTP,L2CAGS2,' ')
        ENDIF
      ENDIF
  999 RETURN
      END
