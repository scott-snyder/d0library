      SUBROUTINE CL2_STPLNK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-      get links to CADT and CAGS into CL2 link area
C-   Inputs  : CADT, CAGS
C-   Outputs : /CL2_STP_LINK/
C-   Controls:
C-
C-   Created  26-APR-1991   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CL2CRATE.INC'
      INCLUDE 'D0$LINKS:IZ2CADT.LINK'
      INCLUDE 'D0$LINKS:IZ2CAGS.LINK'
      INCLUDE 'D0$INC:CL2_STP_LINK.INC'
      INTEGER GZSL2H,L2CADT
      INTEGER ICADT                     ! loop index over cadt banks (crates)
      INTEGER CRATEID            ! hardware,l2 crate numbers
C----------------------------------------------------------------------
C
C ****  SETUP CADT LOOK-UP TABLE
C
      LSL2H = GZSL2H ()
      L2CADT = LC(LSL2H-IZ2CADT)        ! see if already there
      IF (L2CADT.LE.0) THEN
           CALL ERRMSG('CALORIMETER','CL2_STPLNK','CADT CHAIN BAD','E')
      ENDIF
      DO ICADT = 1,12
        CRATEID = L2CRATE(IC(L2CADT+2))    ! LEVEL 2 crate number
        IF ((CRATEID.LE.0).OR.(CRATEID.GT.12)) THEN
          CALL ERRMSG('CALORIMETER','CL2_STPLNK',
     &        'INVALID CRATE NUMBER IN CADT','E')
        ELSE
          IF (L2CADT.LE.0) THEN
            CALL ERRMSG('CALORIMETER','CL2_STPLNK',
     &          'CADT CHAIN BAD','E')
          ELSE
            LL2CADT(CRATEID) = L2CADT
            L2CADT = LC(L2CADT)         ! follow linear chain
          ENDIF
        ENDIF
      ENDDO
      L2CAGS = LC(LSL2H-IZ2CAGS)          ! see if already there
      IF (L2CAGS.LE.0) THEN
        CALL ERRMSG('CALORIMETER','CL2_STPLNK','NO CAGS BANK','E')
      ENDIF
  999 RETURN
      END
