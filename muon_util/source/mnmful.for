      SUBROUTINE MNMFUL( ITRAK, IPTR, XYZ, NPTR, ACTIVE )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Muon scintillator matching track scan
C_                         by full scanning
C-                         Code support only CF octant 0-3
C-
C-   Inputs  : ITRAK      MUOT track number
C-   Outputs : IPTR       matched MSCT hit pointer
C-             XYZ(3)     track hit postion in global coords.
C-             NPTR       number of matched hits
C_             ACTIVE     1 if track points to active scint., 0 otherwise
C-   Controls: 
C-
C-   Created   8-FEB-1994   Atsushi Taketani
C-   Updated  11-Mar-1994   Atsushi Taketani protection against bad track
C-   Updated  12-Jun-1994   R. Markeloff  EZPICK and EZRSET calls removed 
C-   Updated  01-Mar-1994   R. Markeloff  Initialize ACTIVE, update call
C-                          to MNACTIVE
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C-- Argument
      INTEGER  ITRAK,IPTR(4), NPTR, ACTIVE
      REAL     XYZ(3,4)
C-- Include
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'     ! D0 ZEBRA bank.
C-- Local
      LOGICAL FIRST
      REAL    DRFT_MAX, WIRE_MAX
      INTEGER IER
C
      INTEGER LMUOT, GZMUOT
      INTEGER IMOD, ISCN, ICAT
C
      INTEGER NCAN, ICAN(4), ICAN_MOD(4)
      LOGICAL MNACTIVE
C
      REAL    XYZPLN(4,3)
C
      INTEGER I, K
      REAL    TPOS(3), TVEC(3), DEV
C
      DATA    FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZGET( 'SCINT_F_DRFT', DRFT_MAX, IER )
        CALL EZGET( 'SCINT_F_WIRE', WIRE_MAX, IER )
      END IF
C
      ACTIVE = 0
C
C get intersection
C
      CALL MNMINT( ITRAK, DRFT_MAX,WIRE_MAX,IMOD, ISCN, ICAT )
      IF ( IMOD.EQ.0.OR.ISCN.EQ.0 ) THEN
        NPTR = 0
        ACTIVE = 0
        GOTO 999
      END IF
C
C get adjacent sicnti
C
      CALL MNMODT( IMOD, ISCN, ICAT, NCAN, ICAN_MOD, ICAN )
      IF ( NCAN.EQ.0 ) THEN
        ACTIVE = 0
        NPTR = 0
        GOTO 999
      END IF
C
C SCAN scintillator hit
C
      CALL MNFIND( NCAN, ICAN_MOD, ICAN, NPTR, IPTR, XYZPLN )
C
C Scan active scintillator
C
      ACTIVE = 0
      DO I = 1,NCAN
        IF (MNACTIVE(ICAN_MOD(I))) ACTIVE = 1
      ENDDO
      IF ( NPTR.EQ.0 ) GOTO 999
C
C Intersection
C
      LMUOT = GZMUOT(ITRAK)
      DO I=1,3                   ! muot BC track segment
        TPOS(I) = Q(LMUOT+10+I)
        TVEC(I) = Q(LMUOT+16+I)
      END DO
C
      DO 300 I=1,NPTR
        IF ( XYZPLN(I,1).NE.0.0 ) THEN
          DEV      = (XYZPLN(I,1) - TPOS(1))/TVEC(1)
        ELSE IF ( XYZPLN(I,2).NE.0.0 ) THEN
          DEV      = (XYZPLN(I,2) - TPOS(2))/TVEC(2)
        END IF
        DO K=1,3
          XYZ(K,I) = TPOS(K) + TVEC(K)*DEV
        END DO
  300 CONTINUE
C
  999 RETURN
      END
