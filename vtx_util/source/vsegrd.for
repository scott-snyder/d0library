      SUBROUTINE VSEGRD(LAYER,SECTOR)
C------------------------------------------------------------------------
C
C  Routine for finding track segments in x-y plane in current cell
C  of VTX,  using the 'road' method
C
C  Inputs:
C          LAYER,SECTOR
C          INEFF        = maximum number of missing hits in segment
C
C  D.Zieminska  Jan. 1988 (method originally used for CDC by O.Callot)
C-   Updated  21-MAY-1992   Peter M. Grudberg  Add protection against too many
C-                                             hits
C-   Updated  30-Aug-1992   M. Pang     Changed the road defining algorithm
C-   Updated  18-AUG-1992   Myungyun Pang   Fixed a bug in handling of
C-                              wires with no hits
C-   Updated  31-AUG-1992   Myungyun Pang   The formular that calulates
C-                              expected Phi of intermediate hits of a segment
C-                              was corrected.
C-   Updated   3-SEP-1992   M. Pang    Dynamically adjust the road size
C-                              based on the errors on each hit
C-   Updated  25-SEP-1992   Peter M. Grudberg  Handle MXHWIR consistently with
C-                            the routine VPOINT (take mirror hits into account)
C-   Updated  29-SEP-1992   Peter M. Grudberg Handle branch cut in ATAN2 when
C-                              calculating PHISEG
C-   Updated   4-MAR-1994   liang-ping Chen  store relative pointers in PTHIT,
C-                              refresh LPOIN after CALL VFITSG
C------------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZUSER.LINK/LIST'
      INCLUDE 'D0$LINKS:IZRECO.LINK/LIST'
      INCLUDE 'D0$LINKS:IZHITS.LINK/LIST'
      INCLUDE 'D0$LINKS:IZVTXH.LINK/LIST'
      INTEGER NBSENS,MXHWIR,TAGBIT,NACTIV
      PARAMETER (TAGBIT=30)
      PARAMETER (NBSENS=8)
      PARAMETER (MXHWIR=50)
      INTEGER INFHIT(8,2*MXHWIR,NBSENS),NTHIT(NBSENS),PTHIT(NBSENS)
      REAL HITINF(8,2*MXHWIR,NBSENS)
      EQUIVALENCE (INFHIT,HITINF)
      INTEGER INEFF
      REAL PI,PI2,VTTRTO
      INTEGER  LPOIN,IPOINT,ICALL
      LOGICAL  BTEST
      INTEGER  LAYER, SECTOR, WIRE
      INTEGER  INEF, FIRSPL, LASTPL, FIRHIT, LASHIT, NMISS, MEDPLA
      INTEGER  JHIT, IPHIT(NBSENS), JHTMIN, LISTPT( NBSENS )
      REAL     PHIFIR, RAYFIR, PHILAS, TOLPHI, DPH, DPHMIN
      REAL     PHIEXP, TOLRES, TOLPHI_RD(3),PHIDIFF,SEGPHI
      REAL     XLAS,YLAS,XFIR,YFIR,RAYLAS,RAYMID
      INTEGER LUSER,LOC
      PARAMETER   ( PI=3.141593)
      PARAMETER ( PI2=2.*PI )
      INTEGER IER
      DATA ICALL/0/
C-----------------------------------------------------------------------
      IF (ICALL.EQ.0) THEN
        CALL EZPICK('VTRAKS_RCP')
        CALL EZGET('INEFF',INEFF,IER)
        CALL EZGET('TOLPHI_RD',TOLPHI_RD,IER)
        CALL EZGET('VTTRTO',VTTRTO,IER) ! road width (multiple of drift error)
        CALL EZRSET
        ICALL=1
      END IF
      LUSER=LQ(LHEAD-IZUSER)
      IQ(LUSER+1)=0
      IQ(LUSER+2)=0
C
C  Make a set of points from hits in sector; store them in bank 'POIN'
C
      CALL VPOINT(LAYER,SECTOR)
      LUSER=LQ(LHEAD-IZUSER)
      NACTIV=IQ(LUSER+3)
      IF (NACTIV.LE.8) THEN
        TOLPHI=TOLPHI_RD(1)  ! max delta_phi between first and last wire
      ELSE IF (NACTIV.LT.40) THEN
        TOLPHI=TOLPHI_RD(2)
      ELSE
        TOLPHI=TOLPHI_RD(3)
      END IF
      LPOIN=LQ(LUSER-1)
      LOC=LPOIN+9
      CALL UCOPY(IQ(LPOIN+1),NTHIT,NBSENS)
      CALL VZERO(HITINF,8*MXHWIR*NBSENS)
C 
C **** store  relative pointers in PTHIT array
C
      PTHIT(1)=9 
      DO 1 WIRE=1,NBSENS
        IF (WIRE.GT.1) THEN
          PTHIT(WIRE)=PTHIT(WIRE-1)+8*NTHIT(WIRE-1)
          IF (NTHIT(WIRE-1).GT.2*MXHWIR) NTHIT(WIRE-1)=2*MXHWIR
        ENDIF
        IF (NTHIT(WIRE).EQ.0) GO TO 1
        DO 2 IPOINT=1,NTHIT(WIRE)
          IF ( IPOINT .LE. 2*MXHWIR ) 
     &      CALL UCOPY(Q(LOC),HITINF(1,IPOINT,WIRE),8)
          LOC=LOC+8
    2   CONTINUE
    1 CONTINUE
      IF (NTHIT(NBSENS).GT.2*MXHWIR) NTHIT(NBSENS)=2*MXHWIR
      DO 110 INEF = 0, INEFF
        DO 120 FIRSPL = 1, INEF+1
          CALL VFILL( IPHIT, NBSENS, 1 )
          LASTPL = FIRSPL + NBSENS - INEF - 1
          CALL VZERO(LISTPT, NBSENS )
          DO 130 FIRHIT = 1, NTHIT(FIRSPL)
            LOC=PTHIT(FIRSPL)-1+(FIRHIT-1)*8+4 + LPOIN
            INFHIT(4,FIRHIT,FIRSPL)=IQ(LOC)
            IF( BTEST( INFHIT(4,FIRHIT,FIRSPL), TAGBIT) ) GOTO 130
            PHIFIR = HITINF(2, FIRHIT, FIRSPL )
            XFIR = HITINF(5, FIRHIT, FIRSPL )
            YFIR = HITINF(6, FIRHIT, FIRSPL )
            RAYFIR = HITINF(1, FIRHIT, FIRSPL )
            LISTPT( FIRSPL ) = FIRHIT
            DO 140 LASHIT = 1, NTHIT(LASTPL)
              LOC=PTHIT(LASTPL)-1+(LASHIT-1)*8+4 + LPOIN
              INFHIT(4,LASHIT,LASTPL)=IQ(LOC)
              IF( BTEST( INFHIT(4,LASHIT,LASTPL), TAGBIT) ) GOTO 140
              PHILAS = HITINF( 2, LASHIT, LASTPL )
              RAYLAS = HITINF(1, LASHIT, LASTPL )
              XLAS = HITINF( 5, LASHIT, LASTPL )
              YLAS = HITINF( 6, LASHIT, LASTPL )
              IF ( PHILAS-PHIFIR .LT. -TOLPHI ) GOTO 140
              IF ( PHILAS-PHIFIR .GT.  TOLPHI ) GOTO 130
C
C ****  If PHIFIR is over PI, work only with PHILAS below ( already worked
C ****  with duplicated info around -PI )
C
C                IF ( PHIFIR .GE. PI ) THEN
C                  IF( PHILAS .GE. PI ) GOTO 130
C                ENDIF

              SEGPHI = ATAN2(YLAS-YFIR,XLAS-XFIR)
C
C ****  Handle branch cut in ATAN2
C
              IF ( ABS(PHIFIR) .GT. PI/2. ) THEN
                IF ( SEGPHI.LT.0. .AND. PHIFIR.GT.0. ) 
     &            SEGPHI = SEGPHI + PI2
                IF ( SEGPHI.GT.0. .AND. PHIFIR.LT.0. ) 
     &            SEGPHI = SEGPHI - PI2
              ENDIF
              PHIDIFF = PI - SEGPHI + PHIFIR
              NMISS = LASTPL - FIRSPL - NBSENS + INEFF + 1
              LISTPT( LASTPL ) = LASHIT
              DO 150 MEDPLA = FIRSPL+1, LASTPL-1
                IF ( NTHIT(MEDPLA) .NE. 0 ) THEN
                  RAYMID = HITINF(1, 1, MEDPLA )
                  JHIT   = IPHIT( MEDPLA )
                  DPHMIN = 100.
C
C ****  Loop on hit, start with previous one, direction depends on sign,
C ****  will stop due to non decreasing distance. Store hit pointer
C
  160             CONTINUE
                  PHIEXP = PHIFIR + PI -
     &              ASIN((RAYFIR*SIN(PHIDIFF))/HITINF(1, JHIT, MEDPLA))
     &              - PHIDIFF
                  IF( ABS(PHIEXP-HITINF(2,JHIT,MEDPLA)) .LT.
     &                                               DPHMIN ) THEN
                    DPH    = PHIEXP-HITINF(2,JHIT,MEDPLA)
                    DPHMIN = ABS( DPH )
                    JHTMIN = JHIT
                    IF( DPH .GT. 0. ) THEN
                      IF ( JHIT .LT. NTHIT(MEDPLA) ) THEN
                        JHIT = JHIT + 1
                        GOTO 160
                      ENDIF
                    ELSE
                      IF( JHIT .NE. 1 ) THEN
                        JHIT = JHIT - 1
                        GOTO 160
                      ENDIF
                    ENDIF
                  ENDIF
                  IPHIT( MEDPLA ) = JHIT
                  TOLRES = VTTRTO*(1./SQRT(HITINF(3,JHTMIN,MEDPLA)))
     &                                / RAYMID
                  IF ( DPHMIN .LT. TOLRES ) THEN
                    LISTPT( MEDPLA ) = JHTMIN
                  ELSE
                    LISTPT( MEDPLA ) = 0
                    NMISS = NMISS - 1
                    IF( NMISS .LT. 0 ) GOTO 140         ! Try next couple
                  ENDIF
                ELSE                                  ! NO HIT ON THIS WIRE
                  LISTPT( MEDPLA ) = 0
                  NMISS = NMISS - 1
                  IF( NMISS .LT. 0 ) GOTO 140         ! Try next couple
                END IF
  150         CONTINUE
C
C ****  Fit segment, tag hits, load bank SEGM.
C
              CALL VFITSG(LAYER,SECTOR,LISTPT)
C
C ****  refresh LPOIN in case a garbage collection is done in VFITSG
C
              LUSER=LQ(LHEAD-IZUSER)
              LPOIN=LQ(LUSER-1)
  140       CONTINUE
  130     CONTINUE
  120   CONTINUE
  110 CONTINUE
 1000 CONTINUE
      RETURN
      END
