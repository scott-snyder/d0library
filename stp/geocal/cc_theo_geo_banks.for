      SUBROUTINE CC_THEO_GEO_BANKS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : THIS SUBROUTINE GETS THE THEORETICAL
C-                         CORNERS FROM THE GEOMETRY DATABASE.
C-                         IT STICKS THEM INTO BANKS THAT HAVE
C-                         THE SAME FORMAT AS THE SURVEY DATA FOR
C-                         EASE OF MATCH UP DURING FITTING.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   6-MAR-1991   Stephen Kahn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CALOR_SURVEY.INC'
      INCLUDE 'D0$INC:CLINKS.INC'
      INCLUDE 'D0$LINKS:IZCSYL.LINK'
      INCLUDE 'D0$LINKS:IZCMDL.LINK'
      INCLUDE 'D0$LINKS:IZCTHE.LINK'
      INCLUDE 'D0$LINKS:IZCREG.LINK'
      INCLUDE 'D0$LINKS:IZCLGA.LINK'
      INCLUDE 'D0$LINKS:IZCLGI.LINK'
      INCLUDE 'D0$PARAMS:CLGA.PARAMS'
      INCLUDE 'D0$INC:REGION.DEF'
      INTEGER LZFIND, MCORNS, IERR, NC, II, MOD_TYPE, CYL_TYPE
      INTEGER JERR, JJ
      REAL    KLOC, FIXPT(3), DL
      PARAMETER( MCORNS =  20)          ! maximum number of corners that
                                        ! MODULE_CORNERS can produce
      REAL    X(MCORNS), Y(MCORNS), Z(MCORNS)
C
      CALL EZPICK('CAL_SURVEY_MARKERS_RCP')
      CALL EZGET('CC_THERMAL_COEFF',DL,IERR)
      CALL EZGET('CC_FIXED_POINT', FIXPT, JERR)
      IF( IERR.NE.0 .OR. JERR.NE.0) THEN
        CALL ERRMSG('NO THERMAL CONTRACTION','CC_THEO_GEO_BANKS',
     +  'no CC thermal contrat params found','W')
        DL = 1.0
        CALL VZERO( FIXPT, 3)
      END IF
      CALL EZRSET
C
      LQCREG = LZFIND(IXSTP,LC(LCGEH-IZCREG),ICCAL,IGREGN)  ! region pointer
C
C...  CENTRAL CALORIMETER MODULES
C
      DO 100 II = 1, 3                  ! II= 1,2,3 gives EM,FH,CH
      IF(II.EQ.1) THEN
        MOD_TYPE = ICCEMA               ! EM type
        CYL_TYPE = ICCEMI
      ELSE IF(II .EQ. 2) THEN
        MOD_TYPE = ICCFHA               ! FH type
        CYL_TYPE = ICCFHI
      ELSE IF(II .EQ. 3) THEN
        MOD_TYPE = ICCCHA               ! CH type
        CYL_TYPE = ICCCHI
      ELSE
        MOD_TYPE = 0                    ! error condition
        CYL_TYPE = 0
      END IF
C
      LCSYL = LZFIND(IXSTP,LC(LCSRV-IZCSYL),II,ISCYLN)     ! find correct
                                        ! cylinder
      LQCREG = LZFIND(IDVSTP,LC(LCGEH-IZCREG),ICCAL,IGREGN)
      LQCLGI = LZFIND(IDVSTP,LC(LQCREG-IZCLGI),CYL_TYPE,IGIDEN)
                                        ! find corresponding CLGI bank
      LC(LCSYL-IRCLGI) = LQCLGI         ! place link to CLGI in CSYL bank
      LCMDL = LC(LCSYL-IZCMDL)          ! first module
C
      DO WHILE (LCMDL .NE. 0)
        KLOC = IC(LCMDL+ISNUMB)         ! Kroon location number -- note
                                        ! KLOC is floating point number
        LQCLGA = LC(LQCREG-IZCLGA)
        DO WHILE (LQCLGA .NE. 0)        ! search for CLGA bank with
                                        ! proper type and proper
                                        ! location number
          IF( MOD_TYPE .EQ. 100*(IC(LQCLGA+IGIDEN)/100) .AND. KLOC .EQ.
     &      C(LQCLGA+IGSERL)) THEN
            CALL MODULE_CORNERS( LQCLGA, 0, X, Y, Z, NC, IERR)
            CALL BKCTHE( LCTHE, 0)      ! book theoretical module
                                        ! corners bank
            LC(LCTHE-IRCMDL) = LCMDL    ! reference link to CMDL
            LC(LCMDL-IRCTHE) = LCTHE    ! reference link to CTHE
            LC(LCTHE-IRCLGA) = LQCLGA   ! reference link to CLGA
            LC(LCMDL-IRCLGA) = LQCLGA
            IC(LCTHE+ISMODL) = IC(LCMDL+ISMODL)
            IC(LCTHE+ISNUMB) = IC(LCMDL+ISNUMB)
            IC(LCTHE+ISNMSR) = NC
            IC(LCTHE+ISMCOD) = 255          ! all "measurements" made

            DO 70 JJ = 1, 8
            CALL COLD_SHRINK( X(JJ), Y(JJ), Z(JJ), DL, FIXPT)
   70       CONTINUE

            C(LCTHE+ISNILX) = X(2)          ! north-inner-lower corner
            C(LCTHE+ISNILX+1) = Y(2)
            C(LCTHE+ISNILX+2) = Z(2)
            C(LCTHE+ISNIUX) = X(1)          ! north-inner-upper corner
            C(LCTHE+ISNIUX+1) = Y(1)
            C(LCTHE+ISNIUX+2) = Z(1)
            C(LCTHE+ISNOLX) = X(4)          ! north-outer-lower corner
            C(LCTHE+ISNOLX+1) = Y(4)
            C(LCTHE+ISNOLX+2) = Z(4)
            C(LCTHE+ISNOUX) = X(3)          ! north-outer-upper corner
            C(LCTHE+ISNOUX+1) = Y(3)
            C(LCTHE+ISNOUX+2) = Z(3)
            C(LCTHE+ISSILX) = X(6)          ! south-inner-lower corner
            C(LCTHE+ISSILX+1) = Y(6)
            C(LCTHE+ISSILX+2) = Z(6)
            C(LCTHE+ISSIUX) = X(5)          ! south-inner-upper corner
            C(LCTHE+ISSIUX+1) = Y(5)
            C(LCTHE+ISSIUX+2) = Z(5)
            C(LCTHE+ISSOLX) = X(8)          ! south-outer-lower corner
            C(LCTHE+ISSOLX+1) = Y(8)
            C(LCTHE+ISSOLX+2) = Z(8)
            C(LCTHE+ISSOUX) = X(7)          ! south-outer-upper corner
            C(LCTHE+ISSOUX+1) = Y(7)
            C(LCTHE+ISSOUX+2) = Z(7)
          END IF
          LQCLGA = LC(LQCLGA)
        END DO                              ! end of loop on CLGA
        LCMDL = LC(LCMDL)
      END DO                                ! end of loop on CMDL
  100 CONTINUE                              ! end of loop on MOD_TYPE
C----------------------------------------------------------------------
  999 RETURN
      END
