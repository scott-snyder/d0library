      SUBROUTINE ECS_THEO_GEO_BANKS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : THIS SUBROUTINE GETS THE THEORETICAL
C-                         MARKERS FOR THE ECS FROM THE GEOMETRY
C-                         DATABASE.  IT STICKS THEM INTO BANKS THAT
C-                         HAVE THE SAME FORMAT AS THE SURVEY DATA FOR
C-                         EASE OF MATCH UP DURING FITTING.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   20-DEC-1991   Stephen Kahn
C-   Updated   19-APR-1994   Adam L. Lyon: Fixed to compile under IBM/AIX
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
      INTEGER LZFIND, MMARKS, IERR, NC, II, MOD_TYPE, CYL_TYPE
      INTEGER REG_TYPE, MOD2_TYPE, JQCLGA, I, IP
      REAL    KLOC
      PARAMETER( MMARKS =  64)          ! maximum number of marks that
                                        ! MODULE_MARKERS can produce
      REAL    X(MMARKS), Y(MMARKS), Z(MMARKS)
C
C
C...  ECS CALORIMETER MODULES
C
      DO 100 II = 1, 6                  ! II= 1,2,3,4 gives EM,IH,MH,OH
      IF(II.EQ.1) THEN
        MOD_TYPE = ICEEMA               ! EM type
        CYL_TYPE = ICEEMI
        REG_TYPE = ISEMCL               ! only south banks exist
        MOD2_TYPE = 0                   ! will hang off of those
      ELSE IF(II .EQ. 2) THEN
        MOD_TYPE = ICIFHA               ! IH up stream module type
        CYL_TYPE = ICIFHI
        REG_TYPE = ISECAL               ! only south banks exist
        MOD2_TYPE = ICICHA              ! IH down stream module type
      ELSE IF(II .EQ. 3) THEN
        MOD_TYPE = ICMFHA               ! MH up stream module type
        CYL_TYPE = ICMFHI
        REG_TYPE = ISECAL               ! only south banks exist
        MOD2_TYPE = ICMCHA              ! MH down stream module type
      ELSE IF(II .EQ. 4) THEN
        MOD_TYPE = ICOCHA               ! CH module type
        CYL_TYPE = ICOCHI
        REG_TYPE = ISECAL               ! only south banks exist
        MOD2_TYPE = 0                   ! will hang off of those
      ELSE
        MOD_TYPE = 0                    ! error condition
        CYL_TYPE = 0
        REG_TYPE = 0
        MOD2_TYPE = 0
      END IF
C
      IF( REG_TYPE .EQ. 0) GO TO 100    ! not valid region
      LCSYL = LZFIND(IXSTP,LC(LCSRV-IZCSYL),II,ISCYLN)     ! find correct
                                        ! cylinder
      LQCREG = LZFIND(IDVSTP,LC(LCGEH-IZCREG),REG_TYPE,IGREGN)
      LQCLGI = LZFIND(IDVSTP,LC(LQCREG-IZCLGI),CYL_TYPE,IGIDEN)
                                        ! find corresponding CLGI bank
      LC(LCSYL-IRCLGI) = LQCLGI         ! place link to CLGI in CSYL bank
      LCMDL = LC(LCSYL-IZCMDL)          ! first module
C
      DO WHILE (LCMDL .NE. 0)
        KLOC = IC(LCMDL+ISNUMB)         ! Kroon location number -- note
                                        ! KLOC is floating point number
        IF( KLOC .EQ. 0.0 )THEN         ! IH and ECEM
          LQCLGA = LZFIND(IDVSTP, LC(LQCREG-IZCLGA), MOD_TYPE, IGIDEN)
          CALL MODULE_MARKERS( LQCLGA, IC(LQCLGA+IGIDEN), X, Y,
     &       Z, NC, IERR)               ! get marker
                                        ! positions from RCP information
          CALL BKCTHE( LCTHE, NC)       ! book theoretical module corners bank
          LC(LCTHE-IRCMDL) = LCMDL      ! reference link to CMDL
          LC(LCMDL-IRCTHE) = LCTHE      ! reference link to CTHE
          LC(LCTHE-IRCLGA) = LQCLGA     ! reference link to CLGA
          LC(LCMDL-IRCLGA) = LQCLGA
          IC(LCTHE+ISMODL) = IC(LCMDL+ISMODL)
          IC(LCTHE+ISNUMB) = IC(LCMDL+ISNUMB)
          IC(LCTHE+ISNMSR) = NC         ! number of measurements
          IF(NC.GE.32) THEN
C&IF IBMAIX,LINUX
C&            IC(LCTHE+ISMCOD) = NOT(0)
C&ELSE
            IC(LCTHE+ISMCOD) = JNOT(0)
C&ENDIF
          ELSE
            IC(LCTHE+ISMCOD) = 2**NC -1   ! all "measurements made"
          END IF
          DO 75 I = 1, NC
            IP = ISNILX + NMWDS*(I-1)
            C(LCTHE+IP) = X(I)
            C(LCTHE+IP+1) = Y(I)
            C(LCTHE+IP+2) = Z(I)
   75   CONTINUE
        END IF
        LCMDL = LC(LCMDL)
      END DO                                ! end of loop on CMDL
  100 CONTINUE                              ! end of loop on MOD_TYPE
C----------------------------------------------------------------------
  999 RETURN
      END
