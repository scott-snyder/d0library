      SUBROUTINE FDCISA
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Builds the bank FITR which contains the ISAJET
C-   tracks in a fiducial volume inside the FDC
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Modified  20-FEB-1989   Jeffrey Bantly  from GRC CDCISA.FOR for FDC use 
C-   Updated  20-MAR-1990   Jeffrey Bantly  general cleanup 
C-   Updated  26-APR-1991   Jeffrey Bantly  make use of FDC.RCP 
C-   Updated  26-JUN-1991   Robert E. Avery  remove lvldbg parameter 
C-   Updated  14-AUG-1991   Tom Trippe  add ICALL = 1 for repeat calls
C-   Updated  25-NOV-1991   Robert E. Avery  Only do once per event,
C-                              and add check of ISAE bank existence.
C-   Updated  12-DEC-1991   Robert E. Avery  Change radius cuts to 
C-                              correspond to the active gas volume.
C-   Updated  28-FEB-1992   Robert E. Avery  Add option (default) to use
C-      realistic chamber fiducial cuts (rather than the simple disk 
C-      that had been used previuosly). Also, add option to call 
C-      FDC_MARK, which matches FDC tracks (if they exist) to 
C-      FITR tracks, filling the apropriate word in the FITR bank.
C-      Also, a debug option that prints out information for ALL 
C-      ISAE information.
C-   Updated  20-MAR-1992   Robert E. Avery  Get FGEH with explicit
C-      GZFGEH call (in case it has been zeroed). 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:FDLTRK.INC'
C
      INTEGER NISA, LFDPH, HALF,LRCP
      INTEGER PRUNIT, IER, ICALL
      INTEGER GZFDPH,GZFGEH,USUNIT
      INTEGER LISAE, GZISAE
      INTEGER RUN,ID
      INTEGER RUNSAV,IDSAV
C
      REAL RMIN, ZMIN, RMAX, ZMAX
      REAL DIR
C
      LOGICAL REALISTIC_FIDU 
      LOGICAL DO_FISA_MATCH 
      LOGICAL DBG_FDCISA
      LOGICAL DBG_ISAE_ALL
C
      SAVE ICALL,PRUNIT,DBG_FDCISA,DBG_ISAE_ALL
      SAVE REALISTIC_FIDU, DO_FISA_MATCH 
      DATA ICALL /0/
      DATA REALISTIC_FIDU /.TRUE./
C----------------------------------------------------------------------
      LISAE = GZISAE()
      IF ( LISAE .LE. 0 ) GOTO 999
C
      IF(ICALL .EQ. 0) THEN
        ICALL = 1
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('DBG_FDCISA',DBG_FDCISA,IER)
        CALL EZGET('DBG_ISAE_ALL',DBG_ISAE_ALL,IER)
        CALL EZGET('FISA_REALISTIC',REALISTIC_FIDU,IER)
        CALL EZGET('FISA_MATCH ',DO_FISA_MATCH ,IER)
        CALL EZRSET
        PRUNIT=USUNIT()
        IF ( LSFDC .LE. 0 ) CALL FGEOM_INIT()
      ENDIF
C
C  Only one call per event.
C
      CALL EVNTID(RUN,ID)
      IF (RUN .NE. RUNSAV .OR. ID .NE. IDSAV) THEN
        RUNSAV = RUN
        IDSAV = ID
      ELSE
        GOTO 999
      ENDIF
C
C ****  Creates a fiducial volume in one half of the FDC at a time
C
      DO 10 HALF = 0,1
        IF ( REALISTIC_FIDU ) THEN
          CALL FISA_REAL(HALF, NISA)
        ELSE
C
C ****  Find the limits of the FDC RMIN, ZMIN, RMAX, ZMAX in the geometry
C ****  banks
C
          LFGEH = GZFGEH()
          IF ( LFGEH .LE. 0 ) CALL ERRMSG('Bank not booked','FDCISA',
     &            ' **** FDCISA: Geometry FGEH not defined','F')
          LFDPH = GZFDPH()
          IF ( LFDPH .LE. 0 ) CALL ERRMSG('Bank not booked','FDCISA',
     &            ' **** FDCISA: Geometry FDPH not defined','F')
C
          DIR = FLOAT((-1)**(HALF+1))
          RMIN = C(LFDPH+69+2)
          RMAX = C(LFDPH+69+3)
C
C  Z limits taken as aproximately the middle of each Theta layer:          
C
          ZMIN = DIR * ( C(LFGEH+8) - C(LFDPH+69+4) - 3.0 )
          ZMAX = DIR * ( C(LFGEH+8) + C(LFDPH+69+4) + 3.0 )
          CALL FDISTR ( ZMIN, RMIN, ZMAX, RMAX, NISA )
        ENDIF
   10 CONTINUE
C
C ****  Clean up FITR bank
C
      CALL PUFITR
C
C ****  Match to FDC tracks (optionally)
C
      IF ( DO_FISA_MATCH ) THEN
        CALL FISA_MARK
      ENDIF
C
C ****  Print the ISAJET bank if asked
C
      IF ( PRUNIT.GT.0 ) THEN
        IF ( DBG_ISAE_ALL ) THEN
          CALL PRISAE(PRUNIT,0, 0, ' ', 0 )
          CALL PRISV1(PRUNIT,0,0,'ALL',0)
          CALL PRISP1(PRUNIT,0,0,'ALL',0)
          CALL PRISV2(PRUNIT,0,0,'ALL',0)
          CALL PRISP2(PRUNIT,0,0,'ALL',0)
          CALL PRISP3(PRUNIT,0,0,'ALL',0)
        ENDIF
        IF ( DBG_FDCISA ) THEN
          CALL PRFITR( PRUNIT, LFITR, 0, ' ', 0 )
        ENDIF
      ENDIF
C
C--------------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
