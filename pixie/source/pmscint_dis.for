      SUBROUTINE PMSCINT_DIS(IVIEW)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draws Muon Scintillator in view IVIEW
C-
C-   Inputs  : gets IVIEW
C-   Outputs :
C-   Controls:
C-
C-   Created  15-OCT-1993  V.Bhatnagar
C-   Modified 11-FEB-1994  V.Bhatnagar
C-    Looking at MSCT data bank now
C-   Modified 25-FEB-1994  V.Bhatnagar
C-    Displays Non Hit Scints. also
C-   Modified 31-MAR-1994  V.Bhatnagar
C-    Now displays Hit scints directly calling GTMSCT (no need of MUSCNT/STP)
C-   Modified 26-MAY-1994 Vipin Bhatnagar
C-    Cleanup of code
C---------------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER IVIEW,NMOD,IH,IER,EZERROR
      INTEGER NMUMOD,NSPAR,NBUF,IBUF(20)
      INTEGER MUMOD_ID(20),NSCI_MUMOD(20)
      INTEGER MMOD,NHTSCINT,NSCI,NMODUL,NSCINT
      INTEGER I,J,IFLAG,IMOUT,IHT,SCINT_ID
      INTEGER IMARK,NMSCT,OLDMODUN
      INTEGER IADD,MUSCNT_ADDR,HTMSCNT(10),SADD(10)

      REAL SPAR(3),XPAR(3),ROTM(3,3),BUF(20),TPOSX
      REAL TOF,SPARM(3),TXYZ(3),XYZ(3),DXYZ(3),TPOSY
      REAL MODXC,MODYC,MODZC,MODXD,MODYD,MODZD,TPOSZ

      LOGICAL SCINT_DISPLAY
      CHARACTER*4 HSHAPE
      CHARACTER*3 HTCOLR
C----------------------------------------------------------------------
      EQUIVALENCE (IBUF(1),BUF(1))
C----------------------------------------------------------------------
      DATA HTCOLR/'RED'/
      DATA IMARK/2/                     ! 3 = *,2 = +
      DATA OLDMODUN/0/
C----------------------------------------------------------------------
C
C---- Get # of Muon Modu.,Muon Modu.Id and # of Scints for that Modu.
C
      CALL GTMSGH(NMUMOD,MUMOD_ID,NSCI_MUMOD)
      IF (NMUMOD.EQ.0) THEN
        CALL ERRMSG('ERROR IN GTMSGH','PMSCINT_DIS',' ','S')
        CALL PUOPEN
        GOTO 997
      ENDIF
C
      CALL PUOPEN
C
      CALL VZERO(SADD,10)
      NHTSCINT = 0
      IHT      = 0
C
C---Looking into MUHT bank and finding out # of MSCTs,addresses
C
      CALL PMSCINT_ADD(NHTSCINT,SADD,IHT)
C
C----Drawing non-hit scints. only dropping hit ones-----
C
      CALL EZPICK('PX_MUODIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PMSCINT_DIS',
     &    'Bank PX_MUODIS_RCP NOT FOUND','W')
        GOTO 998
      ENDIF
      CALL PUGETV('SCINT DISPLAY',SCINT_DISPLAY)
      CALL EZRSET
      IF (.NOT.SCINT_DISPLAY) THEN
        GOTO 997
      ENDIF
C
C---Get the No.of scints hit dropped now from being displayed
C
      DO 210 NMODUL = 1,NMUMOD
        DO 220 NSCINT = 1,8
          SCINT_ID  = NSCINT*1000 + MUMOD_ID(NMODUL)
          DO 230 IH = 1,IHT
            CALL MNADDC(SADD(IH),NMOD,NSCI)
            HTMSCNT(IH) = NMOD + NSCI*1000
            IF ( HTMSCNT(IH).NE.SCINT_ID) THEN
              CALL PXCOLR('YEL')
              CALL MUSCNT(SCINT_ID,HSHAPE,NSPAR,SPAR,XPAR,ROTM,NBUF,
     &          IBUF)
              DO 625 I = 1,3
                SPARM(I) = 0.
                DO 627 J = 1,3
                  SPARM(I) = SPARM(I) + SPAR(J)*ROTM(I,J)
  627           CONTINUE
  625         CONTINUE
              MODXC = (XPAR(1))
              MODYC = (XPAR(2))
              MODZC = (XPAR(3))
              MODXD = ABS(SPARM(1))
              MODYD = ABS(SPARM(2))
              MODZD = ABS(SPARM(3))
C
              IF (IVIEW .EQ. 1) THEN
                CALL PXBOX(MODZC,MODYC,MODXC,MODZD,MODYD,MODXD)
              ELSEIF (IVIEW .EQ. 2) THEN
                CALL PXBOX(MODXC,MODYC,MODZC,MODXD,MODYD,MODZD)
              ELSEIF (IVIEW .EQ. 3) THEN
                CALL PXBOX(MODZC,MODXC,MODYC,MODZD,MODXD,MODYD)
              ENDIF
            ENDIF
  230     CONTINUE
  220   CONTINUE
  210 CONTINUE
C
  997 CONTINUE

C
C----Get the Id # of Scints Hits
C
      NMSCT = 1
  100 CONTINUE
C
C----Look into data bank for hit scints
C
      CALL GTMSCT(NMSCT,IADD,IFLAG,IMOUT,TOF,TXYZ,XYZ,DXYZ)
      IF ( IADD.EQ.0 ) THEN
        IF ( NMSCT.EQ.1 .AND. NHTSCINT.EQ.1 ) THEN
          NMSCT = 0
          GOTO 998
        ELSEIF ( NMSCT.EQ.NHTSCINT) THEN
          NMSCT = 0
          GOTO 998
        ENDIF
      ELSE
        TPOSX   = TXYZ(1)
        TPOSY   = TXYZ(2)
        TPOSZ   = TXYZ(3)
        XPAR(1) = XYZ(1)
        XPAR(2) = XYZ(2)
        XPAR(3) = XYZ(3)
        SPARM(1)= DXYZ(1)
        SPARM(2)= DXYZ(2)
        SPARM(3)= DXYZ(3)
        CALL MNADDC(IADD,NMOD,NSCI)
        MUSCNT_ADDR = NMOD + NSCI*1000
C
C----Drawing the Hit scints now---------
C
        CALL PXCOLR('MAG')
        MODXC = (XPAR(1))
        MODYC = (XPAR(2))
        MODZC = (XPAR(3))
        MODXD = ABS(SPARM(1))
        MODYD = ABS(SPARM(2))
        MODZD = ABS(SPARM(3))
C
C---Same Muon Modu. types only Scint.# (same event but different view)
C
        MMOD = NMOD
        IF (MMOD.EQ.OLDMODUN) MMOD = NSCI
C
        IF (IVIEW .EQ. 1) THEN
          CALL PXBOX(MODZC,MODYC,MODXC,MODZD,MODYD,MODXD)
          IF ( IMOUT .NE. 0 ) THEN
            CALL PXMARK(HTCOLR,IMARK,TPOSZ,TPOSY,TPOSX)
          ENDIF
          CALL PMCHNM(MMOD,MODZC,MODYC,MODXC,MODZD,MODYD,MODXD)
        ELSEIF (IVIEW .EQ. 2) THEN
          CALL PXBOX(MODXC,MODYC,MODZC,MODXD,MODYD,MODZD)
          IF ( IMOUT .NE. 0 ) THEN
            CALL PXMARK(HTCOLR,IMARK,TPOSX,TPOSY,TPOSZ)
          ENDIF
          CALL PMCHNM(MMOD,MODXC,MODYC,MODZC,MODXD,MODYD,MODZD)
        ELSEIF (IVIEW .EQ. 3) THEN
          CALL PXBOX(MODZC,MODXC,MODYC,MODZD,MODXD,MODYD)
          IF ( IMOUT .NE. 0 ) THEN
            CALL PXMARK(HTCOLR,IMARK,TPOSZ,TPOSX,TPOSY)
          ENDIF
          CALL PMCHNM(MMOD,MODZC,MODXC,MODYC,MODZD,MODXD,MODYD)
        ENDIF
        OLDMODUN = MMOD
        IF (NMSCT.EQ.NHTSCINT) GOTO 998
        NMSCT = NMSCT + 1
        GOTO 100
      ENDIF
C
  998 CONTINUE
C
      CALL JRCLOS

  999 RETURN

      END
