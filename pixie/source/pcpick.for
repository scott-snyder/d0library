      SUBROUTINE PCPICK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Allows user to select a cal cell in 3D view
C-                         Note that this version does not attempt to
C-                         handle EM3 or ECOH correctly.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  25-JAN-1989   Michael W. Peters
C-   UPDATED  10-Jul-1990   Shahriar Abachi  : Modified for rotations.
C-   UPDATED  12-AUG-1990   Shahriar Abachi  : jescap with icode FLAG_3D_MATRIX
C-                                             must now specify segment number.
C-   UPDATED  07-NOV-1990   Shahriar Abachi  : Eugetm and emulmv were hidden in
C-                                             escape functions.
C-   UPDATED  23-NOV-1990   Shahriar Abachi  : Corrected in case of left handed
C-                                             coordinates.
C-   UPDATED  26-DEC-1990   Nobuaki Oshima   : Modify for RCP version
C-                                             + New PULOC3.
C-   UPDATED  03-MAR-1991   Nobuaki Oshima   : Draw a picked cell by temporary
C-                                             segmeny.
C-   Updated   2-JUN-1991   Harrison B. Prosper
C-      Fix call to PUTEXT
C-   UPDATED  18-JUL-1991   Nobuaki Oshima   : Corrects E&S part.
C-   UPDATED  11-OCT-1991   Nobuaki Oshima   : change the format of picked
C-                                            cells listing.
C-   UPDATED  18-JAN-1992   Nobuaki Oshima
C-           Add 'GET_3D_MATRIX' for E&S picking.
C-   UPDATED  24-JUL-1992   Nobuaki Oshima : Clean up E&S part.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PXPARA.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:ESCAPE_CODE_NAMES.DEF'
C
      INTEGER NCLMAX
      PARAMETER (NCLMAX=100)
      INTEGER IDX,IER,ID
      INTEGER NS,IERR,IVAL
      INTEGER ICHOIC,ICHAR,NCELL,ARGSOK,I,J
      INTEGER IETAC(NCLMAX),IPHIC(NCLMAX),LAYERC(NCLMAX),DPHI
      INTEGER GZCAEP,LDCAEP
      INTEGER LCAEP,NRP,NCH
      REAL VR(3),VN(3),XP(3),VC(3),XC(3),VLEN,EMIN,ECELL
      REAL XL(8),YL(8),ZL(8),ENERGY,ET
      REAL XS(2,20),YS(2,20),ZS(2,20)
      LOGICAL OK,CEXIST,EZERROR
C
      CHARACTER*30 LINE(4)
      CHARACTER*40 STR
C-
      REAL    RIGHT, LEFT
C-
      DATA LINE(2)/' Your picked cell is:'/
      DATA LINE(3)/' Eta Phi Lay E(GeV)'/
C-----------------------------------------------------------------
C-
C--- Get window coordinates of picked point
C-
      CALL PU_GET_PICKW(XP)
C-
C=== GET THE CURRENT VIEWPLANE NORMAL VECTOR
C-
      CALL J3RGET(8,VN(1),VN(2),VN(3))
C-
      VLEN=0.
      DO I=1,3
        XC(I) = XP(I) - VN(I)
        VLEN  = VLEN + VN(I)**2
      ENDDO
      VLEN = SQRT(VLEN)
      DO I=1,3
        VC(I) = VN(I)/VLEN
      ENDDO
C-
      RIGHT = -1.0
      CALL J1IGET(43, IVAL)
      IF(IVAL .EQ. 0) RIGHT = 1.0
      LEFT = -1.0 * RIGHT
      XC(3) = XC(3) * LEFT
      VC(3) = VC(3) * LEFT
C-
C=== GET LIST OF CELLS ALONG THE PICK LINE ====================
C-
      CALL CLINPH(XC,VC,NCLMAX,NCELL,IETAC,IPHIC,LAYERC,ARGSOK)
      I=0
      IF(ARGSOK.EQ.0.AND.NCELL.GT.0) THEN
        IF(GZCAEP().LE.0) THEN
          CALL INTMSG(' CAEP BANK DOES NOT EXIST')
          GO TO 995
        ENDIF
        CALL PUGETV('CAL EMIN',EMIN)
C-
C--- LOOP OVER CELLS ON THE PICK LINE
C-
        DO 100 I=1,NCELL
C--- check for valid layer,eta, phi
C-
          IF(LAYERC(I).LT.MNLYEM.OR.LAYERC(I).GT.MXLYCH)GO TO 100
          IF(IPHIC(I).LT.1.OR.IPHIC(I).GT.NPHIL)GO TO 100
          IF(IETAC(I).LT.-NETAL.OR.IETAC(I).GT.NETAL)GO TO 100
C--- CHECK IF CELL EXISTS
C-
          IF(.NOT.CEXIST(IETAC(I),IPHIC(I),LAYERC(I)))GO TO 100
          CALL CELL_ENERGY(IETAC(I),IPHIC(I),LAYERC(I),ENERGY,ET)
          IF(ENERGY.GE.EMIN) GO TO 200
  100   CONTINUE
        LINE(1) = ' % PCPICK - Try again. '
        CALL INTMSG(LINE(1))
        GO TO 995
  200   CONTINUE
C-
C--- Draw picked cell
C.N.O.        CALL CELVEC(IETAC(I),IPHIC(I),LAYERC(I),XS,YS,ZS,NS,IERR)
C.N.O.        IF(NS.GT.2.AND.NS.LE.20)GO TO 330
C.N.O.        WRITE(STR,325)NS
C.N.O.  325   FORMAT(' PCPICK - BAD CELVEC RETURN : NS=',I6)
C.N.O.        CALL PUMESS(STR)
C.N.O.        GO TO 995
C.N.O.  330   IF(IERR.EQ.0)THEN
C.N.O.          CALL JOPEN
C.N.O.          CALL PXCOLR('FOR')  ! Foreground color
C.N.O.          CALL PLTVEC(XS,YS,ZS,NS)
C.N.O.          CALL JCLOSE
C.N.O.        ENDIF
C-
C--- TELL USER ABOUT THE PICKED CELL
C--- ( INTMSG/OUTMSG cannot be before draw picked cell(?)
C-
        LINE(1) = ' '
        WRITE(LINE(4),300) IETAC(I),IPHIC(I),LAYERC(I),ENERGY
  300   FORMAT(3I4,F7.2)
C
C.N.O.        CALL PUTEXT_CREATE(ID)
C.N.O.        CALL PUTEXT(ID,LINE(3),2)
C.N.O.        CALL PUTEXT_DELETE(ID)
        DO I=1,4
          CALL INTMSG(LINE(I))
        ENDDO
      ENDIF
  995 CONTINUE
  999 RETURN
      END
