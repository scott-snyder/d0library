      SUBROUTINE MSPAD(IPLANE,ICL1,HITS,NINDX,INDX,IPAD)
C----------------------------------------------------------------------
C-    MSPAD  - GET A WIDTH OF COPPER AT           
C.             HIT POINTS AND GIVE PULSE HEIGHTS
C.
C.  Input: IPLANE    - Plane Number
C.         ICL1      - Wire Number + 1
C.         HITS(1,j) - Module Number of j-th hit
C.         HITS(2,j) - Plane Number of j-th hit(unused)
C.         HITS(3,j) - Wire Number of j-th hit(unused)
C.         HITS(4,j) - Drift time + the time of flight from original
C.                     Vertex of j-th hit(ns)(unused)
C.         HITS(5,j) - Distance of j-th hit from the Electro-end(cm)
C.         HITS(6,j) - Distance of j-th hit from the Far-end(cm)(unused)
C.       NINDX(m,n)  - Number of hits at m-th cell in n-th plane
C.       INDX(k,m,n) - Pointer to k-th hit at cell'm' and plane'n'
C.
C. Output: IPAD(1,k) - Pulse height of the center PAD in k-th Cell
C.         IPAD(2,k) - Pulse height of the side PAD in k-th Cell
C.                                             
C. N.Oshima   08-Apr-87     V1.00
c. S.Kunori   05-Dec-89     fix a bug, denominator, WIDTH1+WIDTH2=0.
c. S.Igarashi 10-Apr-91     For digitization
C----------------------------------------------------------------------
      IMPLICIT NONE
C-                                                    
      INCLUDE 'D0$INC:GCFLAG.INC/LIST'
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
      INCLUDE 'D0$LINKS:IZSTPO.LINK/LIST'
      INCLUDE 'D0$LINKS:IZSTPC.LINK/LIST'
      INCLUDE 'D0$LINKS:IZSMUO.LINK/LIST'
      INCLUDE 'D0$LINKS:IZMGEH.LINK/LIST'
C- 
      INTEGER LSTPC,LMGEO,IZMGEO
      INTEGER NHDIM,NHMAX
      PARAMETER (NHDIM= 9)
      PARAMETER (NHMAX=40)
      INTEGER IPLANE,ICL1,NINDX(24,4),INDX(NHMAX,24,4),IPAD(2,2)
      INTEGER NPATT,IOE,J,IPULS1,IPULS2,IHIT,NMHITS,NCELL,NUMCEL
      REAL    HITS(NHDIM,NHMAX),XDIS,REFPT
      REAL    COEFF,WIDC0,WIDS0,WIDC1,WIDS1,XFRAC
      REAL    PATLN,PATWD,YCMIN,YCMAX,YSMIN,YSMAX,CSPACE
      REAL    MAXDIG
      INTEGER INT,MOD
      INTEGER L,IP
      INTEGER I1,I2
      REAL    AMOD
      REAL    CRATIO,CSUM
      REAL    MSPADR,MSPADS
C-
      DATA MAXDIG /4096./      ! For 12 bits ADC
C
********************************************
*     Big Branch to old version,  V1.      *
********************************************
      IF(SMUO(2).LT.1.5) THEN
         CALL MSPAD_V1(IPLANE,ICL1,HITS,NINDX,INDX,IPAD)
         RETURN
      ENDIF
C-
      LSTPC = LC (LSTPH-IZSTPC)
      LSMUO = LC (LSTPC-IZSMUO)
        IF (LSMUO .EQ. 0)        GO TO 999
      LMGEH = LC (LSMUO-IZMGEH)
        IF (LMGEH .EQ. 0)        GO TO 999
      PATLN  = C (LMGEH+16)
      PATWD  = C (LMGEH+17)
      YCMIN  = C (LMGEH+18)
      YCMAX  = C (LMGEH+19)
      CSPACE = C (LMGEH+20)
C-
C---  Calculate copper widths and coefficient
      WIDC0 = YCMIN
      WIDS0 = PATWD - YCMIN - CSPACE ! YSMAX
      WIDC1 = YCMAX
      WIDS1 = PATWD - YCMAX - CSPACE ! YSMIN
C-      
      COEFF = (WIDC1-WIDC0)/PATLN
C--   Init. Buffer
      NUMCEL = 0
      CALL VZERO(IPAD(1,1),4)
      NCELL = ICL1
  100 CONTINUE
      NUMCEL = NUMCEL + 1
      NMHITS = NINDX(NCELL,IPLANE)
      IF (NMHITS .EQ. 0)                    GO TO 800
C-
C---  Loop over for each Hits in a Cell  
C-
      DO 500 IHIT=1,NMHITS
      J = INDX(IHIT,NCELL,IPLANE)
      IZMGEO = HITS(1,J)
      LMGEO  = LC (LMGEH-IZMGEO)
      REFPT  =  C (LMGEO+42+IPLANE)
      XDIS   =  HITS(5,J)
C---  Calculate a Fraction of PAD
C-
      IF (XDIS .LT. REFPT) THEN
        IF(REFPT.GT.(PATLN+XDIS))THEN
          XFRAC = 2.*PATLN - REFPT + XDIS
          IOE   = 0
        ELSE
          XFRAC = PATLN - REFPT + XDIS
          IOE   = 1
        ENDIF
      ELSE
        XDIS = XDIS - REFPT
        NPATT = INT (XDIS/PATLN)
        XFRAC = AMOD (XDIS,PATLN)
        IOE   = MOD (NPATT,2)
      ENDIF
C---  Calculate charge ratio
C-
       IF (IOE .EQ. 0) THEN
         CRATIO = MSPADR(XFRAC)
       ELSE
         CRATIO = MSPADR(PATLN-XFRAC)
       ENDIF
C---  Calculate charge sum
C-
       CSUM = MSPADS(HITS(4,J),HITS(7,J),HITS(8,J))
C---  Get a Pulse Height
C-
      IPULS1 = NINT((1.-CRATIO)*CSUM/12.*MAXDIG)
      IPULS2 = NINT((1.+CRATIO)*CSUM/12.*MAXDIG)
C---  Sum up Pulse Heights for each PAD & Cell
C-
      IPAD(1,NUMCEL) = IPAD(1,NUMCEL) + IPULS1
      IPAD(2,NUMCEL) = IPAD(2,NUMCEL) + IPULS2
  500 CONTINUE
C-
C---  End Loop then go to a Ganged Cell if not.
C-
  800 CONTINUE
      IF (NCELL .NE. ICL1)  GO TO 900
        NCELL = NCELL + 1
        GO TO 100
  900 CONTINUE

      DO 910 I1=1,2
        DO 910 I2=1,2
          IF(IPAD(I1,I2).GT.NINT(MAXDIG)) IPAD(I1,I2)=NINT(MAXDIG)
  910 CONTINUE
C-
C-----------debug------------------------------------------------
      IF(IDEBUG.EQ.1.AND.DDIG.EQ.1.AND.PMUO.GE.2) THEN
         WRITE (LOUT,2000) IPLANE
 2000 FORMAT(' ### MSPAD Plane Number:',I2,3X,'IPAD1',5X,'IPAD2')
         NUMCEL = ICL1
         DO 600 IP=1,2
         WRITE (LOUT,2001) NUMCEL,(IPAD(L,IP), L=1,2)
 2001    FORMAT(1X,6X,'Cell Number: ',I4,4I10)
         NUMCEL = NUMCEL + 1
  600    CONTINUE
      ENDIF
C----------------end debug--------------------------------------   
      RETURN
  999 CONTINUE
      WRITE (LOUT,2002)
 2002 FORMAT('  No MUON Constants Banks !!!'
     +/'     ***** PROGRAM STOPPED IN S/R MSPAD *****')
      STOP 1
      END
