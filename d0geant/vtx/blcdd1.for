      SUBROUTINE BLCDD1
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Build the bank CDD1 containing the raw FADC data
Command: get file BLCDD1.FOR
C-
C-
C-   Inputs  : The banks VWDA and VZDA are needed to build CDD1
C-   Outputs : none; The bank CDD1 is filled
C-
C-   Created  4-Oct-1988  Tom Trippe
C-   Modified 07-DEC-1988 Peter Grudberg - added z-strips
C-   Updated  24-MAY-1989 Peter Grudberg - fixed bug in z-strips
C-   Updated  14-JUN-1989 Peter Grudberg changed arguments to FICDD1
C-   Updated  26-OCT-1989   Jeffrey Bantly - add Crate Trailer words
C-   Updated  30-JAN-1990 Peter Grudberg - IXMAIN => IXCOM in MZPUSH call
C-   Updated  14-MAR-1990 Peter Grudberg - avoid overwrites by trailer words
C-   Updated   5-MAY-1992 Alexandre Zinchenko - modified to simulate new
C-                        pulse shapes 
C-   Updated  16-JUN-1992 Liang_ping Chen - include incl. file VTLOCA.INC
C-   Updated  16-JUN-1992   K. Wyatt Merritt  Remove references to z strips,
C-                        since preamps and cables for them have already
C-                        been removed (Ref. T. Trippe, private communication) 
C-   Updated   9-SEP-1992 Alexandre Zinchenko - fixed stupid bug (removed 
C-                        comment in front of line with IFADCC)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:GCUNIT.INC'
      INCLUDE 'D0$LINKS:IZVRFT.LINK'
      INCLUDE 'D0$INC:VTXLNK.INC'
      INCLUDE 'D0$INC:D0LOG.INC' 
      INCLUDE 'D0$INC:VTXPSH.INC' 
      INCLUDE 'D0$INC:VTLOCA.INC' 
C
C
      INTEGER ILYR, ISEC, NBFADC, IPTH(0:20), NBHITS(0:20), LHIT
      INTEGER NWORDS, NWDAT, LVRFT, NVXLYR, NVXSEC
      INTEGER IHIT, IFADC, IPTR, IAD, IADC(3)
      INTEGER IVER, ISKP, IFL, IPTR1
      REAL TIME(100), AREA(100), WIDTH(100), COORD(100)
C----------------------------------------------------------------------
      LVRFT = LC ( LVGEH - IZVRFT )
      IF ( LVRFT .LE. 0 ) THEN
        WRITE (LOUT, *)
     &     '**** BLCDD1: Geometry bank VRFT doesn''t exist'
        GO TO 999
      ENDIF
      NVXLYR = IC ( LVRFT + 1 )
      DO 100 ILYR = 0, NVXLYR - 1
        LAYER=ILYR
        IF ( LVLAY ( ILYR ) .LE. 0 ) GO TO 100
        NVXSEC = IC ( LVRFT + ILYR*7 + 2 )
        DO 200 ISEC = 0, NVXSEC - 1
          SECTOR=ISEC 
C-             NBADC             = number of FADC in this cell
C-             IPTH  (0:NBADC-1) = pointer on first hit on this FADC
C-             NBHITS(0:NBADC-1) = number of hits on this FADC
C-             LHIT              = number of words per hits. 0 if no hits
C-
          CALL ZGVWDA ( ILYR, ISEC, NBFADC, IPTH, NBHITS, LHIT)
          IF ( LHIT .LE. 0 ) GO TO 200
          DO 300 IFADC = 0, NBFADC - 1
c            WIRE=INT(IFADC/2)                       
            IFADCC = IFADC 
            IPTR = IPTH ( IFADC ) + LVWDA(ISEC, ILYR)
            IF(SVTX(6).EQ.1.) IPTR1=IPTH(IFADC+1)+LVWDA(ISEC,ILYR) 
            IAD = IQ ( IPTR + 1 )
            IADC ( 1 ) = ILYR
            IADC ( 2 ) = ISEC
            IADC ( 3 ) = IBITS ( IAD, 0, 4 )
            DO 400 IHIT = 1, NBHITS ( IFADC )
              TIME ( IHIT ) = Q ( IPTR + 2 )
              AREA(IHIT ) = Q ( IPTR + 3 )
              IF(SVTX(6).EQ.1.) THEN 
                IF(MOD(IFADC+1,2).NE.0) THEN 
                  NMEAN(IHIT) = AREA(IHIT)/10000. + 0.5 
                  AREA(IHIT) = AREA(IHIT) - NMEAN(IHIT)*10000. 
                  AREA1(IHIT) = AREA(IHIT) + Q(IPTR1+3) 
                ENDIF
                WIDTH(IHIT ) = Q ( IPTR + 5 )   ! track length
                COORD(IHIT) = Q ( IPTR + 4) ! drift distance 
              ENDIF 
              IPTR = IPTR + LHIT
              IPTR1 = IPTR1 + LHIT 
  400       CONTINUE
            CALL FICDD1 (IADC,TIME,AREA,WIDTH,COORD,NBHITS(IFADC),
     &                   NWDAT) 
  300     CONTINUE
  200   CONTINUE
  100 CONTINUE
C
C ****  Insert CDD1 Crate Trailer words and release empty space when the bank
C ****  is filled (For version 2, 4 trailer words are added.  It may be
C ****  necessary to extend the bnk to fit these words.)
C
  800 CONTINUE 
      IF ( LCDD1 .GT. 0 ) THEN
        IVER =2
        ISKP =NWDAT
        IFL  =2
        NWORDS = NWDAT + 4 - IQ( LCDD1 - 1 )
C
C ****  Make the bank just the right size to fit  the trailer words
C
        CALL MZPUSH( IXCOM, LCDD1, 0, NWORDS, 'R' )
        CALL CDD1FL(IVER,ISKP,IFL)
      ENDIF
  999 RETURN
      END
    
