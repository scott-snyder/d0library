C====================================================================
      SUBROUTINE GTMHOT(IFLAG,ITRAK,NPTRAK,XHTRAK,YHTRAK,ZHTRAK,IHWADD)
C====================================================================
C
C  Description:  Gets X,Y,Z information of all hits on track for
C  ============  track # ITRAK. Either center of wire or including
C                all info
C
C  Argument Declarations:
C  ========================
C  IFLAG - INTEGER - Input: if 0 then just centers, if 1 full info
C  ITRAK - INTEGER - Input - Number of the track for which one would like info.
C  NPTRAK - INTEGER - Output - Number of points on track.
C  XHTRAK(NPTRAK) - X coordinates of hits on track
C  YHTRAK(NPTRAK) - Y coordinates of hits on track
C  ZHTRAK(NPTRAK) - Z coordinates of hits on track
C  IHWADD(NPTRAK) - Wire address of hit cell
C
C  Author:
C  =======
C  Tami Kramer
C
C  Revision History:
C  =================
C  Original Creation - February 1,1988
C  dh 5/88 change input arguements so either center or total info
C  dh 5/88 allow for hit only on one view; flag is -999999. if no hit
C  dh 11/88 ADD DIFFERENT ORIENTATIONS
C  tk 12/2  Add IHWADD to argument list
C  tk 12/5/88 Fix orientation 2
C  DH 9/89 CHANGE GTMUOT CALL
C  DH 12/89 CHANGE ORENT
C  DH 12/89 fix deltaT bug (found by KJ)
C  DH 4/90 add check if too many hits
C  SH 1/91 change for new MHOT 
C  DH 2/92 28 words/MUOH hit plus rotation corrections
C  AT 8/92 include chamber rotation
C=======================================================================
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
C  Argument Declarations:
C  ======================
C
      INTEGER ITRAK,NPTRAK,IFLAG,NT,NHT
      INTEGER SI
      PARAMETER (SI=40)
      INTEGER IHWADD(SI)
      REAL XHTRAK(SI),YHTRAK(SI),ZHTRAK(SI)
C
C  Local Declarations:
C  ===================
C
      INTEGER IFW1,IFW2,ORENT,QUAD,NTRAKS
      INTEGER IHT,IWADD,IHMUOH,TIMSIN,LMUOH,GZMUOH,IPAD,IDELT
      INTEGER NSAMUS,IFW3,ISPARE
      REAL XMAGC,YMAGC,ZMAGC,XCOSIM,YCOSIM,ZCOSIM,DRIFT,WIRE
      REAL XCOSOM,YCOSOM,ZCOSOM,CHSQBV,CHSNBV,MOM,MOMER,XI,YI,ZI
      REAL ELCAL,ELFE,SPARE1,SPARE2
      INTEGER NPL,NCL,NBUF,IBUF,NSPAR, NMOD
      CHARACTER*4 HSHAPE
      REAL  SPAR(3),XPAR(3),ROTM(3,3),VOFF
      REAL  DX,DY,DZ
      INTEGER WAVELEN
      INTEGER IER
C
      LOGICAL  FIRST
      INTEGER  KMGEH, GZMGEH
      REAL     WAVE
      INTEGER  RFLG
      DATA     FIRST/.TRUE./
C
C  Executable Code:
C  ================
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        KMGEH = GZMGEH(1)
        WAVE = C(KMGEH+16)*2.0
      END IF
C
      CALL GTMTRH(NTRAKS)
      IF (ITRAK .LE. NTRAKS) THEN
        CALL GTMUOT(ITRAK,NPTRAK,NSAMUS,QUAD,IFW1,IFW2,IFW3,ISPARE,
     X   XI,YI,ZI,XMAGC,
     X   YMAGC,ZMAGC,XCOSIM,YCOSIM,ZCOSIM,XCOSOM,YCOSOM,ZCOSOM,
     X            CHSQBV,CHSNBV,MOM,MOMER,
     X   ELCAL,ELFE,SPARE1,SPARE2)
        LMUOH = GZMUOH(0)
        IF(NPTRAK.GT.40) NPTRAK=40
        DO 40 IHT = 1,NPTRAK
          CALL GTMHTT(ITRAK,IHT,IWADD,IHMUOH,TIMSIN,IDELT,IPAD)
          NHT=LMUOH+28*(IHMUOH-1)
          XHTRAK(IHT) = Q(NHT+21)
          YHTRAK(IHT) = Q(NHT+22)
          ZHTRAK(IHT) = Q(NHT+23)
          IHWADD(IHT) = IWADD
          IF(IFLAG.EQ.1) THEN      ! put all info on track
CCCC
            ORENT=IABS(IQ(NHT+5))
CCCC     BEND VIEW
            IF(TIMSIN.NE.0) THEN
              NT=IABS(TIMSIN)
              DRIFT=TIMSIN/NT*Q(NHT+14+NT)
            ELSE
              DRIFT=-999999.
            ENDIF
CCCC        NONBEND VIEW ----- JUST DELTA TIME FOR NOW
C            IF(IDELT.NE.0) THEN
C              WIRE=Q(NHT+16+IDELT)
C            ELSE
C              WIRE=-999999.
C            ENDIF
CCCC       nonbend view include pad information
            IF(IDELT.NE.0) THEN
C              CALL MUADD( IWADD, NMOD, NPL, NCL,IER )
C              CALL MUCELL( NMOD ,NPL,NCL,HSHAPE,NSPAR,SPAR,XPAR,ROTM,
C     1                    VOFF,NBUF,IBUF )
              IF (IPAD.EQ.0) THEN
                WIRE=-999999.
              ELSE
                IF (IPAD.LT.0) THEN
                  WIRE = Q(NHT+19)
                ELSE
                  WIRE = Q(NHT+20)
                END IF
                WAVELEN = WAVE * (IABS(IPAD)-1)
                WIRE= WIRE + WAVELEN
              END IF
            ELSE
              WIRE=-999999.
            ENDIF
            IF(ORENT.EQ.1) THEN
              XHTRAK(IHT)=Q(NHT+21)
              YHTRAK(IHT)=Q(NHT+22)+WIRE
              ZHTRAK(IHT)=Q(NHT+23)+DRIFT
            ELSE IF(ORENT.EQ.2) THEN          ! Fix bug - tk
              XHTRAK(IHT)=Q(NHT+21)+WIRE
              YHTRAK(IHT)=Q(NHT+22)
              ZHTRAK(IHT)=Q(NHT+23)+DRIFT
            ELSE IF(ORENT.EQ.3) THEN
              XHTRAK(IHT)=Q(NHT+21)+DRIFT
              YHTRAK(IHT)=Q(NHT+22)+WIRE
              ZHTRAK(IHT)=Q(NHT+23)
            ELSE IF(ORENT.EQ.4) THEN
              XHTRAK(IHT)=Q(NHT+21)+WIRE
              YHTRAK(IHT)=Q(NHT+22)+DRIFT
              ZHTRAK(IHT)=Q(NHT+23)
            ENDIF
            CALL MUROTC( IHWADD(IHT), XHTRAK(IHT), YHTRAK(IHT),
     1                   ZHTRAK(IHT), DX,DY,DZ,IER )
              XHTRAK(IHT) = XHTRAK(IHT)+DX
              YHTRAK(IHT) = YHTRAK(IHT)+DY
              ZHTRAK(IHT) = ZHTRAK(IHT)+DZ
          ENDIF
   40   CONTINUE
      ENDIF
      RETURN
      END
