      SUBROUTINE GTFTHT( HALF, QUAD, SECTOR, WIRE, Z, PK, RT, TZ, NZ)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get all the hits from a single channel
C-
C-   Inputs  : HALF,QUAD,SECTOR,WIRE
C-   Outputs : Z  = drift times converted to bins in the FADC
C-             PK = pulse area in FADC bins
C-             RT = risetime
C-             TZ = marker type (2=hit found(+),5=on segment(x))
C-             NZ = number of hits found
C-
C-   Created  20-APR-1990   Jeffrey Bantly
C-   Updated   8-NOV-1990   Jeffrey Bantly  improve method 
C-   Updated  29-APR-1991   Jeffrey Bantly  add PK to output,use new RCP,PARAMS 
C-   Updated   4-NOV-1991   Robert E. Avery  VAX intrinsice functions fix
C-   Updated  14-FEB-1992   Susan K. Blessing  Remove machine block. 
C-   Updated  12-MAY-1992   Susan K. Blessing  Declare PEDS (removed from
C-    FDEVNT.INC).
C-   Updated  25-MAY-1992   Robert E. Avery  Remove reference to SHIFTT. 
C-   Updated  29-JUNE-1992  Tacy M. Joffe-Minor  NSEG constrained by
C-                                               HITS_PER_WIRE
C-   Updated  10-AUG-1992   Susan K. Blessing  Move EZGET inside IF ICALL
C-    and call EZPICK.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INCLUDE 'D0$INC:FDEVNT.INC'
C
      INTEGER HALF,QUAD,SECTOR,WIRE,ICALL,IER
      INTEGER TZ(MX_HIT_WIRE), NZ, IHIT, LOC, LOC1, IH
      INTEGER SEGNUM,STATWORD,NSEG,ISEG,SEGMARK(MX_HIT_WIRE)
      INTEGER PREV_WIRE
      INTEGER LKFTSC,LKFTDA
      INTEGER GZFTSC,GZFTDA
      INTEGER HITS_PER_WIRE
C
      REAL    Z(MX_HIT_WIRE), DRIFTT(MX_HIT_WIRE)
      REAL    PK(MX_HIT_WIRE), RT(MX_HIT_WIRE)
      REAL    ETZERO,ATZERO
      REAL    VELOP,VELOM
      REAL    GAIN,MIPCONV
      REAL    PEDS(2)
C
      DATA ICALL/ 0/
C----------------------------------------------------------------------
C
      IF( ICALL.LE.0) THEN
        PREV_WIRE=20
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('HITS_PER_WIRE',HITS_PER_WIRE,IER)
        CALL EZRSET
        ICALL = 1
      ENDIF
C
      CALL VZERO(Z,MX_HIT_WIRE)
      CALL VZERO(PK,MX_HIT_WIRE)
      CALL VZERO(RT,MX_HIT_WIRE)
      CALL VZERO(TZ,MX_HIT_WIRE)
      NZ = 0
      IF(WIRE.LT.PREV_WIRE) THEN
        NSEG=0
        CALL VFILL(SEGMARK,MX_HIT_WIRE,-1)
      ENDIF
      PREV_WIRE=WIRE
C
      LKFTDA=GZFTDA(HALF,QUAD,SECTOR)
      IF( LKFTDA.LE.0 ) GOTO 999
C
      CALL FGTLPD(HALF,0,QUAD,SECTOR,WIRE,PEDS(1),PEDS(2))
      CALL FGTLTM(HALF,0,QUAD,SECTOR,WIRE,ETZERO,ATZERO,
     &                                 VELOP,VELOM)
      CALL FGTLGN(HALF,0,QUAD,SECTOR,WIRE,GAIN,MIPCONV)
      IF ( GAIN .LE.0 ) GAIN = 1.0
      IF(WIRE.LT.NBTSEN) THEN
        LKFTSC=GZFTSC(HALF,QUAD,SECTOR)
        IF( LKFTSC.LE.0 ) GOTO 999
        NZ = IQ(LKFTSC+4+WIRE)
        IF( NZ.LE.0 ) GOTO 999
        DO 10 IHIT=1,MIN0(NZ,HITS_PER_WIRE)
          LOC=IQ(LKFTSC+4+IQ(LKFTSC+2)+WIRE)+((IHIT-1)*IQ(LKFTSC+3))
          LOC=LOC+LKFTSC-1              ! Hit position in FTSC
          LOC1=IQ(LOC+10)
          LOC1=LOC1+LKFTDA              ! Pulse position in FTDA
          DRIFTT(IHIT) = Q(LOC1+2)
          Z(IHIT)=(DRIFTT(IHIT)+ETZERO)/NBPBIN + TMPUBN + .5
          PK(IHIT)= Q(LOC1+5)/GAIN
          RT(IHIT)= Q(LOC1+9)/NBPBIN
          TZ(IHIT)=999
          IH=IQ(LOC+9)
          IF( BTEST(IH,2) ) THEN
            TZ(IHIT) = 99
          ELSE
            GOTO 15
          ENDIF
          STATWORD=IQ(LOC+9)
          SEGNUM=0
          CALL MVBITS(STATWORD,8,8,SEGNUM,0)
C
          DO 12 ISEG=1,NSEG
            IF(SEGNUM.EQ.SEGMARK(ISEG)) THEN
              TZ(IHIT)=ISEG
              GOTO 15
            ENDIF
   12     CONTINUE
C
          NSEG = NSEG + 1
          IF(NSEG.GT.HITS_PER_WIRE) NSEG=HITS_PER_WIRE
          SEGMARK(NSEG)=SEGNUM
          TZ(IHIT)=NSEG
C
   15     CONTINUE
C
   10   CONTINUE
      ELSE
        NZ = IQ(LKFTDA+4+WIRE)
        IF( NZ.LE.0 ) GOTO 999
        DO 20 IHIT=1,MIN0(NZ,HITS_PER_WIRE)
          LOC1=IQ(LKFTDA+4+IQ(LKFTDA+2)+WIRE)+((IHIT-1)*IQ(LKFTDA+3))
          LOC1=LOC1+LKFTDA-1            ! Pulse position in FTDA
          DRIFTT(IHIT) = Q(LOC1+2)
          Z(IHIT)=(DRIFTT(IHIT)+ETZERO)/NBPBIN + TMPUBN + .5
          PK(IHIT)= Q(LOC1+5)/GAIN
          TZ(IHIT)=999
          STATWORD=IQ(LOC1+8)
          SEGNUM=0
          CALL MVBITS(STATWORD,8,8,SEGNUM,0)
          IF(SEGNUM.GT.0) THEN
            TZ(IHIT)=99
          ELSE
            GOTO 25
          ENDIF
C
          DO 22 ISEG=1,NSEG
            IF(SEGNUM.EQ.SEGMARK(ISEG)) THEN
              TZ(IHIT)=ISEG
              GOTO 25
            ENDIF
   22     CONTINUE
C
          NSEG = NSEG + 1
          IF(NSEG.GT.HITS_PER_WIRE) NSEG=HITS_PER_WIRE
          SEGMARK(NSEG)=SEGNUM
          TZ(IHIT)=NSEG
C
   25     CONTINUE
C
   20   CONTINUE
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
