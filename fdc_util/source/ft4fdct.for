      SUBROUTINE FT4FDCT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Loop over Quads 0-3, Sectors 4-5 in the 
C-                         inner Theta chambers and make tracks from
C-                         single segments IF they have a good delay 
C-                         line hit.
C-
C-   Created  18-DEC-1990   Jeffrey Bantly
C-   Updated  29-APR-1991   Jeffrey Bantly  use new RCP,PARAMS 
C-   Updated  17-SEP-1991   Susan K. Blessing  Change size of (I)QTRAK
C-    to accomodate theta and phi errors and two spare words.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INTEGER HALF,QUAD,SECTOR,LAYER,MODULE,IADD
      INTEGER IER,INEFF,ICALL
      INTEGER MAXSEG,ISEG,NSEG,LADDER(0:2)
      INTEGER LKFSEG,LOC
      INTEGER IQTRAK(26),IQHSEC(3,34)
      INTEGER NZFIND,NZBANK,GZFSEG
C
      REAL QTRAK(26),QHSEC(3,34)
      REAL CHINORM,CHIMAX_SHORT
C
      LOGICAL BTEST
C&IF VAXVMS,VAXELN
C&ELSE
C&        EXTERNAL BTEST
C&ENDIF
C
      SAVE INEFF,ICALL
      DATA ICALL/0/
C------------------------------------------------------------------------
C
      IF (ICALL.EQ.0) THEN
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET_i('TINEFF',INEFF,IER)
        CALL EZGET('CHIMAX',CHIMAX_SHORT,IER)
        CALL EZRSET
        ICALL=1
      END IF
C
C  Find valid, unused segments and fit them in a track.
C
      DO 10 HALF=0,1
        LAYER=0
        MODULE=3*HALF
        LKFSEG=GZFSEG(HALF,LAYER)
        IF(LKFSEG.LE.0) GOTO 10
        MAXSEG=NZBANK(IXCOM,LKFSEG)
        IF(MAXSEG.LE.0) GOTO 10
        DO 20 QUAD=0,3
          DO 30 SECTOR=4,5
            CALL FCODER(IADD,HALF,0,QUAD,SECTOR,0,0,2)
            NSEG=NZFIND(IXCOM,LKFSEG,IADD,+2)
            DO 40 ISEG=1,NSEG
              LOC=IQUEST(ISEG)
              IF(LOC.LE.0) GOTO 30
              IF (BTEST(IQ(LOC),IUSED)) GOTO 40
              LADDER(0)=IQ(LOC-5)
              LADDER(1)=0
              LADDER(2)=0
              IF(LADDER(0).LE.0 .OR. LADDER(0).GT.MAXSEG) GOTO 40
              CALL FTFDCT(HALF,LADDER,QTRAK,IQTRAK,QHSEC,IQHSEC,CHINORM)
              IF(CHINORM.LE.CHIMAX_SHORT) THEN
                IQTRAK(1)=HALF
                CALL LDFDCT(QTRAK,QHSEC,HALF,LADDER) ! Store track
              ENDIF
   40       CONTINUE                    ! End of segment loop
   30     CONTINUE                      ! End of sector loop
   20   CONTINUE                        ! End of Quadrant loop
   10 CONTINUE                          ! End of Half loop
C
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
