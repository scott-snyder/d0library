      SUBROUTINE PRFPDA(PRUNIT,LJFPDA,MFPDA,CFL,IFL)
C---------------------------------------------------------------------
C-
C-  Purpose and Methods : Print out FPDA (hits in one FDC Phi sector) bank
C-
C-  INPUT: PRUNIT = unit number for printout
C-         LJFPDA = bank address
C-         NFPDA  = numerical bank identifier (not used)
C-         CFL    = not used
C-         IFL   = 0      no printout
C-         IFL   = 1      no. of data per sector
C-         IFL   = 2      no. of data per sector and per wire
C-         IFL   = 3      full printout of bank
C-
C-   Created   x-JAN-1987   Daria Zieminska
C-   Updated   6-OCT-1988   Jeffrey Bantly  modified for new hit format
C-   Updated  14-MAR-1989   Jeffrey Bantly  full four levels of printout
C-   Updated  20-MAR-1990   Jeffrey Bantly  use logical format
C-   Updated   8-NOV-1990   Jeffrey Bantly  add 'ALL' & 1 dump option 
C-   Updated  14-FEB-1992   Susan K. Blessing  Remove machine block. 
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
C
      INTEGER HALF,UNIT,QUAD,SECTOR,WIRE,UB
      INTEGER PRUNIT,LKFPDA,LJFPDA,NFPDA,MFPDA,IFL,IWIRE,INDEX,IEND
      INTEGER NTOT,NCH,NWPH,NDATA(0:15),IPTR(0:15),KK,IDATA
      INTEGER NUMBER,JD,IVERS,NTOT1(0:35)
      INTEGER ISTAT,IBYTE0,IBYTE1,IBYTE2,IBYTE3
      INTEGER IHALF,MINHLF,MAXHLF,ISECT,MINSEC,MAXSEC
      INTEGER GZFPDA
C
      CHARACTER CFL*(*)
C-----------------------------------------------------------------------
C
      IF (IFL.LE.0) GOTO 999
      IF (CFL.EQ.'ALL') THEN
        MINHLF=0
        MAXHLF=1
        MINSEC=0
        MAXSEC=35
      ELSEIF (CFL.EQ.'ONE') THEN
        CALL FCODER(MFPDA,HALF,UNIT,QUAD,SECTOR,WIRE,UB,1)
        MINHLF=HALF
        MAXHLF=HALF
        MINSEC=SECTOR
        MAXSEC=SECTOR
      ENDIF
C
      IF(CFL.EQ. 'ALL' .AND. IFL.EQ. 1) THEN
        DO 1 IHALF=0,1
          DO 4 ISECT=0,35
            LKFPDA=GZFPDA(IHALF,ISECT)
            IF( LKFPDA.LE.0 ) GOTO 4
            IVERS =IBITS(IQ(LKFPDA),13,5)
            NTOT1(ISECT) =IQ(LKFPDA+1)  ! number of hits in this sector
    4     CONTINUE                    ! End ISECT loop
          WRITE(PRUNIT,5) IVERS, IHALF
          WRITE(PRUNIT,6) (ISECT,ISECT=0,17)
          WRITE(PRUNIT,7) (NTOT1(ISECT),ISECT=0,17)
          WRITE(PRUNIT,6) (ISECT,ISECT=18,35)
          WRITE(PRUNIT,7) (NTOT1(ISECT),ISECT=18,35)
    1   CONTINUE                          ! End IHALF loop
        GOTO 999
      ENDIF
    5 FORMAT(/' Hit banks FPDA -Version',I3,'  Half',I2,'  Unit 1')
    6 FORMAT(2X,18(' S',I2,' '))
    7 FORMAT(1X,18I5)
C
      LKFPDA=LJFPDA
      DO 10 IHALF=MINHLF,MAXHLF
        DO 40 ISECT=MINSEC,MAXSEC
C
          IF (CFL.NE.'ONE' .OR. LKFPDA.LE.0)
     &                  LKFPDA=GZFPDA(IHALF,ISECT)
          IF (LKFPDA.LE.0) THEN
            WRITE(PRUNIT,1011) LKFPDA
            GO TO 999
          END IF
C
          NFPDA = IQ(LKFPDA-5)
          CALL FCODER(NFPDA,HALF,UNIT,QUAD,SECTOR,WIRE,UB,1)
C
          IVERS =IBITS(IQ(LKFPDA),13,5)
          NTOT  =IQ(LKFPDA+1)    ! number of data in this sector
          NCH   =IQ(LKFPDA+2)    ! number of channels of FADC in this sector
          NWPH  =IQ(LKFPDA+3)    ! number of words per hit in this sector
          DO 100 IWIRE=0,NCH-1
            NDATA(IWIRE)=IQ(LKFPDA+IWIRE+4)
            IPTR(IWIRE)=IQ(LKFPDA+IWIRE+NCH+4)
  100     CONTINUE
          IF ( IFL .GE. 1 ) WRITE(PRUNIT,101) IVERS,HALF,SECTOR,
     &                  NTOT,NCH,NWPH
          IF ( IFL .EQ. 2 ) THEN
            WRITE(PRUNIT,102)
            WRITE(PRUNIT,103) (NDATA(KK),KK=0,NCH-1)
          END IF
C
          IF ( NTOT .LE. 0 ) GOTO 40
          IF ( IFL .NE. 3 ) GOTO 40
          WRITE(PRUNIT,105)
          DO 200 INDEX=0,NCH-1
            DO 201 IDATA=1,NDATA(INDEX)
              JD=LKFPDA+IPTR(INDEX)+NWPH*(IDATA-1)-1
              IWIRE =IBITS(IQ(JD+1),0,4)
C  Unpack status word
              ISTAT=IQ(JD+8)
              IBYTE0=IBITS(ISTAT,0,8)
              IBYTE1=IBITS(ISTAT,8,8)
              IBYTE2=IBITS(ISTAT,16,8)
              IBYTE3=IBITS(ISTAT,24,8)
              WRITE(PRUNIT,106) IWIRE,Q(JD+2),Q(JD+6),Q(JD+3),
     $              Q(JD+7),Q(JD+4),Q(JD+5),IBYTE3,IBYTE2,
     $              IBYTE1,IBYTE0,(JD+1)
  201       CONTINUE
  200     CONTINUE
C
   40   CONTINUE
   10 CONTINUE
C
  101 FORMAT(/' Data bank for sector FPDA - Version',I3/,
     $' Half/Sector =',I2,I3,' Nu data in this sector =',I5,
     $' Nu of channels per sector =',I4,' Nu of words per hit =',I4)
  102 FORMAT(' Chan    0    1    2    3    4    5    6    7    8  ',
     $'  9   10   11   12   13   14   15  ')
  103 FORMAT(' Hits',16I5)
  105 FORMAT(' Channel     drift_time   drift_error ',
     $'pulse_area pul_hgt_err ',
     $'pulse_width peak_height     status    rawptr '/
     $       '                (ns)           (ns)   ',
     $'  (ns)        (ns)     ',
     $'    (ns)                 by3by2by1by0        ')
  106 FORMAT(1X,I5,6X,F7.1,'    +-  ',F6.1,6X,F7.1,'  +-',F6.1,5X,F7.1,
     $ 5X,F7.1,5X,4I3,I8)
 1011 FORMAT(/' WRONG ADDRESS, LKFPDA =',I10)
C-----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
