      SUBROUTINE PRFTDA(PRUNIT,LJFTDA,MFTDA,CFL,IFL)
C----------------------------------------------------------------------
C-
C-  Purpose and Methods : Print out FTDA (hits in FDC Theta sector) bank
C-
C-  INPUT: PRUNIT= unit number for printout
C-         LKFTDA = bank address
C-         NFTDA = numerical bank identifier (not used)
C-         CFL   = not used
C-         IFL   = 0      no printout
C-         IFL   = 1      no. of data per sector
C-         IFL   = 2      no. of data per sector and per wire
C-         IFL   = 3      full printout of bank
C-
C-   Created   x-JAN-1987   Daria Zieminska
C-   Updated   6-OCT-1988   Jeffrey Bantly  modified for new hit format
C-   Updated  14-MAR-1989   Jeffrey Bantly  full four levels of printout
C-   Updated  20-MAR-1990   Jeffrey Bantly  use logical format 
C-   Updated  14-FEB-1992   Susan K. Blessing  Remove machine block. 
C-
C------------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
C
      INTEGER HALF,UNIT,QUAD,SECTOR,WIRE,UB
      INTEGER PRUNIT,LKFTDA,LJFTDA,NFTDA,MFTDA,IFL,IWIRE,INDEX,IEND
      INTEGER NTOT,NCH,NWPH,NDATA(0:9),IPTR(0:9),KK,IDATA
      INTEGER IADD,NUMBER,JD,I,IVERS,NTOT1(0:7,0:5)
      INTEGER ISTAT,IBYTE0,IBYTE1,IBYTE2,IBYTE3
      INTEGER IHALF,MINHLF,MAXHLF,ITHE,MINTHE,MAXTHE,IQUAD,MINTQD
      INTEGER MAXTQD,ISECT,MINSEC,MAXSEC
      INTEGER GZFTDA
C
      CHARACTER CFL*(*)
C------------------------------------------------------------------------
C
      IF ( IFL .LE. 0 ) GOTO 999
      IF (CFL.EQ.'ALL') THEN
        MINHLF=0
        MAXHLF=1
        MINTHE=0
        MAXTHE=0
        MINTQD=0
        MAXTQD=7
        MINSEC=0
        MAXSEC=5
      ELSEIF (CFL.EQ.'ONE') THEN
        CALL FCODER(MFTDA,HALF,UNIT,QUAD,SECTOR,WIRE,UB,1)
        MINHLF=HALF
        MAXHLF=HALF
        MINTHE=UNIT
        MAXTHE=UNIT
        MINTQD=QUAD
        MAXTQD=QUAD
        MINSEC=SECTOR
        MAXSEC=SECTOR
      ENDIF
C
      IF(CFL.EQ. 'ALL' .AND. IFL.EQ. 1) THEN
        DO 1 IHALF=0,1
          DO 2 ITHE=0,0
            DO 3 IQUAD=0,7
              DO 4 ISECT=0,5
                LKFTDA=GZFTDA(IHALF,IQUAD,ISECT)
                IF( LKFTDA.LE.0 ) GOTO 4
                IVERS =IBITS(IQ(LKFTDA),13,5)
                NTOT1(IQUAD,ISECT) =IQ(LKFTDA+1) ! number of hits in this sector
    4         CONTINUE                    ! End ISECT loop
    3       CONTINUE                      ! End IQUAD loop
            WRITE(PRUNIT,5) IVERS, IHALF, ITHE
            WRITE(PRUNIT,6) ((IQUAD,ISECT,ISECT=0,5),IQUAD=0,3)
            WRITE(PRUNIT,7) ((NTOT1(IQUAD,ISECT),ISECT=0,5),IQUAD=0,3)
            WRITE(PRUNIT,6) ((IQUAD,ISECT,ISECT=0,5),IQUAD=4,7)
            WRITE(PRUNIT,7) ((NTOT1(IQUAD,ISECT),ISECT=0,5),IQUAD=4,7)
    2     CONTINUE                        ! End ITHE loop
    1   CONTINUE                          ! End IHALF loop
        GOTO 999
      ENDIF
    5 FORMAT(/' Hit banks FTDA -Version',I3,'  Half',I2,'  Unit',I2)
    6 FORMAT(1X,24(' ',I1,'-',I1))
    7 FORMAT(1X,24I4)
C
      LKFTDA=LJFTDA
      DO 10 IHALF=MINHLF,MAXHLF
        DO 20 ITHE=MINTHE,MAXTHE
          DO 30 IQUAD=MINTQD,MAXTQD
            DO 40 ISECT=MINSEC,MAXSEC
C
              IF (CFL.NE.'ONE' .OR. LKFTDA.LE.0) LKFTDA=
     &                             GZFTDA(IHALF,ITHE,IQUAD,ISECT)
              IF (LKFTDA.LE.0) THEN
                WRITE(PRUNIT,1011) LKFTDA
                GO TO 999
              END IF
C
              NFTDA=IQ(LKFTDA-5)
              CALL FCODER(NFTDA,HALF,UNIT,QUAD,SECTOR,WIRE,UB,1)
C
              IVERS =IBITS(IQ(LKFTDA),13,5)  ! version number
              NTOT  =IQ(LKFTDA+1)    ! number of data in sector
              NCH   =IQ(LKFTDA+2)    ! number of channels of FADC in sector
              NWPH  =IQ(LKFTDA+3)    ! number of words per hit in sector
              DO 100 IWIRE=0,NCH-1
                INDEX=IWIRE
                NDATA(INDEX)=IQ(LKFTDA+INDEX+4)
                IPTR(INDEX)=IQ(LKFTDA+INDEX+NCH+4)
  100         CONTINUE
C
              IF ( IFL .GE. 1 ) WRITE(PRUNIT,101) IVERS,HALF,UNIT,
     &                  QUAD,SECTOR,NTOT,NCH,NWPH
C
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
                  JD=LKFTDA+IPTR(INDEX)+NWPH*(IDATA-1)-1
                  IWIRE =IBITS(IQ(JD+1),0,4)
                  NUMBER=NUMBER+IWIRE*2
C  Unpack status word
                  ISTAT=IQ(JD+8)
                  IBYTE0=IBITS(ISTAT,0,8)
                  IBYTE1=IBITS(ISTAT,8,8)
                  IBYTE2=IBITS(ISTAT,16,8)
                  IBYTE3=IBITS(ISTAT,24,8)
                  WRITE(PRUNIT,106) IWIRE,Q(JD+2),Q(JD+6),Q(JD+3),
     $                     Q(JD+7),Q(JD+4),Q(JD+5),IBYTE3,IBYTE2,
     $                     IBYTE1,IBYTE0,(JD+1)
  201           CONTINUE
  200         CONTINUE
C
   40       CONTINUE
   30     CONTINUE
   20   CONTINUE
   10 CONTINUE
C
  101 FORMAT(/' Data bank for sector FTDA - Version',I3/,
     $' Half/Unit/Quad/Sector =',3I2,I3,' Nu data in sector =',I5,
     $' Nu of channels/sector =',I4,' Nu of words/hit =',I4)
  102 FORMAT(' Chan    0    1    2    3    4    5    6    7  ',
     $'  8    9  ')
  103 FORMAT(' Hits',10I5)
  105 FORMAT(' Channel      drift_time  drift_error ',
     $'pulse_area pul_hgt_err  ',
     $'pulse_width  peak_height     status    rawptr'/
     $       '                 (ns)        (ns)     ',
     $'   (ns)        (ns)     ',
     $'   (ns)                   by3by2by1by0       ')
  106 FORMAT(1X,I5,9X,F7.1,'   +-',F6.1,6X,F7.1,'   +-',F6.1,6X,F6.1,6X,
     $ F7.1,5X,4I3,I8)
 1011 FORMAT(/' WRONG ADDRESS, LKFTDA =',I10)
C---------------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
