      SUBROUTINE PRFTSC(PRUNIT,LJFTSC,MFTSC,CFL,IFL)
C----------------------------------------------------------------------
C-
C-  Purpose and Methods : Print out FTSC (Hits in FDC Theta unit) bank.
C-
C-  INPUT: PRUNIT= unit number for printout
C-         LJFTSC = bank address
C-         NFTSC = numerical bank identifier (not used)
C-         CFL   = not used
C-         IFL   = 0      no printout
C-         IFL   = 1      no. of hits per sector
C-         IFL   = 2      no. of hits per sector and per wire
C-         IFL   = 3      full printout of bank
C-
C-   Created   x-JAN-1987   Daria Zieminska
C-   Updated   6-OCT-1988   Jeffrey Bantly  modified for new hit format
C-   Updated  14-MAR-1989   Jeffrey Bantly  full four levels of printout
C-   Updated  20-MAR-1990   Jeffrey Bantly  use logical format
C-   Updated   8-NOV-1990   Jeffrey Bantly  add 'ALL' & 1 dump format 
C-   Updated  14-FEB-1992   Susan K. Blessing  Remove machine block. 
C-
C------------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
C
      INTEGER HALF,UNIT,QUAD,SECTOR,WIRE,UB
      INTEGER PRUNIT,LKFTSC,LJFTSC,NFTSC,MFTSC,IFL,IWIRE
      INTEGER NTOT,NWIR,NWPH,NHITS(0:7),IPTR(0:7),KK,JH
      INTEGER IWIR,NUMBER,IHITS,IVERS,NTOT1(0:7,0:5)
      INTEGER ISTAT,IBYTE0,IBYTE1,IBYTE2,IBYTE3
      INTEGER IHALF,MINHLF,MAXHLF,ITHE,MINTHE,MAXTHE,IQUAD,MINTQD
      INTEGER MAXTQD,ISECT,MINSEC,MAXSEC
      INTEGER GZFTSC
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
        CALL FCODER(MFTSC,HALF,UNIT,QUAD,SECTOR,WIRE,UB,1)
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
                LKFTSC=GZFTSC(IHALF,IQUAD,ISECT)
                IF( LKFTSC.LE.0 ) THEN
                  NTOT1(IQUAD,ISECT) = 0
                ELSE
                  IVERS =IBITS(IQ(LKFTSC),13,5)
                  NTOT1(IQUAD,ISECT) =IQ(LKFTSC+1) ! number of hits in this sector
                ENDIF
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
    5 FORMAT(/' Hit banks FTSC -Version',I3,'  Half',I2,'  Unit',I2)
    6 FORMAT(1X,24(' ',I1,'-',I1))
    7 FORMAT(1X,24I4)
C
      LKFTSC=LJFTSC
      DO 10 IHALF=MINHLF,MAXHLF
        DO 20 ITHE=MINTHE,MAXTHE
          DO 30 IQUAD=MINTQD,MAXTQD
            DO 40 ISECT=MINSEC,MAXSEC
C
              IF (CFL.NE.'ONE' .OR. LKFTSC.LE.0)
     &                  LKFTSC=GZFTSC(IHALF,IQUAD,ISECT)
              IF (LKFTSC.LE.0) THEN
                WRITE(PRUNIT,1011) LKFTSC
                GO TO 999
              END IF
C
              NFTSC = IQ(LKFTSC-5)
              CALL FCODER(NFTSC,HALF,UNIT,QUAD,SECTOR,WIRE,UB,1)
C
              IVERS =IBITS(IQ(LKFTSC),13,5)
              NTOT  =IQ(LKFTSC+1)    ! number of hits in this sector
              NWIR  =IQ(LKFTSC+2)    ! number of wires in this sector
              NWPH  =IQ(LKFTSC+3)    ! number of words per hit in this sector
              DO 100 IWIRE=0,NWIR-1
                NHITS(IWIRE)=IQ(LKFTSC+IWIRE+4)
                IPTR(IWIRE)=IQ(LKFTSC+IWIRE+NWIR+4)
  100         CONTINUE
C
              IF ( IFL .GE. 1 ) WRITE(PRUNIT,101) IVERS,HALF,UNIT,QUAD,
     &                  SECTOR,NTOT,NWIR,NWPH
C
              IF ( IFL .EQ. 2 ) THEN
                WRITE(PRUNIT,102)
                WRITE(PRUNIT,103) (NHITS(KK),KK=0,NWIR-1)
              ENDIF
C
C  Print all contents of bank
C
              IF ( NTOT .LE. 0 ) GOTO 40
              IF ( IFL .NE. 3 ) GOTO 40
              WRITE(PRUNIT,104)
              DO 200 IWIRE=0,NWIR-1
                IF(NHITS(IWIRE).LE.0) GOTO 200
                DO 201 IHITS=1,NHITS(IWIRE)
                  JH=LKFTSC+IPTR(IWIRE)+NWPH*(IHITS-1)-1
                  IWIR =IBITS(IQ(JH+1),0,4)
C  Unpack status word
                  ISTAT=IQ(JH+9)
                  IBYTE0=IBITS(ISTAT,0,8)
                  IBYTE1=IBITS(ISTAT,8,8)
                  IBYTE2=IBITS(ISTAT,16,8)
                  IBYTE3=IBITS(ISTAT,24,8)
                  WRITE(PRUNIT,105) IWIR,Q(JH+2),Q(JH+3),Q(JH+5),
     $                 Q(JH+4),Q(JH+6),Q(JH+7),Q(JH+8),IQ(JH+10),
     $                 IBYTE3,IBYTE2,IBYTE1,IBYTE0
  201           CONTINUE
  200         CONTINUE
C
   40       CONTINUE
   30     CONTINUE
   20   CONTINUE
   10 CONTINUE
C
  101 FORMAT(/' Hit bank for sector FTSC - Version',I3/,
     $' Half/Unit/Quad/Sector =',4I2,'  # hits in sector =',I5,
     $'  # of wires/sector =',I4,'  # of words/hit =',I4)
  102 FORMAT(' Wire    0    1    2    3    4    5    6    7')
  103 FORMAT(' Hits',8I5)
  104 FORMAT(' Wire  Drift+  Drift- DrftErr Y/X_Delay_Line',
     $'  Delay Err    Ionization  IonizErr  TRACK ',
     $'    Status'/
     $       '        (cm)    (cm)    (cm)       (cm)     ',
     $'    (cm)         (M.I.P.)  (M.I.P.)        ',
     $'  by3by2by1by0')
  105 FORMAT(1X,I3,2X,F6.3,F8.3,'+-',F6.3,4X,F6.1,'    +-  ',F6.1,
     $ 9X,F7.1,F9.1,I7,4X,4I3)
 1011 FORMAT(/' WRONG ADDRESS, LKFTSC =',I10)
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
