      SUBROUTINE PRFPSC(PRUNIT,LJFPSC,MFPSC,CFL,IFL)
C-----------------------------------------------------------------------
C-
C-  Purpose and Methods : Print out FPSC (Hits in FDC Phi sector) bank
C-
C-  INPUT: PRUNIT= unit number for printout
C-         LJFPSC = bank address
C-         NFPSC = numerical bank identifier (not used)
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
      INTEGER PRUNIT,LKFPSC,LJFPSC,NFPSC,MFPSC,IFL,IWIRE
      INTEGER NTOT,NWIR,NWPH,NHITS(0:15),IPTR(0:15),KK,JH
      INTEGER IWIR,NUMBER,IHITS,IVERS,NTOT1(0:35)
      INTEGER ISTAT,IBYTE0,IBYTE1,IBYTE2,IBYTE3
      INTEGER IHALF,MINHLF,MAXHLF,ISECT,MINSEC,MAXSEC
      INTEGER GZFPSC
C
      CHARACTER CFL*(*)
C------------------------------------------------------------------------
C
      IF (IFL.LE.0) GOTO 999
      IF (CFL.EQ.'ALL') THEN
        MINHLF=0
        MAXHLF=1
        MINSEC=0
        MAXSEC=35
      ELSEIF (CFL.EQ.'ONE') THEN
        CALL FCODER(MFPSC,HALF,UNIT,QUAD,SECTOR,WIRE,UB,1)
        MINHLF=HALF
        MAXHLF=HALF
        MINSEC=SECTOR
        MAXSEC=SECTOR
      ENDIF
C
      IF(CFL.EQ. 'ALL' .AND. IFL.EQ. 1) THEN
        DO 1 IHALF=0,1
          DO 4 ISECT=0,35
            LKFPSC=GZFPSC(IHALF,ISECT)
            IF( LKFPSC.LE.0 ) GOTO 4
            IVERS =IBITS(IQ(LKFPSC),13,5)
            NTOT1(ISECT) =IQ(LKFPSC+1)  ! number of hits in this sector
    4     CONTINUE                    ! End ISECT loop
          WRITE(PRUNIT,5) IVERS, IHALF
          WRITE(PRUNIT,6) (ISECT,ISECT=0,17)
          WRITE(PRUNIT,7) (NTOT1(ISECT),ISECT=0,17)
          WRITE(PRUNIT,6) (ISECT,ISECT=18,35)
          WRITE(PRUNIT,7) (NTOT1(ISECT),ISECT=18,35)
    1   CONTINUE                          ! End IHALF loop
        GOTO 999
      ENDIF
    5 FORMAT(/' Hit banks FPSC -Version',I3,'  Half',I2,'  Unit 1')
    6 FORMAT(2X,18(' S',I2,' '))
    7 FORMAT(1X,18I5)
C
      LKFPSC=LJFPSC
      DO 10 IHALF=MINHLF,MAXHLF
        DO 40 ISECT=MINSEC,MAXSEC
C
          IF (CFL.NE.'ONE' .OR. LKFPSC.LE.0)
     &                  LKFPSC=GZFPSC(IHALF,ISECT)
          IF (LKFPSC.LE.0) THEN
            WRITE(PRUNIT,1011) LKFPSC
            GO TO 999
          END IF
C
          NFPSC = IQ(LKFPSC-5)
          CALL FCODER(NFPSC,HALF,UNIT,QUAD,SECTOR,WIRE,UB,1)
C
          IVERS =IBITS(IQ(LKFPSC),13,5)
          NTOT  =IQ(LKFPSC+1)    ! number of hits in this sector
          NWIR  =IQ(LKFPSC+2)    ! number of wires in this sector
          NWPH  =IQ(LKFPSC+3)    ! number of words per hit in this sector
          DO 100 IWIRE=0,NWIR-1
            NHITS(IWIRE)=IQ(LKFPSC+IWIRE+4)
            IPTR(IWIRE)=IQ(LKFPSC+IWIRE+NWIR+4)
  100     CONTINUE
C
          IF ( IFL .GE. 1 ) WRITE(PRUNIT,101) IVERS,HALF,SECTOR,
     &                  NTOT,NWIR,NWPH
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
            IF(NHITS(IWIRE).LE.0) GO TO 200
            DO 201 IHITS=1,NHITS(IWIRE)
              JH=LKFPSC+IPTR(IWIRE)+NWPH*(IHITS-1)-1
              IWIR =IBITS(IQ(JH+1),0,4)
C  Unpack status word
              ISTAT=IQ(JH+9)
              IBYTE0=IBITS(ISTAT,0,8)
              IBYTE1=IBITS(ISTAT,8,8)
              IBYTE2=IBITS(ISTAT,16,8)
              IBYTE3=IBITS(ISTAT,24,8)
              WRITE(PRUNIT,105) IWIR,Q(JH+2),Q(JH+3),Q(JH+5),
     $          Q(JH+4),Q(JH+6),Q(JH+7),Q(JH+8),IQ(JH+10),
     $          IBYTE3,IBYTE2,IBYTE1,IBYTE0
  201       CONTINUE
  200     CONTINUE
C
   40   CONTINUE
   10 CONTINUE
C
  101 FORMAT(/' Hit bank for sector FPSC - Version',I3/,
     $' Half/Sector =',I2,I3,' Nu hits in this sector =',I5,
     $' N of wires per sector =',I4,' Nu of words per hit =',I4)
  102 FORMAT(' Wire    0    1    2    3    4  ',
     $'  5    6    7    8    9   10   11   12   13   14   15  ')
  103 FORMAT(' Hits',16I5)
  104 FORMAT(' Wire    Drift+  Drift- DrftErr   Spare',
     $'  Spare  Ionization IonizErr  TRACK',
     $'    Status'/
     $       '          (cm)    (cm)    (cm)         ',
     $'          (M.I.P.)  (M.I.P.)       ',
     $' by3by2by1by0')
  105 FORMAT(1X,I3,2X,F8.3,F8.3,'+-',F6.3,2X,F6.1,' +-',F6.1,
     $ 2X,F7.1,F9.1,I7,2X,4I3)
 1011 FORMAT(/' WRONG ADDRESS, LKFPSC =',I10)
C------------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
