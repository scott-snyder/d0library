      SUBROUTINE PRPLV0(PRUNIT,LOC,IDNUM,CFL,IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print out the PLV0 bank contents.
C-
C-   Inputs  : PRUNIT = output unit
C-             LOC    = (not used)
C-             IDNUM  = (not used)
C-             CFL    = (not used)
C-             IFL    = print level
C-                      IFL = 0   no printout
C-                      IFL = 1   Correct Bunch number and Z vertex only
C-                      IFL = 2   1 and Z quality, num hits by counter type
C-                                  and multiple interaction flags
C-                      IFL = 3   1,2 and specific counters hit
C-
C-   Created  20-JUL-1992   Jeffrey Bantly
C-   Updated   2-JUL-1993   Jeffrey Bantly  PLV0 bank updated 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER PRUNIT,LOC,IDNUM,IFL
      INTEGER I
      INTEGER ICONT(20)
C
      REAL    CONT(20)
      EQUIVALENCE ( ICONT, CONT )
C
      CHARACTER*3 CFL
C
C----------------------------------------------------------------------
C
      IF ( IFL.LE.0 ) GOTO 999
C
      CALL GTPLV0(ICONT)
C
      IF ( ICONT(1).EQ.0 ) THEN
        WRITE(PRUNIT,*) ' Bank PLV0 not found or empty'
        GOTO 999
      ENDIF
C
      WRITE(PRUNIT,1000) IBITS(ICONT(1),7,3)
      WRITE(PRUNIT,1001) IBITS(ICONT(1),0,1),CONT(2),CONT(3),CONT(5)
C
      IF ( IFL.LT.2 ) GOTO 999
C
      WRITE(PRUNIT,1002) (IBITS(ICONT(1),I,1),I=1,6)
      IF ( CONT(4).GT.1000.0 ) CONT(4) = 999.99
      IF ( CONT(6).GT.1000.0 ) CONT(6) = 999.99
      WRITE(PRUNIT,1003) CONT(4),CONT(6),(ICONT(I),I=7,10)
C
      IF ( IFL.LT.3 ) GOTO 999
C
      WRITE(PRUNIT,1012) (IBITS(ICONT(1),I,1),I=10,17)
      WRITE(PRUNIT,1004) (IBITS(ICONT(11),(I-1),1),I=1,20)
      WRITE(PRUNIT,1005) (IBITS(ICONT(13),(I-21),1),I=21,36)
      WRITE(PRUNIT,1006) (IBITS(ICONT(12),(I-37),1),I=37,56)
      WRITE(PRUNIT,1007) (IBITS(ICONT(13),(I-41),1),I=57,72)
      WRITE(PRUNIT,1008) CONT(14),CONT(15)
      WRITE(PRUNIT,1009) CONT(16),CONT(17)
C
 1000 FORMAT(//' Bank PLV0 Contents - Correct Bunch =',I2)
 1001 FORMAT(/' FASTZ ',I1,' =',F8.2,'      Slow Z =',F8.2,
     &       '      Slower Z =',F8.2)
 1002 FORMAT(/' Multiple Interaction Bits =',4I2,6X,'Good Slow Z =',I2,
     &       '   Good Slower Z =',I2)
 1003 FORMAT(/' Slow Z Quality =',F7.2,18X,'Slower Z Quality =',F7.2/
     &       /' Number of North Short Counters Hit = ',I3/
     &       ' Number of North Long  Counters Hit = ',I3/
     &       ' Number of South Short Counters Hit = ',I3/
     &       ' Number of South Long  Counters Hit = ',I3)
 1004 FORMAT(/' North Short Counters Hit = ',20I2)
 1005 FORMAT(' North Long  Counters Hit = ',16I2)
 1006 FORMAT(' South Short Counters Hit = ',20I2)
 1007 FORMAT(' South Long  Counters Hit = ',16I2)
 1008 FORMAT(/' Full Z =',F10.2,19X,'    Full SZ =',F10.2)
 1009 FORMAT(' North FASTZ Time =',F9.2,13X,' South FASTZ Time =',F9.2/)
 1012 FORMAT(/' P Halo Bit =',I2,24X,'   Pbar Halo Bit =',I2/
     &       ' Slow Z Interaction Bit =',I2,
     &       12X,'   Slower Z Interaction Bit =',I2/
     &       ' Min, max times Excluded N =',I2,'  S =',I2,
     &       '     Long Counters used N =',I2,'  S =',I2)
C----------------------------------------------------------------------
  999 RETURN
      END
