      SUBROUTINE PRCDCH ( PRUNIT, KCDCH, NCDCH, CARFL, IPRFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print CDC hits header bank CDCH
C-
C-   Inputs  : PRUNIT : unit number
C-              KCDCH : bank address (not used) 
C-              NCDCH : bank number  (not used)
C-              CARFL : Character flag (not used)
C-              IPRFL : Level of print ( not used )
C-
C-   Created  17-AUG-1987   Ghita Rahal-Callot
C-   Updated  28-JUN-1989   Qizhong Li-Demarteau   rewrite 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:CDCLNK.INC'
C
      INTEGER PRUNIT, KCDCH, NCDCH, IPRFL, NVERS, NHITSW
      CHARACTER*(*) CARFL
      INTEGER NHITS, NWORDS, NWIRES,GZCDCH
C----------------------------------------------------------------------
C
      IF (LCDCH .LE. 0) THEN
        LCDCH = GZCDCH()
        IF (LCDCH .LE. 0) THEN
          WRITE(PRUNIT,1001) LCDCH        
 1001     FORMAT(/' Wrong Address, LCDCH =',I10)
          GO TO 999
        ENDIF
      ENDIF
C
      NHITS = IQ(LCDCH+1)      ! number of hits in CDC 
      NHITSW = IQ(LCDCH+10)    ! number of SW hits in CDC 
      NVERS = IBITS(IQ(LCDCH),13,5) 
      WRITE(PRUNIT,1002) NVERS, NHITS, NHITSW 
 1002 FORMAT(/,' Bank CDCH:  hit header bank for Central Drift Chamber',
     &  '  (version',I2,')'/,'  Total number of hits in CDC =',I10,
     &  /,'  Total number of hits on CDC sense wire =',I10)
C
      IF (IPRFL .LE. 1) GOTO 999
      NWORDS = IQ(LCDCH+2)
      NWIRES = IQ(LCDCH+3)
      WRITE(PRUNIT,1003) NWIRES,NWORDS
 1003 FORMAT
     & (' Sense Wire hits bank DSEC:  Number of wires per sector =',I3,
     &  '            Number of words per hit =',I3)
C
      NWORDS = IQ(LCDCH+4)
      NWIRES = IQ(LCDCH+5)
      WRITE(PRUNIT,1004) NWIRES,NWORDS
 1004 FORMAT
     & ('        CDC hits bank DCDA:  Number of FADC  per sector =',I3,
     &  '            Number of words per hit =',I3)
C
  999 RETURN
      END
