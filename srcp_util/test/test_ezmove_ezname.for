      PROGRAM TEST_EZMOVE_EZNAME
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  13-JUN-1992   Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSCPH.LINK'
      INTEGER LUN,LSUP,LCRCP,IZLINK,NCRCP,IFL,IER,LSRCP
      LOGICAL OK
      CHARACTER CFL*7
C----------------------------------------------------------------------
C
C ****  INIT 
C
      CALL MZEBRA(0)
      CALL INZSTP
      CALL CONSTP
C
C ****  READ RCP FILE
C
      CALL INRCP('TEST_RCP',IER)
      LSUP = LC(LSTPH-IZSCPH)
      IZLINK = 3
C
C ****  MOVE TO STP 
C
      CALL EZMOVE ('TEST_RCP',LSUP,IZLINK)
      LCRCP = LC(LSUP-IZLINK)
      LUN = 77
      NCRCP = 0 
      CFL = 'ALL'
      IFL = 1
      CALL D0OPEN(  LUN,'PRCP1.RCP','OFL',OK)
      CALL PRCRCP ( LUN,LCRCP, NCRCP, CFL, IFL )
      CLOSE (LUN)
C
C ****  WRITE OUT LSCPH TREE
C
      LUN= 45
      CALL ZZOPEN (LUN,'EZTEST',IER,'OUTPUT')
      LSUP = LC(LSTPH-IZSCPH)
      CALL FZOUT  (LUN,IDVSTP,LSUP,1,' ',1,0,0)
      CALL ZZCLOS (LUN,IER,'OUTPUT')
      CLOSE (LUN)
C
C ****  DROP CRCP & RCPFILE
C
      LSUP = LC(LSTPH-IZSCPH)
      CALL MZDROP(IXSTP,LSUP,'L')
      CALL EZDROP('TEST_RCP')
C
C ****  READ BACK LSCPH TREE
C
      LUN= 46
      CALL ZZOPEN (LUN,'EZTEST',IER,'INPUT')
      CALL FZIN   (LUN,IDVSTP,LSTPH,-IZSCPH,' ',0,0)
      CALL ZZCLOS (LUN,IER,'INPUT')
      CLOSE (LUN)
C
C ****  DECLARE CRCP as 'NEW_RCP'
C
      LSUP = LC(LSTPH-IZSCPH)
      CALL EZNAME ('NEW_RCP',LSUP,IZLINK)
      CALL EZLOC ('NEW_RCP',LSRCP)
      LUN = 49
      CALL D0OPEN(  LUN,'PRCP2.RCP','OF',OK)
      CALL EZDUMP ( LUN,LSRCP, 0)
      CLOSE (LUN)
C
      END
