      SUBROUTINE D0HCHD
C========================================================================
C
C  Description:  Change HBOOK4 directory
C  =============
C
C  Author:
C  ========
C  Tami Kramer
C
C  Revision History:
C  ==================
C  Original Creation - May 15,1989
C
C=========================================================================
C
      IMPLICIT NONE
C
C  Local Declarations:
C  ====================
C
      INTEGER NWORD
      PARAMETER (NWORD=8)
      REAL ARRAY(NWORD)
      INTEGER LEN,TRULEN
      LOGICAL FLGVAL
      CHARACTER*40 CDIR,NDIR
      CHARACTER*60 MSG,MESS1,MESS2
C==========================================================================
C
C  Executable Code:
C  =================
C
      CALL HCDIR(CDIR,'R')
      LEN = TRULEN(CDIR)
      MSG = ' The current directory is > '//CDIR(1:LEN)
      CALL INTMSG(MSG)
      CALL GETPAR(1,' Enter full hist DIR name (ex //PAWC/TEST)>'
     X   ,'U',NDIR)
      LEN = TRULEN(NDIR)
      CALL HCDIR(NDIR(1:LEN),' ')
      CALL HCDIR(CDIR,'R')
      CALL D0HSDN(CDIR(1:LEN))
      LEN = TRULEN(CDIR)
      MSG = ' Directory has been changed to > '//CDIR(1:LEN)
      CALL INTMSG(MSG)
      RETURN
      END
