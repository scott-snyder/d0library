      SUBROUTINE D0DAD_ZBINIT
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Initialize D0DAD specific Zebra division and
C-     global storage as well.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  10-Jan-1994   John D. Hobbs
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE  'D0$INC:zebcom.inc'
      INCLUDE  'D0$INC:quest.inc'
      INCLUDE  'D0$INC:d0dadcom.inc'
      LOGICAL FIRST
      SAVE    FIRST
      DATA    FIRST/.TRUE./
C-----------------------------------------------------------------------
C
      IF( .NOT.FIRST ) RETURN
      FIRST=.FALSE.
C
C  Initialize private division for D0DAD (why didn't I use C...)
C  and top bank.  Bank format is described in d0dadcom.inc
C
      LDSUP=0
      CALL VZERO(LRUNS,NLECHD)
      CALL MZDIV(IXCOM,IXDDAD,'DAD DIV',100,5000000,'P')
      CALL MZLINT(IXCOM,'/D0DAD/',LINTA,LDADH,LFCHD)
      CALL MZLINT(IXCOM,'/DADEC/',LINTB,LRUNS,LFLOK)
      CALL MZBOOK(IXDDAD,LDADH,LDSUP,1,'DADH',NFTYPE,NFTYPE,NFTYPE,2,0)
C
C  Setup file type descriptors
C
      CFTYPE(JFUE)='UE'
      CFTYPE(JFEC)='EC'
      CFTYPE(JFDF)='DF'
      CFTYPE(JFFC)='FC'
C
  999 RETURN
      END
