      SUBROUTINE UDST_GET_JNEP(LJNEP,XDATA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : return JNEP parameters
C-
C-   Inputs  : LJNEP     - pointer to JNEP bank
C-   Outputs : XDATA
C-
C-   Updated  29-SEP-1992   Ulrich Heintz
C-   Updated  14-AUG-1993   Ulrich Heintz - added px,py,pz,link to matched EM
C-   Updated  22-NOV-1993   Ian Adam - add 0.3 cone
C-   Updated  28-NOV-1995   Ulrich Heintz - added px,py,pz  
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LJNEP,I,KJNEP,KJNEP1,ICONE,ID
      PARAMETER (KJNEP=11)
      CHARACTER*8 JNEP_TAGS(KJNEP,3)
      REAL PTJN,EJN,ETJN,PHIJN,ETAJN,RMEJN,RMPJN,FEMJN,PYJN,PZJN,LJETJN
      COMMON/JNEP_OBJECT/
     &     PTJN,EJN,ETJN,PHIJN,ETAJN,RMEJN,RMPJN,FEMJN,PYJN,PZJN,LJETJN
      REAL XX(KJNEP),XDATA(KJNEP)
      EQUIVALENCE(XX,PTJN)
      DATA JNEP_TAGS/
     &  'PTJN7' ,'EJN7'  ,'ETJN7' ,'PHIJN7','ETAJN7','RMEJN7','RMPJN7',
     &  'FEMJN7','PYJN7','PZJN7','LJETJN7',
     &  'PTJN5' ,'EJN5'  ,'ETJN5' ,'PHIJN5','ETAJN5','RMEJN5','RMPJN5',
     &  'FEMJN5','PYJN5','PZJN5','LJETJN5',
     &  'PTJN3' ,'EJN3'  ,'ETJN3' ,'PHIJN3','ETAJN3','RMEJN3','RMPJN3',
     &  'FEMJN3','PYJN3','PZJN3','LJETJN3'/
C----------------------------------------------------------------------
      PTJN  = Q(LJNEP+2)
      PYJN  = Q(LJNEP+3)
      PZJN  = Q(LJNEP+4)
      EJN   = Q(LJNEP+5)
      ETJN  = Q(LJNEP+6)
      PHIJN = Q(LJNEP+8)
      ETAJN = Q(LJNEP+9)
      RMEJN = Q(LJNEP+12)
      RMPJN = Q(LJNEP+13)
      FEMJN = Q(LJNEP+14)
      LJETJN = 0.0                  ! filled in calling routine
      DO I=1,KJNEP
        XDATA(I) = XX(I)
      ENDDO
C----------------------------------------------------------------------
  999 RETURN
C
      ENTRY UDST_JNEP_TAGS(ICONE,KJNEP1,ID)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : book JNEP groups
C-
C-   Inputs  : ICONE
C-   Outputs : KJNEP1,ID
C-
C-   Created  23-AUG-1993   Ulrich Heintz
C-   Updated  22-NOV-1993   Ian Adam - add 0.3 cone
C----------------------------------------------------------------------
      KJNEP1=KJNEP
      IF(ICONE.EQ.1)THEN
        ID=11
        CALL UDST_BOOK_GROUP(ID,'JNP7',JNEP_TAGS(1,1),KJNEP)
      ELSEIF(ICONE.EQ.2)THEN
        ID=12
        CALL UDST_BOOK_GROUP(ID,'JNP5',JNEP_TAGS(1,2),KJNEP)
      ELSEIF(ICONE.EQ.3)THEN
        ID=13
        CALL UDST_BOOK_GROUP(ID,'JNP3',JNEP_TAGS(1,3),KJNEP)
      ENDIF
C----------------------------------------------------------------------
  998 RETURN
      END
