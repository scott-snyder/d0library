      SUBROUTINE TEST_PACK_REC
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TESTS PACK_REC
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  11-APR-1990   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:SHLCON.INC'
      INTEGER RKEY(5),RCYCLE,LREC
      INTEGER UKEY(5),UICYCLE
      INTEGER INDX_KEY,INDX
C----------------------------------------------------------------------
      TYPE *, ' TYPE IN RKEY(5), RCYCLE '
      ACCEPT *, RKEY,RCYCLE
C
      CALL PACK_REC(RKEY,RCYCLE,LREC)
C
      CYCLES(INDX_KEY(RKEY)) = RCYCLE
      CALL UNPACK_REC(UKEY,UICYCLE,LREC)
C
      TYPE *, ' UKEY,UICYCLE ',UKEY,UICYCLE
      INDX = INDX_KEY(UKEY)
      TYPE *, ' INDX_UKEY,CYCLES(INDX_KEY(UKEY)) ',INDX,CYCLES(INDX)
C
  999 RETURN
      END
