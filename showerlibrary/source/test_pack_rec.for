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
C-   Updated  24-MAR-2004   sss - compile with g77.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:SHLCON.INC'
      INTEGER RKEY(5),RCYCLE,LREC
      INTEGER UKEY(5),UICYCLE
      INTEGER INDX_KEY,INDX
C----------------------------------------------------------------------
      write (*,*), ' TYPE IN RKEY(5), RCYCLE '
C&IF LINUX
C&      read (*,*), RKEY,RCYCLE
C&ELSE
      ACCEPT *, RKEY,RCYCLE
C&ENDIF
C
      CALL PACK_REC(RKEY,RCYCLE,LREC)
C
      CYCLES(INDX_KEY(RKEY)) = RCYCLE
      CALL UNPACK_REC(UKEY,UICYCLE,LREC)
C
      write (*,*), ' UKEY,UICYCLE ',UKEY,UICYCLE
      INDX = INDX_KEY(UKEY)
      write (*,*),' INDX_UKEY,CYCLES(INDX_KEY(UKEY)) ',INDX,CYCLES(INDX)
C
  999 RETURN
      END
