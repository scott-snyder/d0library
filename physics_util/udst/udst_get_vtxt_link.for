      SUBROUTINE UDST_GET_VTXT_LINK(LZTRK,UDST_VTXT_LINK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : find the link ZTRK->VTXT
C-
C-   Inputs  : LZTRK
C-   Outputs : UDST_VTXT_LINK
C-
C-   Created  15-OCT-1994   Ulrich Heintz
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:UDST_LINKS.INC'
      INTEGER LZTRK,LVTXT,VTXT_ID,UDST_VTXT_LINK,I
C----------------------------------------------------------------------
      UDST_VTXT_LINK=0    ! initaily no link has been found
C
C... check LZTRK > 0
      IF (LZTRK.LE.0) GOTO 999
C
C... check that there is a link to VTXT
      LVTXT=LQ(LZTRK-6)
      IF (LVTXT.LE.0) GOTO 999
C
C... found a linked VTXT
      VTXT_ID=IQ(LVTXT-5)
C
C... loop over the table of VTXT objects to find the position of this VTXT
      DO I=1,N_VTXT_MAX
        IF (VTXT_ID.EQ.VTXT_NUMBER(I)) THEN
          UDST_VTXT_LINK=I
          GOTO 999
        ENDIF
      ENDDO
C... the bank is not in the table - this should not happen
      CALL ERRMSG('VTXT not found','UDST_GET_VTXT_LINK','notify author',
     &  'W')
C----------------------------------------------------------------------
  999 RETURN
      END
