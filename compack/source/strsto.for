      FUNCTION STRSTO(STR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-    Store string STR somewhere and return a cookie identifying it.
C-    The only valid operations on the cookie are to pass it to STRFET,
C-    STRAPP, or STRFRE.
C-
C-    This version stores the string in memory allocated by LIB$GET_VM.
C-    The first eight bytes of each block contain a string descriptor,
C-    which points to the remainder of the block.
C-
C-   Returned value  : An opaque integer*4 cookie.
C-   Inputs  :
C-    STR : The string to be stored.
C-
C-   Created  16-MAY-1991   Scott Snyder
C-   Updated  18-SEP-1991   Herbert Greenlee
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER STRSTO
      CHARACTER*(*) STR
C&IF VAXVMS
      INCLUDE '($DSCDEF)'
      RECORD /DSCDEF1/ DSC
C&ELSE
C&      integer dsc(2)
C&ENDIF
      INTEGER LEN
C&IF ALFOSF
C&      INTEGER*8 PTR
C&ELSE
      INTEGER PTR
C&ENDIF
      LOGICAL ISTAT
      INTEGER  TRULEN
      LOGICAL  LIB$GET_VM
      EXTERNAL TRULEN, LIB$GET_VM
C----------------------------------------------------------------------
      STRSTO = 0
      LEN = TRULEN(STR)
      IF (LEN .GT. 0) THEN
C&IF VAXVMS
        DSC.DSC$W_MAXSTRLEN = LEN
        DSC.DSC$B_DTYPE = DSC$K_DTYPE_T
        DSC.DSC$B_CLASS = DSC$K_CLASS_S
        ISTAT = LIB$GET_VM(LEN + DSC$K_S_BLN, PTR)
        IF (.NOT. ISTAT) THEN
          CALL LIB$SIGNAL(%VAL(ISTAT))
        ELSE
          DSC.DSC$A_POINTER = PTR + DSC$K_S_BLN
          CALL STRCOP(STR, DSC)
          CALL STRDCP(DSC, %VAL(PTR))
          STRSTO = PTR
        ENDIF
C&ELSE
C&        dsc(1) = len
C&        istat = lib$get_vm(len+8, ptr)
C&        if(.not.istat)then
C&          call intmsg(' strsto: out of dynamic memory')
C&          call lib$stop(%val(istat))
C&        endif
C&        dsc(2) = ptr + 8
C&        call strcop(%ref(str), %val(dsc(2)), %val(len), %val(len))
C&        call strdcp(dsc(1), %val(ptr))
C&        strsto = ptr
C&ENDIF
      ENDIF
  999 RETURN
      END
