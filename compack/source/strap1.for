      FUNCTION STRAP1(STR1, STR2)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-    Append STR1 to STR2 and store the resulting string with STRSTO.
C-
C-   Returned value  :
C-    The cookie of the stored string.
C-    
C-   Inputs  :
C-    STR1, STR2 : The two strings to concatenate.
C-
C-   Created  16-MAY-1991   Scott Snyder
C-   Updated  12-DEC-1991   Herbert Greenlee
C-     UNIX compatible version
C-   Modified 14-AUG-1992   sss - fix unix version
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER STRAP1
      CHARACTER*(*) STR1, STR2
C
C&IF VAXVMS
      INTEGER  STRSTO
      EXTERNAL STRSTO
C&ELSE
C&      integer dsc(2)
C&      integer len1, len2, len, ptr, istat
C&      integer  trulen, lib$get_vm
C&      external trulen, lib$get_vm
C&ENDIF
C----------------------------------------------------------------------
C&IF VAXVMS
      STRAP1 = STRSTO(STR1//STR2)
C&ELSE
C&      strap1 = 0
C&      len1 = trulen (str1)
C&      len2 = trulen (str2)
C&      len = len1 + len2
C&      if (len .gt. 0) then
C&        dsc(1) = len
C&        istat = lib$get_vm (len+8, ptr)
C&        if (.not. istat) then
C&          call intmsg (' strap1: out of dynamic memory')
C&          call lib$stop (%val(istat))
C&        endif
C&        dsc(2) = ptr + 8
C&        if (len1 .gt. 0) call strcop (%ref(str1), %val(dsc(2)),
C&     &                                %val(len1), %val(len1))
C&        if (len2 .gt. 0) call strcop (%ref(str2), %val(dsc(2)+len1),
C&     &                                %val(len2), %val(len2))
C&        call strdcp (dsc(1), %val(ptr))
C&        strap1 = ptr
C&      endif
C&ENDIF
  999 RETURN
      END
