      FUNCTION filter_bits(pass,nstring,filter_names)

C----------------------------------------------------------------------
C-   Purpose and Methods :  Use triggers defined in array filter_names to
C-        generate a specialized bit mask which is independent of trigger
C-        version number.
C-
C-   Inputs  :
C-        pass                L   if .true. then set bit if filter called and 
C-                                passed, otherwise, set bit only if filter 
C-                                called
C-        nstring             I   number of filter names
C-        filter_names(128)   C   list of names of filters to check
C-
C-   Outputs  :
C-        filter_bits         I   value returned is bit pattern giving which L2
C-                                trigger passed in the order specified in
C-                                filter_names
C-
C-   Created  Feb-20-1994   Bob Kehoe
C-   Updated  Jun-16-1994   Bob Kehoe  --  deal with problem of unfilled FILT
C-                                         banks for showerlibrary
C-   Updated  Oct-25-1994   Bob Kehoe  --  allow caller to switch with pass
C-                                         parameter
C----------------------------------------------------------------------

      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER k,l,nstring,filter_bits
      INTEGER ntrigon,nfilton,trigbon(32),filtbon(128)
      INTEGER bit,bitnum
      CHARACTER*32 filter_names(128),trignon(32),filtnon(128)
      LOGICAL l2name_passed,pass,bit_passed,found

C----------------------------------------------------------------------
      filter_bits = 0

      IF (pass) THEN
        DO k = 1,nstring
          IF (l2name_passed(filter_names(k))) THEN
            filter_bits = filter_bits + 1*(2**(k-1))
          ENDIF
        ENDDO
      ELSE
        bit_passed = 0
        DO k = 1,nstring
          CALL gttsum(ntrigon,trigbon,trignon,nfilton,filtbon,filtnon)
          CALL get_l2_bit_number(filter_names(k),bitnum,found)
          IF (found) THEN
            IF (lhead.GT.0) THEN
              l = int(bitnum/32)
              bit = mod(bitnum,32)
              bit_passed = btest(iq(lhead+15+l),bit)
            ELSE
              CALL errmsg('HEAD bank error','filter_bits',
     &            'no HEAD bank','W')
            ENDIF
          ENDIF
          IF (bit_passed) filter_bits = filter_bits + 1*(2**(k-1))
        ENDDO
      ENDIF

  999 RETURN
      END
