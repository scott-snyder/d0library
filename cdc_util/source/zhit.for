C----------------------------------------------------------------------
      REAL FUNCTION ZHit(IMask)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   9-NOV-1993   Gregory L. Landsberg
C-   Updated  27-Jan-1996   sss - compile with g77.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      Include      'D0$INC:ZEBCOM.INC'
      Integer       GZDHIT, LDHIT, IMask, I
C
      integer mask_0003fffe
      parameter (mask_0003fffe = '0003fffe'X)
C
      LDHIT = GZDHIT()
      If (LDHIT .le. 0) Go To 1
      Do I = 1,IQ(LDHIT+2)
        If (IAND(IQ(LDHIT+IQ(LDHIT+3)*(I-1)+4),mask_0003FFFE)
     &    .eq. IMask) Then
          ZHit = IBITS(IQ(LDHIT+IQ(LDHIT+3)*(I-1)+5),16,16)/100.
          Return
        End If
      End Do
    1 ZHit = 99.99
      Call ERRMSG('CAPHEL','ZHIT','Hit not found in the DHIT bank','W')
C----------------------------------------------------------------------
  999 RETURN
      END
