      SUBROUTINE BKL2CH()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book header bank L2CH for level-2 CD
C-   hit-finding results. L2CH hangs from the filter result bank, FRES.
C-
C-   Inputs  : FRES bank
C-   Outputs : L2CH bank
C-   Controls: 
C-
C-   Created  26-APR-1993   Chris Klopfenstein
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZL2CH.LINK/LIST'
      INTEGER MPL2CH(5), ISETVN
      integer L2CH, gzfres, FRES
      integer n_links, n_struc, n_data, type
      parameter (n_struc = 5)   ! 1 per detector + spare
      parameter (n_links = n_struc)
      parameter (n_data = 4)    ! 1 data word / detector
      parameter (type = 1)      ! bank data type is bit string
      LOGICAL FIRST
      character*4 string
      data string / 'L2CH' /
      DATA FIRST / .TRUE. /
C
C======================================================================
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL UCTOH(string, MPL2CH, 4, 4)
        MPL2CH(2) = n_links
        MPL2CH(3) = n_struc
        MPL2CH(4) = n_data
        MPL2CH(5) = type
      ENDIF
C        
      fres = GZFRES()
      IF ( fres .le. 0 ) call bkfres(fres)
C
C ****  Book L2CH
C
      L2CH = LQ(FRES - IZL2CH)
      IF ( L2CH .EQ. 0 ) THEN
        CALL MZLIFT ( IXMAIN, L2CH, FRES, -IZL2CH, MPL2CH, 3 )
      ENDIF
C
      IQ(L2CH) = ISETVN(IQ(L2CH),0)
  999 RETURN
      END
