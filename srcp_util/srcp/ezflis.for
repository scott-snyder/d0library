      PROGRAM EZFLIS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-   Produce a list of the parameter names in an RCP text file.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  23-NOV-1988   Harrison B. Prosper
C-   Updated  12-Feb-1992   Herbert Greenlee
C-      UNIX version.  Changed OPEN to D0OPEN.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*32 PAR(5000)
      CHARACTER*48 INFIL,OUTFIL
      INTEGER      I,J,K,L,M,N,NPAR,LUN
      PARAMETER( LUN  = 20 )
      LOGICAL OK
C----------------------------------------------------------------------
C
C ****  Initialize ZEBRA
C
      CALL MZEBRA (0)
C
C ****  Initialize STP structure
C
      CALL INZSTP
C
C ****  Get array names
C
      WRITE(6,50)
   50 FORMAT(1X,/,' Enter name of text file to be listed  : ',$)
      READ (5,60) INFIL
   60 FORMAT(A48)
C
      WRITE(6,70)
   70 FORMAT(     ' Name of output file : ',$)
      READ (5,60) OUTFIL
C
      CALL D0OPEN(LUN,INFIL,'I',OK)
      IF(.NOT.OK)CALL D0_ABORT(' D0OPEN failed')
      CALL EZPAR  (LUN,'ALL',PAR,NPAR)
      CLOSE(UNIT=LUN)
C
      WRITE(6,75)
   75 FORMAT(' Working...'/)
      CALL D0OPEN(LUN,OUTFIL,'OF',OK)
      IF(.NOT.OK)CALL D0_ABORT(' D0OPEN failed')
      WRITE(LUN,80) OUTFIL
   80 FORMAT(1X,' Filename: ',A48/)
C
      WRITE(LUN,100) INFIL
  100 FORMAT(1X,' Parameter list for file ',A48)
      WRITE(LUN,110) (I,PAR(I),I=1,NPAR)
  110 FORMAT(2(2X,I5,1X,A32))
      CLOSE(UNIT=LUN)
C
      END
