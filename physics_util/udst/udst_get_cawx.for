      SUBROUTINE UDST_GET_CAWX(LCLUS,NCAWX,CAWXCELLS,CAWXDATA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return CAWX info for a PELC or PPHO object
C-
C-   Inputs  : LCLUS     - link to PELC or PPHO
C-   Outputs : NCAWX     - number of cells in cluster
C-             CAWXCELLS - array of packed addresses of cells
C-             CAWXDATA  - array of cell energies (GeV)
C-   Controls: 
C-
C-   Created   3-DEC-1993   Ian Adam
C-   Updated  14-OCT-1995   Ian Adam  CASH -> CAWX 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'      
      INCLUDE 'D0$LINKS:IZCAWX.LINK'

      INTEGER     LCLUS,LCACL,LCAWX,LCASH
      INTEGER     J,NCAWX
      INTEGER     N_CSH_MAX
      PARAMETER(N_CSH_MAX=2000)
      REAL        CAWXDATA(N_CSH_MAX)
      INTEGER     CAWXCELLS(N_CSH_MAX)
C----------------------------------------------------------------------
      LCACL=LQ(LCLUS-2)
      IF (LCACL.LE.0) THEN
        CALL ERRMSG('NO CACL BANK','UDST_GET_CAWX',' ','W')
        GOTO 999
      ENDIF
      LCASH=LQ(LCACL-2)
      IF (LCASH.LE.0) THEN
        CALL ERRMSG('NO CASH BANK','UDST_GET_CAWX',' ','W')
        GOTO 999
      ENDIF
      LCAWX = LQ(LCASH-IZCAWX)
      IF (LCAWX.LE.0) THEN
        CALL ERRMSG('NO CAWX BANK','UDST_GET_CAWX',' ','W')
        GOTO 999
      ENDIF

      NCAWX=IQ(LCAWX+2)
      IF (NCAWX.GT.N_CSH_MAX) THEN
        CALL ERRMSG('CAWX','UDST_GET_CAWX','TOO MANY CELLS','W')
        GOTO 999
      ENDIF

      DO J=1,NCAWX
        CAWXCELLS(J)=IQ(LCAWX+2*J+1)
        CAWXDATA(J)=Q(LCAWX+2*J+2)
      ENDDO

  999 RETURN
      END
