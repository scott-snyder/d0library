      SUBROUTINE UDST_GET_CASH(LCLUS,NCASH,CASHCELLS,CASHDATA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return CASH info for a PELC or PPHO object
C-
C-   Inputs  : LCLUS     - link to PELC or PPHO
C-   Outputs : NCASH     - number of cells in cluster
C-             CASHCELLS - array of packed addresses of cells
C-             CASHDATA  - array of cell energies (GeV)
C-   Controls: 
C-
C-   Created   3-DEC-1993   Ian Adam
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      
      INTEGER     LCLUS,LCACL,LCASH
      INTEGER     J,NCASH
      INTEGER     N_CSH_MAX
      PARAMETER(N_CSH_MAX=2000)
      REAL        CASHDATA(N_CSH_MAX)
      INTEGER     CASHCELLS(N_CSH_MAX)
C----------------------------------------------------------------------
      LCACL=LQ(LCLUS-2)
      IF (LCACL.LE.0) THEN
        CALL ERRMSG('NO CACL BANK','UDST_GET_CASH',' ','W')
        GOTO 999
      ENDIF
      LCASH=LQ(LCACL-2)
      IF (LCASH.LE.0) THEN
        CALL ERRMSG('NO CASH BANK','UDST_GET_CASH',' ','W')
        GOTO 999
      ENDIF

      NCASH=IQ(LCASH+2)
      IF (NCASH.GT.N_CSH_MAX) THEN
        CALL ERRMSG('CASH','UDST_GET_CASH','TOO MANY CELLS','W')
        GOTO 999
      ENDIF

      DO J=1,NCASH
        CASHCELLS(J)=IQ(LCASH+2*J+1)
        CASHDATA(J)=Q(LCASH+2*J+2)
      ENDDO

  999 RETURN
      END
