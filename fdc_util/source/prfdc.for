      SUBROUTINE PRFDC(PRUNIT,IFL)
C------------------------------------------------------------------
C-
C-    Purpose and Methids : Prints out all FDC hit banks
C-
C-    Input : PRUNIT = Unit number for printout
C-            IFL    = 0         no printout
C-            IFL    = 1         no. of hits per layer
C-            IFL    = 2         no. of hits per bank in structure
C-            IFL    = 3         full printout
C-
C-   Created  xx-JAN-1987   Daria Zieminska
C-   Updated  21-OCT-1988   Jeffrey Bantly  modified for new hit format
C-   Updated  20-MAR-1990   Jeffrey Bantly  use logical format 
C-   Updated  29-APR-1991   Jeffrey Bantly  use new RCP,PARAMS 
C-
C--------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$LINKS:IZCDD3.LINK/LIST'
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$INC:QUEST.INC/LIST'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      INTEGER PRUNIT,LTOPB,NHITS,NFDCH,IFL,ICALL,IER
      INTEGER LKFDCH,LKFHLF,LKFTHE,LKFTQD,LKFTSC,LKFPHI
      INTEGER LKFPSC,LKFTDA,LKFPDA,LCDD3
      INTEGER IDEN,HALF,QUAD,SECTP,SECTT
      INTEGER GZFDCH,GZFHLF,GZFDUN,GZFTQD,GZFTSC,GZFTDA
      INTEGER GZFPSC,GZFPDA
C
      CHARACTER*3 CFL
C
      SAVE ICALL,CFL
      DATA CFL/'ONE'/
      DATA ICALL/0/
C-------------------------------------------------------------------------
C
      IF ( ICALL .EQ. 0 ) THEN
        ICALL=1
C        CALL EZPICK('FTRAKS_RCP')
C        CALL EZRSET
      ENDIF
C
      IF ( IFL .LE. 0 ) GOTO 999
      LKFDCH=GZFDCH()
C
C   Print out contents of bank FDCH
C
      IF (LKFDCH .LE. 0) GO TO 999               ! FDCH not booked
      IDEN=IQ(LKFDCH-5)
      CALL PRFDCH(PRUNIT,LKFDCH,IDEN,CFL,IFL)
C
C   Print out contents of banks hanging from FDCH
C
      DO 100 HALF=0,1
        LKFHLF=GZFHLF(HALF)
        IF (LKFHLF .LE. 0) GO TO 100
        IDEN=IQ(LKFHLF-5)
        IF(IQ(LKFHLF+1).GE.1) CALL PRFHLF(PRUNIT,LKFHLF,IDEN,CFL,IFL)
C
C  Print out theta layers
C
        LKFTHE=GZFDUN(HALF,0)
        IF (LKFTHE .LE. 5) GO TO 101
        IDEN=IQ(LKFTHE-5)
        IF(IQ(LKFTHE+1).GE.1) CALL PRFDUN(PRUNIT,LKFTHE,IDEN,CFL,IFL)
        IF ( IFL .LE. 1 ) GOTO 101
        DO 200 QUAD=0,7
          LKFTQD=GZFTQD(HALF,QUAD)
          IF (LKFTQD .LE. 5) GO TO 200
          IDEN=IQ(LKFTQD-5)
          IF(IQ(LKFTQD+1).GE.1) CALL PRFTQD(PRUNIT,LKFTQD,IDEN,CFL,IFL)
          DO 202 SECTT=0,MXSECT
            LKFTSC=GZFTSC(HALF,QUAD,SECTT)
            IF (LKFTSC .LE. 5) GO TO 202
            IDEN=IQ(LKFTSC-5)
            IF(IQ(LKFTSC+1).GE.1)
     &                           CALL PRFTSC(PRUNIT,LKFTSC,IDEN,CFL,IFL)
            LKFTDA=GZFTDA(HALF,QUAD,SECTT)
            IF ( LKFTDA .LE. 5 ) GO TO 202
            IDEN=IQ(LKFTDA-5)
            IF(IQ(LKFTDA+1).GE.1)
     &                           CALL PRFTDA(PRUNIT,LKFTDA,IDEN,CFL,IFL)
  202     CONTINUE
  200   CONTINUE
C
C  Print out phi layers
C
  101   CONTINUE
        LKFPHI=GZFDUN(HALF,1)
        IF (LKFPHI .LE. 5) GO TO 100
        IDEN=IQ(LKFPHI-5)
        IF(IQ(LKFPHI+1).GE.1) CALL PRFDUN(PRUNIT,LKFPHI,IDEN,CFL,IFL)
        IF ( IFL .LE. 1 ) GOTO 100
        DO 300 SECTP=0,MXSECP
          LKFPSC=GZFPSC(HALF,SECTP)
          IF (LKFPSC .LE. 5) GO TO 300
          IDEN=IQ(LKFPSC-5)
          IF(IQ(LKFPSC+1).GE.1) CALL PRFPSC(PRUNIT,LKFPSC,IDEN,CFL,IFL)
          LKFPDA=GZFPDA(HALF,SECTP)
          IF ( LKFPDA .LE. 5 ) GO TO 300
          IDEN=IQ(LKFPDA-5)
          IF(IQ(LKFPDA+1).GE.1) CALL PRFPDA(PRUNIT,LKFPDA,IDEN,CFL,IFL)
  300   CONTINUE
  100 CONTINUE
C-------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
