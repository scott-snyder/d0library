      SUBROUTINE SHLB_ELEC_MATCH_KEY(LISP1,KEY1,CYC_MAX,CYC,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Unclear
C-
C-   Inputs  : LISP1
C-             KEY(5) [I]
C-             CYC_MAX
C-             
C-   Outputs : CYC
C-             IER 0=ok
C-   Controls: 
C-
C-   Created  29-APR-1993   W. Dharmaratna, Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:SHLCON.INC'
      INCLUDE 'D0$INC:SHLDAT.INC'
      INCLUDE 'D0$INC:SHSORT.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:GCUNIT.INC'
      INCLUDE 'D0$INC:PI.DEF'
C
      INTEGER LISP1,LISV1
      INCLUDE 'D0$INC:SHLITR.INC'
C
      INTEGER IER,MIN_CYC,NT
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      INTEGER CYC,CYC_MAX,KEY1(NKEY),NTRIES_MAX
      REAL    RNDM,MIN_RESIDUAL,RESIDUAL,XYZ_ISP1(3),XYZ_SHLB(3)
      REAL    DIR(3),VTX(3),P4(4),IPSTO,IPTRK,PHI_ROT
C
      INTEGER RKEY(NKEY)
C
      INTEGER I,J,K,N
      INTEGER NVTXG,JQ
      INTEGER PRD_KEY,INDX_KEY
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('SHOWERLIBRARY_RCP')
        CALL EZGET('NTRIES_ELECTRON',NTRIES_MAX,IER)
        CALL EZRSET
      ENDIF
      CALL UCOPY_i(KEY1,RKEY,NKEY)
      NT = MIN(CYC_MAX,NTRIES_MAX)
      MIN_RESIDUAL = 1.E10
      LISV1 = LQ(LISP1+1)
      IF(LISV1.GT.0)CALL UCOPY(Q(LISV1+7),VTX(1),3)
      CALL UCOPY(Q(LISP1+2),P4(1),4)
      DO 10, I = 1, 3
   10 P4(I) = P4(I)/P4(4)  !ELECTRON MASS SMALL
      DO N = 1, NT
        CYC = N
C
C ****  NOW TO SORT ON KEYS AND DO RZVIN.
C
        CALL RZVIN1(SHLB,NSHLB,NDATA,RKEY,CYC,' ')
        IF (IQUEST(1).NE.0.OR.IQUEST(6).NE.CYC)THEN
          CALL ERRMSG('SHOWERLIBRARY','SHLB_ELEC_MATCH_KEY',
     &        'ERROR DURING RZVIN1','W')
          WRITE(LOUT,605)RKEY,CYC
  605     FORMAT(' *******  Error during RZ READ ******',(I8))
          DO 606 JQ=1,10
            WRITE(LOUT,607)IQUEST(JQ)
  606     CONTINUE
  607     FORMAT(I8)
        ELSE
          CALL UCOPY(SHLB(16),XYZ_SHLB(1),3)
C
C****  Rotate phi as done for cells
C
          IPSTO = SHLB(11)                  ! PHI OF STORED TRACK
          IPTRK = IPHIC_PRIMARY             ! PHI OF CURRENT TRACK
          PHI_ROT =  (IPSTO-IPTRK)*TWOPI/64
          DIR(1) = P4(1)*COS(PHI_ROT)-P4(2)*SIN(PHI_ROT)
          DIR(2) = P4(1)*SIN(PHI_ROT)+P4(2)*COS(PHI_ROT)
          DIR(3) = P4(3)
          CALL CLINPH_FAST_XYZ(VTX,DIR,XYZ_ISP1,IER)
          RESIDUAL = SQRT((XYZ_SHLB(1)-XYZ_ISP1(1))**2
     &      +(XYZ_SHLB(2)-XYZ_ISP1(2))**2         ! check only phi
     &      +(ABS(XYZ_SHLB(3))-ABS(XYZ_ISP1(3)))**2)
          IF (RESIDUAL.LT.MIN_RESIDUAL) THEN
            MIN_CYC = CYC
            MIN_RESIDUAL = RESIDUAL
          END IF
        ENDIF
      ENDDO
      CYC = MIN_CYC
  999 RETURN
      END
