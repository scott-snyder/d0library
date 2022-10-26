      SUBROUTINE MULIB_MATCH_KEY(LISP1,KEY1,CYC_MAX,CYC,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Unclear
C-
C-   Inputs  : LISP1
C-             KEY(4) [I]
C-             CYC_MAX
C-
C-   Outputs : CYC
C-             IER 0=ok
C-   Controls:
C-
C-   Created  29-APR-1993   W. Dharmaratna, Chip Stewart, Jasbir Singh
C-   Updated  24-MAR-2004   sss - compile with g77.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:MULCON.INC'
      INCLUDE 'D0$INC:MULDAT.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:GCUNIT.INC'
      INCLUDE 'D0$INC:PI.DEF'
C
      INTEGER LISP1,LISV1
C
      INTEGER IER,MIN_CYC,NT
      LOGICAL FIRST,OK
      DATA FIRST/.TRUE./
      INTEGER CYC,CYC_MAX,KEY1(NKEY),NTRIES_MAX,IETAM,IPHIM
      REAL    RNDM,MIN_RESIDUAL,RESIDUAL,XYZ_ISP1(3),VTX(3),PP
      REAL    DIR(3),PTRK,ETRK,PLIB(4)
      REAL DIFF_PHI
C
      INTEGER RKEY(NKEY)
C
      INTEGER I,J,K,N
      INTEGER NVTXG,JQ
      INTEGER PRD_KEY,MUON_KEY
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('MUONLIBRARY_RCP')
        CALL EZGET('NTRIES_MATCH',NTRIES_MAX,IER)
        CALL EZRSET
      ENDIF
      CALL UCOPY_i(KEY1,RKEY,NKEY)
      NT = MIN(CYC_MAX,NTRIES_MAX)
      MIN_RESIDUAL = 1.E10
      LISV1 = LQ(LISP1+1)
      DO N = 1, NT
        CYC = N
C
C ****  NOW TO SORT ON KEYS AND DO RZVIN.
C
        CALL MUON_RZVIN1(MULIB,NMULIB,NDATA,RKEY,CYC,' ')
        IF (IQUEST(1).NE.0.OR.IQUEST(6).NE.CYC)THEN
          CALL ERRMSG('MUONLIBRARY','MULIB_MATCH_KEY',
     &        'ERROR DURING MUON_RZVIN1','W')
          WRITE(LOUT,605)RKEY,CYC
C&IF LINUX
C&  605     FORMAT(' *******  Error during RZ READ ******',(I7))
C&ELSE
  605     FORMAT(' *******  Error during RZ READ ******',(I))
C&ENDIF
          DO 606 JQ=1,10
            WRITE(LOUT,607)IQUEST(JQ)
  606     CONTINUE
  607     FORMAT(I8)
        ELSE
C
C****  Find eta, phi for each stored track and compare with ISAJET track
C
          CALL UCOPY(MULIB(13),VTX(1),3)
          CALL UCOPY(MULIB(6),PLIB(1),4)
          PP = SQRT(PLIB(1)*PLIB(1)+PLIB(2)*PLIB(2)+PLIB(3)*PLIB(3))
          DIR(1) = PLIB(1)/PP
          DIR(2) = PLIB(2)/PP
          DIR(3) = PLIB(3)/PP
          CALL MUON_CLINPH_FAST(VTX,DIR,ESTO,PSTO,IETAM,IPHIM,OK)
          PTRK = PHIM_PRIMARY               ! PHI OF CURRENT TRACK
          ETRK = ETAM_PRIMARY               ! ETA OF CURRENT TRACK
          RESIDUAL = DIFF_PHI(PTRK,PSTO)
          RESIDUAL = SQRT(RESIDUAL**2+ (ESTO-ETRK)**2)
          IF (RESIDUAL.LT.MIN_RESIDUAL) THEN
            MIN_CYC = CYC
            MIN_RESIDUAL = RESIDUAL
          ELSE
         END IF
        ENDIF
      ENDDO
 990  CONTINUE
      CYC = MIN_CYC
  999 RETURN
      END
