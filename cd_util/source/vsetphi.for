      SUBROUTINE VSETPHI
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : set VPHIBIT in IQ(LVTXT+1) to indicate that VTXT
C-                         is in phi road, so it will not be dropped from DST
C-                         by ZDROP_TRACKS
C-   Inputs  :      road phi information for PELC, PPHO, PTAU,
C-                  phi of ZTRK for PMUO
C-   Outputs :      VPHIBIT bit in IQ(LVTXT+1) is set for VTXT in phi roads
C-   Controls:
C-
C-   Created  24-SEP-1993   Liang-ping Chen
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$PARAMS:ZTRAKS.PARAMS'
      INCLUDE 'D0$INC:PI.DEF/LIST'
      INCLUDE 'D0$LINKS:IZPELC.LINK'
      INTEGER IBSET
      INTEGER LPARH, LVTRH, LVTXT
      INTEGER GZPARH, GZVTRH
      INTEGER LPELC, LPPHO, LHMTE, LHMTP, LPTAU
      INTEGER GZPPHO, GZPELC, GZPTAU
      INTEGER LPMUO, LZTRK, LZFIT
      INTEGER GZPMUO
      INTEGER NVPELC, NVPPHO, NVPTAU, NVPMUO        
      REAL    PHIMIN, PHIMAX
      REAL    PHIMEAN, NEWPHIMIN, NEWPHIMAX         
      INTEGER NVCUT/3/
      REAL    PHIWID/0.020/
      LOGICAL VINPHI
      LOGICAL FIRST/.TRUE./, EZERROR
      INTEGER IER
C
      IF (FIRST) THEN
        CALL EZPICK('VTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('ZTRAKS','VPHIBIT',
     &    'Unable to find bank VTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('PHIWID',PHIWID,IER)
        CALL EZRSET
        FIRST=.FALSE.
      ENDIF
C
      LVTRH=GZVTRH()
      IF (LVTRH.LE.0) GO TO 999
C
C     set VTXRPHI bit for all VTXT which are in phi window of PELEC roads
C 
      LPELC = GZPELC()
      DO WHILE(LPELC.GT.0) 
        LHMTE=LQ(LPELC-1)
        PHIMIN=Q(LHMTE+9)
        PHIMAX=Q(LHMTE+10)
        PHIMEAN= .5*(PHIMIN+PHIMAX)    
        NEWPHIMIN=PHIMEAN-PHIWID
        NEWPHIMAX=PHIMEAN+PHIWID
C
        NVPELC=0 
        LVTXT=LQ(LVTRH-1)
        DO WHILE (LVTXT.GT.0)
          IF (VINPHI(LVTXT, NEWPHIMIN, NEWPHIMAX)) THEN  
            NVPELC=NVPELC+1
            IF (NVPELC.GT.NVCUT) GOTO 25 
            IQ(LVTXT+1) = IBSET(IQ(LVTXT+1),VPHIBIT)
          ENDIF
          LVTXT=LQ(LVTXT)
        ENDDO
25      LPELC=LQ(LPELC)
      ENDDO
C
C     set VTXRPHI bit for all VTXT which are in phi window of PPHO roads
C 
50    LPPHO = GZPPHO()
      DO WHILE(LPPHO.GT.0) 
        LHMTP=LQ(LPPHO-1)
        PHIMIN=Q(LHMTP+9)
        PHIMAX=Q(LHMTP+10)
        PHIMEAN= .5*(PHIMIN+PHIMAX)    
        NEWPHIMIN=PHIMEAN-PHIWID
        NEWPHIMAX=PHIMEAN+PHIWID
C       
        NVPPHO=0
        LVTXT=LQ(LVTRH-1)
        DO WHILE (LVTXT.GT.0)
          IF (VINPHI(LVTXT, NEWPHIMIN, NEWPHIMAX)) THEN  
            NVPPHO=NVPPHO+1
            IF (NVPPHO.GT.NVCUT) GOTO 75
            IQ(LVTXT+1) = IBSET(IQ(LVTXT+1),VPHIBIT)
          ENDIF
          LVTXT=LQ(LVTXT)
        ENDDO
75      LPPHO=LQ(LPPHO)
      ENDDO
C
C     set VTXRPHI bit for all VTXT which are in phi window of PTAU roads
C 
100   LPTAU = GZPTAU()
      DO WHILE(LPTAU.GT.0) 
        PHIMEAN=Q(LPTAU+9)
        NEWPHIMIN=PHIMEAN-PHIWID
        NEWPHIMAX=PHIMEAN+PHIWID
C 
        NVPTAU=0
        LVTXT=LQ(LVTRH-1)
        DO WHILE (LVTXT.GT.0)
          IF (VINPHI(LVTXT, NEWPHIMIN, NEWPHIMAX)) THEN  
            NVPTAU=NVPTAU+1
            IF (NVPTAU.GT.NVCUT) GOTO 125
            IQ(LVTXT+1) = IBSET(IQ(LVTXT+1),VPHIBIT)
          ENDIF
          LVTXT=LQ(LVTXT)
        ENDDO
125     LPTAU=LQ(LPTAU)
      ENDDO
         

C
C     set VTXRPHI bit for all VTXT which are in phi window of ZTRK of PMUO
C                                                              roads
C
200   LPMUO=GZPMUO(0)
      DO WHILE(LPMUO.GT.0) 
        LZTRK  =LQ(LPMUO-5)
        IF (LZTRK.GT.0) THEN 
          LZFIT  =LQ(LZTRK-1 )
          PHIMEAN= Q(LZFIT+10)
        ELSE
          GOTO 295      
        ENDIF  
        NEWPHIMIN=PHIMEAN-PHIWID
        NEWPHIMAX=PHIMEAN+PHIWID
C
        NVPMUO=0
        LVTXT=LQ(LVTRH-1)
        DO WHILE (LVTXT.GT.0)
          IF (VINPHI(LVTXT, NEWPHIMIN, NEWPHIMAX)) THEN  
            NVPMUO=NVPMUO+1
            IF (NVPMUO.GT.NVCUT) GOTO 295
            IQ(LVTXT+1) = IBSET(IQ(LVTXT+1),VPHIBIT)
          ENDIF
          LVTXT=LQ(LVTXT)
        ENDDO
295     LPMUO=LQ(LPMUO)
      ENDDO

C----------------------------------------------------------------------
  999 RETURN
      END
