
      SUBROUTINE VPKPHI(LZTRK,PT_ZTRK, LVTXT, NVTXT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : find the VTXT which is the closest in phi to a ZTRK,
C-                         among those in a PT dependent phi window.     
C-   Inputs  :    LZTRK    Pointer to the ZTRK
C-                PT_ZTRK  transverse momentum of the ZTRK, it is used for the 
C-                         phi window definetion for VTXT candidates.
C-                         Minimun of 1 GeV is used for this purpose here.
C-                          
C-   Outputs :    LVTXT    pointer to the VTXT which is the closest in phi to 
C-                         the ZTRK
C-                NVTXT    Number of VTXT in the PHI window
C-   Controls:    NONE 
C-
C-   created 29-SEP-1993    Liang-Ping Chen   
C-   Updated  11-JUL-1994   Liang-ping Chen  use MINENDSEG rather than MINPHI,
C-                          add NVTX, comments; set a minimun for PT.
C-   Updated  12-OCT-1994   Liang-Ping Chen  special treatment for LZTRK associated
C-                          to PELC, return LVTXT which match with
C-                          the cluster the best. NVTXT returns the number 
C-                          of VTXT in a IMPCUT window.
C-- --------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER LZTRK
      REAL    PT_ZTRK, PT
      INTEGER LVTXT,LDTRK,LFDCT,LVTXTSV, NVTXT
      REAL    PHIV,XV,YV,RV,MINENDSEG, RENDSEG, XZ,YZ,PHIZ
      REAL    ENDSEG1
      LOGICAL OK,FIRST
      INTEGER IER, LRCP
      INTEGER GZVTXT
      INTEGER LPELC          
      CHARACTER*4 BANK
      REAL    IMP, MINIMP, IMPCUT/6./
      REAL    EMX, EMY, EMR     
      DATA    FIRST/.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        CALL EZLOC('ZTRAKS_RCP', LRCP) ! for the sake of CVRPHI, FVRPHI below
        IF (LRCP.EQ.0) THEN  
          CALL INRCP('ZTRAKS_RCP',IER)
        ENDIF
        FIRST=.FALSE.
      ENDIF 
C
C    for ZTRK associated with PELC, match with the cluster centers, 
C    use the r phi impact parameter of VTXT to the cluster center
C     
      LVTXTSV=0
      LVTXT=0
      NVTXT=0
      LPELC=LQ(LZTRK-4)
      IF (LPELC.LE.0) GOTO 100
      CALL UHTOC(IQ(LPELC-4),4,BANK,4)
      IF (BANK .NE.'PELC') GOTO 100
      EMX=Q(LPELC+23)  
      EMY=Q(LPELC+24)  
      EMR=SQRT(EMX**2+EMY**2)

      LVTXT = GZVTXT(0)
      MINIMP = 999.
      DO WHILE (LVTXT .GT. 0)
        PHIV = Q(LVTXT+6)
        XV  =  Q(LVTXT+7)     
        YV  =  Q(LVTXT+8)
        RV  = SQRT(XV**2 + YV**2 )
        IF (EMX*XV+EMY*YV.GT.0.8*EMR*RV) THEN
          IMP= ABS( (EMY - YV)*COS(PHIV) -
     &              (EMX - XV)*SIN(PHIV)   )

          IF (IMP.LT.IMPCUT) THEN
            NVTXT=NVTXT+1
            IF (IMP.LT.MINIMP) THEN
              MINIMP=IMP 
              LVTXTSV = LVTXT
            ENDIF
          ENDIF
        ENDIF 
        LVTXT = LQ(LVTXT)
      ENDDO
      LVTXT = LVTXTSV
      GOTO 999
C
C    for ZTRK associated to other objects, match with DTRK/FDCT,
C    since the objects do not provide a better phi for VTXT to match
C
100   LDTRK=LQ(LZTRK-7)
      LFDCT=LQ(LZTRK-8)
      IF (LDTRK.GT.0.AND.LFDCT.LE.0) THEN
        PHIZ = Q(LDTRK+6)
        XZ = Q(LDTRK+7)
        YZ = Q(LDTRK+8)
      ELSEIF (LDTRK.LE.0.AND.LFDCT.GT.0) THEN 
        PHIZ = Q(LFDCT+6)
        XZ  = Q(LFDCT+4)
        YZ  = Q(LFDCT+5) 
      ELSE
        LVTXT=0
        GOTO 999
      ENDIF  
      LVTXT = GZVTXT(0)
      MINENDSEG = 999.
      PT=AMAX1(PT_ZTRK, 1.)
      NVTXT=0
      DO WHILE (LVTXT .GT. 0)
        PHIV = Q(LVTXT+6)
        XV =  Q(LVTXT+7)     
        YV =  Q(LVTXT+8)
        IF (LDTRK.GT.0) THEN
          CALL CVRPHI(PHIV,PHIZ,PT,XZ,YZ,XV,YV,OK)
        ELSEIF (LFDCT.GT.0) THEN
          CALL FVRPHI(PHIV,PHIZ,PT,XZ,YZ,XV,YV,OK)
        ENDIF
        IF (OK) THEN
          NVTXT=NVTXT+1
          RENDSEG=ENDSEG1( XZ,YZ,PHIZ,XV,YV,PHIV)
          IF (RENDSEG.LT.MINENDSEG) THEN
            MINENDSEG=RENDSEG 
            LVTXTSV = LVTXT
          ENDIF
        ENDIF
        LVTXT = LQ(LVTXT)
      ENDDO
      LVTXT = LVTXTSV
  999 RETURN
      END
