      SUBROUTINE CHKVER(ITRAK,NVER,ZVER,DZVER,XPF,XDF,
     &        XWF,VERTEX,DZV,OK_VER)                 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :CHECK WHICH VERTEX IS THE BEST 
C-
C-   Inputs  : ITRAK,NVER,ZVER,DZVER,XPF,XDF,XWF,
C-   Outputs : VERTEX,DZV,OK_VER
C-   Controls: 
C-
C-   Created  20-NOV-1991   A.Klatchko   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      LOGICAL DRIFT,WIRE,OK_VER
      REAL ZVER(*),DZVER(*),SLDA,SLWA,XPF,XDF,XWF,VERTEX(*),DZV,  
     &  ZMUD,ZMUW,DZ,ZMIN
      INTEGER ITRAK,NVER,ORENT,IV,LMFIT,GZMFIT,NEW_ORENT
      REAL XCIN,YCIN,ZCIN,XPIN,XDIN,XWIN
C----------------------------------------------------------------------
      OK_VER = .FALSE.
      DRIFT = .FALSE.
      WIRE = .FALSE.
      LMFIT = GZMFIT(ITRAK)
      IF(LMFIT .LE. 0)GOTO 999
      ORENT = NEW_ORENT(IQ(LMFIT+4))
      XCIN = Q(LMFIT+20)/Q(LMFIT+23)
      YCIN = Q(LMFIT+21)/Q(LMFIT+23)
      ZCIN = Q(LMFIT+22)/Q(LMFIT+23)
      CALL ORCOR(ORENT,XCIN,YCIN,ZCIN,XPIN,XDIN,XWIN)
      SLDA = XDIN/XPIN
      SLWA = XWIN/XPIN
      ZMIN = 30.
      VERTEX(1) = 0.0
      VERTEX(2) = 0.0
      VERTEX(3) = 0.0
      IF(ORENT.EQ.1.OR.ORENT.EQ.2)THEN
       ZMUD =  XDF - SLDA*XPF
       DRIFT = .TRUE.
      ELSEIF(ORENT.EQ.3.OR.ORENT.EQ.4)THEN
       ZMUD = XPF - XDF/SLDA 
       ZMUW = XPF - XWF/SLWA 
       DRIFT = .TRUE.
       WIRE  = .TRUE.
      ENDIF 
      DO IV = 1,NVER
       IF(DRIFT)THEN
        DZ = ABS(ZVER(IV) - ZMUD) 
        IF( DZ .LT. ZMIN)THEN
         ZMIN=DZ
         VERTEX(3) = ZVER(IV)
         DZV = DZVER(IV)
         OK_VER = .TRUE.
        ENDIF
       ENDIF 
       IF(WIRE)THEN
        DZ = ABS(ZVER(IV) - ZMUW) 
        IF( DZ .LT. ZMIN)THEN
         ZMIN=DZ
         VERTEX(3) = ZVER(IV)
         DZV = DZVER(IV)
         OK_VER = .TRUE.
        ENDIF
       ENDIF 
      ENDDO 
  999 RETURN
      END
