C DEC/CMS REPLACEMENT HISTORY, Element CHECK_VER.FOR
C *1    20-AUG-1991 14:09:30 ABACHI " IDENTIFIES VERTEX FROM LIST"
C DEC/CMS REPLACEMENT HISTORY, Element CHECK_VER.FOR
      SUBROUTINE CHECK_VER(ORENT,NVER,ZVER,DZVER,SLDA,SLWA,XPDF,XDF,
     &        XPWF,XWF,VERTEX,DZV,OK_VER)                 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :CHECK WHICH VERTEX IS THE BEST 
C-
C-   Inputs  : ORENT,NVER,ZVER,DZVER,SLDA,SLWA,XPDF,XDF,XPWF,XWF,
C-   Outputs : VERTEX,DZV,OK_VER
C-   Controls: 
C-
C-   Created  27-JUL-1991   AKL
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL DRIFT,WIRE,OK_VER
      REAL ZVER(*),DZVER(*),SLDA,SLWA,XPDF,XDF,XPWF,XWF,VERTEX(*),DZV,  
     &  ZMUD,ZMUW,DZ,ZMIN
      INTEGER NVER,ORENT,IV
C----------------------------------------------------------------------
      ZMIN = 30.
      OK_VER = .FALSE.
      DRIFT = .FALSE.
      WIRE = .FALSE.
      VERTEX(1) = 0.0
      VERTEX(2) = 0.0
      VERTEX(3) = 0.0
      IF(ORENT.EQ.1.OR.ORENT.EQ.2)THEN
       ZMUD =  XDF - SLDA*XPDF
       DRIFT = .TRUE.
      ELSEIF(ORENT.EQ.3.OR.ORENT.EQ.4)THEN
       ZMUD = XPDF - XDF/SLDA 
       ZMUW = XPWF - XWF/SLWA 
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
