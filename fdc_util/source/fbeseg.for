      SUBROUTINE FBESEG(NSEN, MAXLVL,X,Y,IMAX,IFINAL,FRESID,
     &                           XBEST,YBEST,BESTCHI,BESTA,BESTB)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find the best fit amongst the sets of X,Y
C-                         points available.
C-
C-   Inputs  : NSEN   [I]= Number of sense wires
C-             MAXLVL [I]= Maximum number of points on line
C-             X, Y   [F]= Available set of X,Y points
C-             IMAX   [I]= Maximum number of possibilities at each level
C-   Outputs : IFINAL [I]= Final level choices of input set
C-             FRESID [F]= Final residuals from the best fit
C-             XBEST  [F]= Final x-coor from best fit
C-             YBEST  [F]= Final y-coor from best fit
C-             BESTCHI [F]= Chisq from best fit
C-             BESTA [F]= Slope from best fit
C-             BESTB [F]= Intercept from best fit
C-
C-   Created  18-JUN-1990   Jeffrey Bantly
C-   Updated  12-DEC-1990   Susan K. Blessing  Made all passed arrays
C-    variable length to correct overwriting error.
C-   Updated  17-JUN-1991   Susan K. Blessing  Return BESTA and BESTB 
C-   Updated   3-JUL-1991   Susan K. Blessing  Remove VZERO calls for 
C-    IFINAL,FRESID,XBEST,YBEST - not necessary
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      INTEGER NSEN
      INTEGER MAXLVL,I(16),IMAX(NSEN),IFINAL(NSEN),NLVL,JJ
C
      REAL X(NSEN,MX_HIT_WIRE*2),Y(NSEN,MX_HIT_WIRE*2),TX(16),TY(16)
      REAL A,B,CHISQ,RESID(16),FRESID(NSEN)
      REAL XBEST(NSEN),YBEST(NSEN)
      REAL BESTCHI,BESTA,BESTB
C
C----------------------------------------------------------------------
      CALL VFILL(I,16,1)
      BESTCHI = 99999.0
C
      I(1) = 0
      NLVL = 1
  100 CONTINUE                          ! Begin loop over possible roads
      I(NLVL) = I(NLVL) + 1
      IF( I(NLVL) .LE. IMAX(NLVL) ) THEN
        NLVL = 1
        DO 12 JJ=1,MAXLVL
          TX(JJ) = X(JJ,I(JJ))
          TY(JJ) = Y(JJ,I(JJ))
   12   CONTINUE
        CALL SEGFIT(TX,TY,MAXLVL,A,B,CHISQ,RESID)
C    CHISQ is the chi**2/dof
        IF( CHISQ .LT. BESTCHI .AND. CHISQ .GT. 0.0 ) THEN
          BESTCHI = CHISQ
          BESTA = A
          BESTB = B
          DO 13 JJ=1,MAXLVL
            IFINAL(JJ)=I(JJ)
            FRESID(JJ)=RESID(JJ)
            XBEST(JJ)=TX(JJ)
            YBEST(JJ)=TY(JJ)
   13     CONTINUE
        ENDIF
      ELSE
        NLVL = NLVL + 1
        I(NLVL-1) = 1
        IF( NLVL .GT. MAXLVL ) GOTO 150
      ENDIF
      GOTO 100
C
  150 CONTINUE                          ! Done
C----------------------------------------------------------------------
  999 RETURN
      END
