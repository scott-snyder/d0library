      SUBROUTINE PTGTWI(WIRE,LAYER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Gets wire number and layer number of the TRD 
C-           from the user. It will return a specific wire or layer num 
C-           according to the following:  
C-
C-            LAYER=-1, WIRE=NUM- returns the layer with the higest 
C-                          energy released in the wire number (NUM).
C-            LAYER=-1  WIRE=-1 - returns the layer and wire num that 
C-                          has the highest energy releassed overall.
C-            LAYER=NUM, WIRE=-1- returns the highest wire num in the
C-                          given layer (NUM).
C-            LAYER=NUM, WIRE=NUM- returns the same wire and layer 
C-                          number.
C-            LAYER=0  , WIRE=NUM- returns all three layers of wire NUM
C-            LAYER=0  , WIRE=-1 - Returns all 3 layers of the wire with 
C-                           the highest energy.
C-
C-   Inputs  : WIRE,LAYER - Entered by the user.
C-   Outputs : WIRE,LAYER - according to the above criteria 
C-
C-   Created  19-JAN-1989   LUPE ROSAS
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER WIRE,LAYER
      INTEGER LEN,ITRY, ANODE
      CHARACTER*80 STRING
      CHARACTER*80 PROM(2) 
C----------------------------------------------------------------------
      DATA PROM/'0 if you wat all three layers displayed',
     X         'Enter wire number; -1 for wire wit maximum energy'/
      DATA ANODE /1/
C----------------------------------------------------------------------
      ITRY =0          
  10  CALL PUMESS('Enter layer number;')
      CALL PUMESS('-1 for layer and wire with maimumx energy')
      CALL PURSTR(PROM(1),STRING,LEN)  
      IF (LEN.NE.0) THEN
        READ(STRING(1:LEN),*,ERR=800)LAYER       
      ENDIF
      IF ((LAYER.GT.3).OR.(LAYER.LT.-1)) THEN 
        GO TO 800          ! Checking for ilegal layer num
      ENDIF
      CALL PURSTR(PROM(2),STRING,LEN)
      IF (LEN.NE.0) THEN
        READ(STRING(1:LEN),*,ERR=810)WIRE
      ENDIF
C-- Checking if legal wire number entered
      IF ((WIRE.GT.256).OR.(WIRE.LT.-1).OR.(WIRE.EQ.0)) THEN
        GO TO 810
      ENDIF
C-
C-   CHECKING OPTIONS
      IF (LAYER.EQ.-1.) THEN
        IF (WIRE.EQ.-1.) THEN 
          CALL PTHENG(WIRE,LAYER)  ! Wire and layer with the maximum enrg 
        ELSE
          CALL PTHEWI(WIRE,LAYER)  ! Layer of WIRE with its max enrg
        ENDIF
      ELSEIF(WIRE.EQ.-1.) THEN   
        IF(LAYER.NE.0) THEN
          CALL PTHELA(LAYER,ANODE,WIRE)  ! Wire that has the max enrg in LAYER
        ELSE
          CALL PTHENG(WIRE,LAYER)   ! Disp all 3 layrs of the wire with
          LAYER=0.                  ! max enrg
        ENDIF
      ENDIF
      GO TO 999  
C-  ERROS MESSAGES
  800 IF(ITRY.GT.0) GO TO 999      ! Checking the number of tries,
      ITRY=1                       ! only alouds you for 1 try after a
C                                  ! bad entry
      CALL PUMESS(' ERROR IN READING LAYER NUMBER .. TRY AGAIN')
      GO TO 10
  810 IF (ITRY.GT.0) GO TO 999
      ITRY = 1.
      CALL PUMESS(' ERROR IN READING WIRE NUMBER .. TRY AGAIN')
      GO TO 10
  999 RETURN
      END
