      LOGICAL FUNCTION INTERR()
C-----------------------------------------------------------------
C-                                                               -
C-     handle flags for INTERRUPT menu                           -
C-                                                               -
C-     ENTRY SETINT(FL1)                                         -
C-     Input:                                                    -
C-     FL1 = logical value for subsequent calls to INTERR        -
C-          if INTMEN has not been called (INTAST=false)         -
C-                                                               -
C-     ENTRY INTREQ  true for command requests from              -
C-                   INTERRUPT menu                              -
C-                                                               -
C-     ENTRY SETINQ(FL2)                                         -
C-     Input:                                                    -
C-     FL2 = value of INTREQ in subsequent calls is FL1.AND.FL2  -
C-                                                               -
C-          SDP Dec.,1986                                        -
C-          Modified Aug.,1988                                   -
C-                                                               -
C-----------------------------------------------------------------
C
      IMPLICIT NONE
      LOGICAL FL,INTEFL,INTAST,INTREQ,SETINT,SETINQ,INRQFL
C
      INTERR=INTEFL.AND.(.NOT.INTAST())
      RETURN
C
      ENTRY INTREQ()
      INTREQ=INTEFL.AND.INRQFL
      RETURN
C
      ENTRY SETINT(FL)
      INTEFL=FL
      IF ( INTEFL ) THEN
        CALL INTMSG(' Interrupt menu turned ON')
      ELSE
        CALL INTMSG(' Interrupt menu turned OFF')
      ENDIF
      RETURN
C
      ENTRY SETINQ(FL)
      INRQFL=FL
      RETURN
C
      END      
