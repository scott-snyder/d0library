      SUBROUTINE TRGPED(FILNAM,WIRE,LAYER,PEDES,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get TRD pedestals
C-
C-   Inputs  : FILNAM= File name containing the ped. information
C-             WIRE = wire number (from 1)
C-             LAYER= layer number (from 1)
C-   Outputs : IERR =0 if no error
C-             PEDES= Ped. value
C-   Controls:
C-
C-   Created  26-APR-1989   A. Zylberstejn :adapted from prtrpd
C-   Updated  10-APR-1990   J.Fr.Glicenstein  Remove the remaining print
C-   Updated  25-JAN-1994   A. Zylberstejn  : fix a problem with layer 3
C-   Updated  28-JAN-1994   J.P. Cussonneau  pedes have a fixed value 
C-                                           for new mc data 
C-   Updated  29-APR-1994   A. Zylberstejn  Remove swapping of wires in layer 3
C-                                          already done in TCODER
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$LINKS:IZTPDH.LINK'
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INCLUDE 'D0$LINKS:IZSTRD.LINK'
      INCLUDE 'D0$INC:TRDWIR.INC'
C
      INTEGER WIRE,LAYER,LBK,NBK,IFL,LAUX,IADDR,ADR
      LOGICAL FIRST,RUN1A
      LOGICAL MC_DATA
      INTEGER LSTPC,   LTRPD,WI
      REAL PEDES
      CHARACTER*(*) FILNAM
      INTEGER IBK,IERR,K,NW
      INTEGER TRD_FIND_VER,VERSION
      DATA FIRST/.TRUE./
C
      IERR=1
      IF(FIRST)THEN
C        LOUT = trunit()
        FIRST=.FALSE.
C -- FIND VERSION NUMBER FOR MC DATA
        IF ( LHEAD.GT.0 ) THEN
          MC_DATA =  IQ(LHEAD+1) .GT. 1000
        ELSE
          MC_DATA =  .FALSE.
        ENDIF
        VERSION = TRD_FIND_VER()
C --
        IF(LSTPH.EQ.0)THEN
          CALL ERRMSG(' Error in TRGPED: bank STPH not booked' ,
     &      'TRGPED',' ','W')
          GO TO 999
        END IF
        LSTPC = LC ( LSTPH - IZSTPC )
        IF(LSTPC.EQ.0)THEN
          CALL ERRMSG(' Error in TRGPED: bank STPC not booked',
     &      'TRGPED',' ','W')
          GO TO 999
        END IF
        LSTRD=LC(LSTPC-IZSTRD)
        IF(LSTRD.EQ.0)THEN
          CALL ERRMSG(' Error in TRGPED: bank STPD not booked',
     &      'TRGPED',' ','W')
          GO TO 999
        END IF
        LTPDH=LC(LSTRD-IZTPDH)
        IF(LTPDH.EQ.0) THEN
          CALL ERRMSG(' Error in TRGPED: bank TPDH not booked',
     &      'TRGPED',' ','W')
C          WRITE(LOUT,*)' Error in TRGPED: bank TPDH not booked'
          GOTO 999
        ENDIF
C        WRITE (LOUT,1001) IC(LTPDH+1),IC(LTPDH+2)
      END IF
C --
      IF(MC_DATA.AND.VERSION.EQ.1) THEN
        PEDES = 8.83
        IERR = 0
        GOTO 10
      ENDIF  
C
      LAUX=LC(LTPDH-LAYER)
      WI=WIRE
c      IF(.NOT.RUN1A() .AND. LAYER.EQ.3)WI=WIRE+2*MOD(WIRE,2)-1
      PEDES=C(LAUX+WI)
      IERR=0
   10 CONTINUE
C
  999 RETURN
 1001 FORMAT(///,'  TRD PEDESTAL BANK ,  MIN RELEVANT RUN NUMBER ',I8,
     >'      MAX  ',I8)
 1002 FORMAT(1X,16F7.2)
 1003 FORMAT(/,'   TRD PEDESTAL BANK NUMBER ',I6)
      END
