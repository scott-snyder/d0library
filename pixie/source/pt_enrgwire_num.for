      SUBROUTINE PT_ENRGWIRE_NUM(CASE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Displays the energy and wire number on the
C-   screen of a requested wire.
C-
C-   Inputs  : Case=1 :   "    "  all hits
C-                 =2  called for hits on traks
C-   Outputs : None
C-
C-   Created  18-APR-1991   Lupe Howell
C-   Updated  18-FEB-1994   A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:PXPARA.INC'
      INCLUDE 'D0$INC:worksp.INC'
C----------------------------------------------------------------------
C
      INTEGER WIRE,LAYER,CASE,I,J,K,E1,E2,IS,IE,NCELL
C
      REAL ENRG
C
      CHARACTER*28  MESS
      CHARACTER*2 C2
      CHARACTER*4 TXTMS1
      CHARACTER*4 TXTMS2
      CHARACTER*5 TXTMS3
      CHARACTER*3 CLAYER   ! Character Layer num
      CHARACTER*7 CENRG,CX1! Character Energy
      CHARACTER*7 CX       ! Dummy char variable
      CHARACTER*4 CX2,
     X           CWIRE     ! Character wire number
      CHARACTER*80 TEXT
C
      INTEGER ENDVIEW,ION
C----------------------------------------------------------------------
      DATA TXTMS1/' WIR'/
      DATA TXTMS2/' LAY'/
      DATA TXTMS3/' ENRG='/
C----------------------------------------------------------------------
      CALL PUMESS('click outside TRD  to exit')
   10 ENRG = 0.
      CALL PTLOCA(WIRE,LAYER,ENRG)     ! Picks the wire of the trd
      IF(WIRE.LE.0)GO TO 999
C
      IF(CASE.EQ.2)THEN
C
        CALL PTRD_GET_TRACK(WIRE,LAYER)
        NCELL=0
        DO I=1,3
          TEXT=' '
          J=(I-1)*10
c          WRITE(73,*)'layer',',wire',WIRE,' nb of cells',IWS(J+1)
          WRITE(C2,'(i2)')I
          IF(IWS(J+1).NE.0)THEN
c            WRITE(73,'(4(i4,f4.1))')(IWS(K+J+1),WS(K+J+5),K=1,IWS(J+1))
            IS=18
            TEXT=' layer '//C2//' hit cells'
            NCELL=NCELL+1
            DO K=1,IWS(J+1)
              IE=IS+5
              WRITE(CX2,'(i4)')IWS(K+J+1)
              TEXT(IS:IE)=CX2
              IS=IS+5
            END DO
            TEXT(IS:IS+10)='energies: '
            IS=IS+10
            DO K=1,IWS(J+1)
              IE=IS+5
              WRITE(CX1,'(f7.2)') WS(K+J+5)
              TEXT(IS:IE)=CX1
              IS=IS+5
            END DO
          ELSE
            TEXT=' layer '//C2//' no hit cells'
          END IF
c          WRITE(73,*)TEXT
          CALL PUMESS(TEXT)         !
        END DO
        IF(NCELL.GE.2)THEN
          TEXT=' '
          WRITE(CX1,'(f7.2)') WS(100)
          IF(NCELL.EQ.3)THEN
            WRITE(CX,'(f7.2)') WS(101)
            TEXT='tot energy: '//CX1//' Truncated mean'//CX
          ELSE
            TEXT='tot energy: '//CX1
          END IF
          CALL PUMESS(TEXT)         !
        END IF
      ELSE
C **** Converting wire, layer and enrg number into charc var
        WRITE(CX2,110)WIRE
  110   FORMAT(I4)
        READ(CX2,120)CWIRE
  120   FORMAT(A4)
        WRITE(CX2,122)LAYER
  122   FORMAT(I2)
        READ(CX2,124)CLAYER
  124   FORMAT(A2)
        WRITE(CX1,130)ENRG
  130   FORMAT(F7.1)
        READ(CX1,140)CENRG
  140   FORMAT(A7)
        MESS=TXTMS1//CWIRE//TXTMS2//CLAYER//TXTMS3//CENRG
        CALL PUMESS(MESS)         !
      END IF
      GO TO 10
C----------------------------------------------------------------------
C
C ****  ENTRY PT_FADCWIRE: Displays the FADC of a wire in a TRD view
C
C----------------------------------------------------------------------
      ENTRY PT_FADCWIRE
C
C ****  Getting wire from the user
C
  160 ENRG = 0.
      ION = 1
      CALL PTLOCA(WIRE,LAYER,ENRG)     ! Picks the wire of the trd
C      CALL JVSALL(0)                   ! Clear the screen
      CALL PUHEAD('TRD_HITS')          ! Show header
      IF(WIRE.LE.0)GO TO 999
      CALL PTFADC(WIRE,LAYER)          ! Plots the FADC trace
      GO TO 160
  999 CONTINUE
      RETURN
      END
