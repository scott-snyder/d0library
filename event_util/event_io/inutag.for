      SUBROUTINE INUTAG
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Called every time an event is read in. Check whether
C-   the event contains a UTAG bank. If yes, copy the event to the run division
C-   and drop all banks except UTAG. If no, copy check whether there is a UTAG
C-   bank in the run division and copy it to the event. If there is no UTAG bank
C-   present issue an error message.
C-
C-   Created  29-JAN-1994   Ulrich Heintz
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZANLS.LINK'
      INCLUDE 'D0$LINKS:IZUTAG.LINK'
      INTEGER LUTAG,GZUTAG,LANLS,GZANLS,LUDST,GZUDST,LUTAGR,LANLSR
      INTEGER IXWIPE,I,NS,IOS
C----------------------------------------------------------------------
      LUTAG=GZUTAG()
      IF(LUTAG.GT.0)THEN              ! if there is a UTAG bank in the event
        IF(LHEADR.EQ.0)THEN           ! if there is no b-o-r record copy the  
          IXWIPE=IXCOM+IXDVR          ! entire event
          CALL MZWIPE(IXWIPE)
          CALL MZCOPY(IXMAIN,LHEAD,IXDVR,LHEADR,1,' ')
          NS=IQ(LHEADR-2)             ! drop everything that hangs from HEAD
          DO I=1,NS                   ! except ANLS
            IF(I.NE.IZANLS.AND.LQ(LHEADR-I).GT.0)THEN
              CALL MZDROP(IXCOM,LQ(LHEADR-I),'L')
            ENDIF
          ENDDO
          LANLSR=LQ(LHEADR-IZANLS)    ! drop everything that hangs from ANLS
          NS=IQ(LANLSR-2)             ! except UTAG
          DO I=1,NS
            IF(I.NE.IZUTAG.AND.LQ(LANLSR-I).GT.0)THEN
              CALL MZDROP(IXCOM,LQ(LANLSR-I),'L')
            ENDIF
          ENDDO
        ELSE                          ! else copy only ANLS or UTAG to run
          LANLSR=LQ(LHEADR-IZANLS)    ! division
          IF(LANLSR.EQ.0)THEN
            LANLS=GZANLS()
            CALL MZCOPY(IXMAIN,LANLS,IXDVR,LHEADR,-IZANLS,' ')
            LANLSR=LQ(LHEADR-IZANLS)  ! drop everything that hangs from ANLS
            NS=IQ(LANLSR-2)           ! except UTAG
            DO I=1,NS
              IF(I.NE.IZUTAG.AND.LQ(LANLSR-I).GT.0)THEN
                CALL MZDROP(IXCOM,LQ(LANLSR-I),'L')
              ENDIF
            ENDDO
          ELSE
            LUTAGR=LQ(LANLSR-IZUTAG)
            IF(LUTAGR.GT.0)CALL MZDROP(IXCOM,LUTAGR,' ')
            CALL MZCOPY(IXMAIN,LUTAG,IXDVR,LANLSR,-IZUTAG,' ')
          ENDIF
        ENDIF
      ELSE                            ! if there is no UTAG in the event
        LUDST=GZUDST()
        IOS=MOD(IQ(LHEAD+1),1000)
        IF(LUDST.GT.0)THEN            ! quit if there is no UDST bank
          LUTAGR=0                    ! else search for UTAG in run division
          IF(LHEADR.GT.0)THEN
            LANLSR=LQ(LHEADR-IZANLS)
            IF(LANLSR.GT.0)LUTAGR=LQ(LANLSR-IZUTAG)
          ENDIF
          IF(LUTAGR.GT.0)THEN         ! if found copy it to event
            LANLS=GZANLS()
            CALL MZCOPY(IXDVR,LUTAGR,IXMAIN,LANLS,-IZUTAG,' ')
          ELSE                        ! else something is screwed up -> error
            CALL ERRMSG('UTAG_LOST','INUTAG','lost UTAG bank','W')
          ENDIF
        ELSEIF(IOS.NE.0.AND.IOS.LE.4)THEN   ! if this is a begin-run-record
          LUTAGR=0                    ! else search for UTAG in run division
          IF(LHEADR.GT.0)THEN
            LANLSR=LQ(LHEADR-IZANLS)
            IF(LANLSR.GT.0)LUTAGR=LQ(LANLSR-IZUTAG)
          ENDIF
          IF(LUTAGR.GT.0)THEN         ! if found copy it to event
            LANLS=GZANLS()
            IF(LANLS.LE.0)CALL BKANLS(LANLS)
            CALL MZCOPY(IXDVR,LUTAGR,IXMAIN,LANLS,-IZUTAG,' ')
          ENDIF
         ENDIF
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
