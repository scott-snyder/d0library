      SUBROUTINE VTUNPK( LABEL, DATAS )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Unpack the FADC of the selected chanel of
C-              the VTX for standard datas
C-
C-   Inputs  : LABEL: VTX wire or strip channel number
C-             For wires, LABEL = (LAYER*32+SECTOR)*16+WIRE*2+END
C-             For strips,LABEL = (ZLAYER+8)*512 + STRIP*2 + ZEND
C-   Outputs : DATAS  [I] : Unpacked datas, see CDEXPD description
C-
C-   Created   4-FEB-1988   Olivier Callot
C-   Modified 07-FEB-1989   Peter Grudberg  modified for VTX use
C-   Modified 07-NOV-1989   P.G. - change ZDEXPD call
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER DATAS(*), DCDTYP, LABEL
C----------------------------------------------------------------------
      DCDTYP = 0                        ! unpack all CDDn banks
      CALL ZDEXPD( DCDTYP, LABEL, DATAS )
  999 RETURN
      END
