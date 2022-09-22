      subroutine ez_set_element_l (array_name,array_element,idx
     &     ,ocurrence, val, ier)
      implicit none
      CHARACTER*(*) ARRAY_NAME
      CHARACTER*(*) ARRAY_ELEMENT
      INTEGER IDX, OCURRENCE, IER
      logical val(*)
      call ez_set_element (array_name, array_element, idx, ocurrence,
     &     val, ier)
      return
      end
