program ej6tp6
!          dado un vEC de n elementos, devolveré el n de mayor o menor modulo.

    implicit none

    integer, parameter :: N=5
    complex v(n), rtdo
    integer i
    character opcion

!       uso un vector de ejemplo
    v(1)=(1.0,2.0)
    v(2)=(3.0,4.0)
    v(3)=(5.0,-1.0)
    v(4)=(-2.0,-3.0)
    v(5)=(0.5,1.5)
         

    write(*,*)"ingrese el M para el mayor mod y m para el menor"
    read(*,*) opcion

    rtdo= mod_ext(v,n,opcion)
         
    write(*,*) "el elemento con el", opcion, "modulo es:", rtdo

contains

    function mod_extremo(v,n,opt) rtdo(extremo)
     implicit none
        ! declaro las variables
     complex, intent(in):: v(n)
     integer, intent(in):: n
     character, intent(in):: opt
     complex :: extremo
     real :: mod_actual, mod_extremo
     integer:: i


!       iniciamos con el primer valor

     extremo=v(1)
     mod_ext= abs(v(1))

!       comparamos cada el

     do i=2,n
        mod_actual= abs(v(i))

         if ((opt= "M" .and. mod_actual .gt. mod_extremo) .or. &
             (opt == 'm' .and. mod_actual .le. mod_extremo)) then
            extremo = vec(i)
            mod_extremo = mod_actual
         endif
     enddo
    end function modulo_extremo

end program 