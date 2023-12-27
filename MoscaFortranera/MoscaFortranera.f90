! ****************************************************************************
!
!  PROGRAM: MoscaFortranera
!
!  AUTHOR: AlvaroR04
!  PURPOSE: Juego de la mosca creado en FORTRAN
!
! ****************************************************************************

program MoscaFortranera
    
    implicit none
    
    
    ! --- AREA DE VARIABLES ---
    
    
    integer::i = 0
    integer::tamanio = 0
    integer::posicionUsuario = 0
    integer::posicionMosca = 0
    integer::estadoMosca = 0
    integer::intentos = 5
    character::salir = ""
    integer, dimension(:), allocatable :: tablero
    
    
    ! --- AREA DE INSTRUCCIONES ---
    
    
    ! Controla que el usuario escribe el tamanio correcto
    
    do while(tamanio < 3)
        print *, "Tamaño mayor que 3: "
        read *, tamanio
    end do
    
    ! Inicializa el tablero
    
    allocate(tablero(tamanio))
    
    do i = 1, tamanio
        tablero(i) = 0
    end do    
    
    ! Coloca a la mosca en una posicion aleatoria 
    
    posicionMosca = obtenerAleatorio(tamanio)
    tablero(posicionMosca) = 1
    
    ! El juego no termina hasta que no haya intentos o la mosca haya sido atrapada
    
    do while(intentos > 0 .and. estadoMosca /= 2)
        
        ! Controla que el usuario seleccione la posicion correcta
        
        do while(posicionUsuario < 1 .or. posicionUsuario > tamanio)
            print *, "Seleccione una posicion (1-", tamanio, "): "
            read *, posicionUsuario
        end do
        
        ! El jugador gana si en esa posicion esta la mosca
        
        if(tablero(posicionUsuario) == 1) then
            estadoMosca = 2
        else
            ! Si en la posicion de atras (si existe tal posicion), esta la mosca, el estado de la mosca sera 1
            
            if(posicionUsuario - 1 >= 1) then
                if(tablero(posicionUsuario - 1) == 1) then
                    estadoMosca = 1
                end if
            end if
            
            ! Si no se encontro antes, se hara con la de delante (si existe tal posicion)
            
            if(posicionUsuario + 1 <= tamanio .and. estadoMosca /= 1) then
                if(tablero(posicionUsuario + 1) == 1) then
                    estadoMosca = 1
                end if
            end if
            
            ! Si en ninguna se encontro cerca de la posicion del usuario, el estado de la mosca sera 0
            
            if(estadoMosca /= 1) then
                estadoMosca = 0
            end if
        end if
        
        ! Eventos que ocurriran segun el estado de la mosca
        
        select case(estadoMosca)
            case(0)
                print *, "Frío"
            case(1)
                ! Se vuelve a reubicar a la mosca en el tablero
                
                tablero(posicionMosca) = 0
                posicionMosca = obtenerAleatorio(tamanio)
                tablero(posicionMosca) = 1
            
                print *, "¡Caliente, pero se ha escapado!"
        end select
        
        posicionUsuario = 0
        intentos = intentos - 1
    end do
    
    ! Mensaje segun haya ganado o no el jugador
    
    if(estadoMosca == 2) then
        print *, "¡La mosca fue atrapada!"
    else
        print *, "La mosca estaba en: "
    end if
    
    ! Mostrar el tablero en pantalla
    
    do i = 1, tamanio
        if(tablero(i) == 0) then
            write(*, "(a)", advance="no") " [] "
        else
            write(*, "(a)", advance="no") " <Bo "
        end if
    end do
    
    ! Libera el tablero en memoria
    
    deallocate(tablero)
    
    ! Este sistema impide cerrar la terminal, se hace para que el usuario pueda leer los resultados
    
    print *, "" ! Salto de linea
    print *, "Escriba cualquier caracter (que no sea ningún tipo de espacio) y presiona ENTER para salir: "
    read *, salir
    
    
    ! --- AREA DE METODOS Y FUNCIONES ---
    
    
    contains
    
    ! ****************************************************************************
    ! Permite obtener un valor aleatorio entre 1 y un maximo no incluido
    !
    ! @param max es el maximo incluido del aleatorio
    ! ****************************************************************************
    
    function obtenerAleatorio(max) result(res)
        implicit none
        
        integer, intent(in) :: max
        real::aleatorio
        integer::res
        
        call RANDOM_SEED()
        call RANDOM_NUMBER(aleatorio)
        
        res = 1 + int(aleatorio * max)
    end function

end program MoscaFortranera

