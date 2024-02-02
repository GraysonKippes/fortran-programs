! RLC circuit solver
! Version: 0.1
! Author: Grayson Kippes
! Creation date: 2/2/2024
! Last modified: 2/2/2024
! License: MIT License

program RLC_solver
        implicit none

        integer, parameter :: OVER_DAMPED = 0
        integer, parameter :: UNDER_DAMPED = 1
        integer, parameter :: CRITICALLY_DAMPED = 2

        logical :: is_parallel
        real :: R ! Resistance of the resistor in the circuit.
        real :: L ! Inductance of the inductor in the circuit.
        real :: C ! Capacitance of the capacitor in the circuit.

        real :: neper_freq
        real :: neper_freq_squared
        real :: res_freq
        real :: res_freq_squared
        real :: s1 ! Root s1 of the circuit.
        real :: s2 ! Root s2 of the circuit.
        integer :: damping
        character(:), allocatable :: damping_text

        ! Character input for circuit configuration.
        character :: ui_configuration
        ui_configuration = 'n' ! 'n' stands for neither, default initial state.
        do while (ui_configuration /= 's' .and. ui_configuration /= 'p')
                print *, 'Enter the RLC circuit configuration (s = series / p = parallel): '
                read(*,*) ui_configuration
                if (ui_configuration /= 's' .and. ui_configuration /= 'p') then
                        print *, 'Invalid character response.'
                end if
        end do

        if (ui_configuration == 'p') then
                is_parallel = .true.
        else
                is_parallel = .false.
        end if

        ! Get RLC values from user.
        print *, 'Enter the resistance of the circuit: '
        read(*,*) R
        print *, 'Enter the inductance of the circuit: '
        read(*,*) L
        print *, 'Enter the capacitance of the circuit: '
        read(*,*) C
       
        ! Calculate neper frequency depending on configuration.
        if (is_parallel) then
                neper_freq = 1.0 / (2.0 * R * C)
        else
                neper_freq = R / (2.0 * L)
        end if
        neper_freq_squared = neper_freq**2

        res_freq_squared = 1.0 / (L * C)
        res_freq = sqrt(res_freq_squared)

        s1 = -neper_freq + sqrt(neper_freq_squared - res_freq_squared)
        s2 = -neper_freq - sqrt(neper_freq_squared - res_freq_squared)

        if (neper_freq_squared > res_freq_squared) then
                damping = OVER_DAMPED
                damping_text = 'over damped'
        else if (neper_freq_squared < res_freq_squared) then
                damping = UNDER_DAMPED
                damping_text = 'under damped'
        else
                damping = CRITICALLY_DAMPED
                damping_text = 'critically damped'
        end if

        print *, 'Neper frequency = ', neper_freq
        print *, 'Resonant frequency = ', res_freq
        print *, 'Root s1 = ', s1
        print *, 'Root s2 = ', s2
        print *, 'The circuit is '//damping_text//'.'

end program
