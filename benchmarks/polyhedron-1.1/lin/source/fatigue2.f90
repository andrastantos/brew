module free_input 
!
!      Copyright (C) 1997 by Quetzal Computational Associates, Inc.
!      Address any questions about this copyright notice to:
!
!         Dr. John K. Prentice
!         Quetzal Computational Associates, Incorporated
!         3455 Main Avenue, Suite 4
!         Durango, CO   81301
!         USA
!
!         Phone:  970-382-8979
!         e-mail: quetzal@quetzalcoatl.com
!
implicit none

public :: next, value, convert_lower_case, check_eof, check_number

integer, parameter, public :: nin = 10
character (len=80), public :: card, field, lfield
integer, public ::   icpnt, free_format_error_flag
logical, public ::   eoff
integer, parameter, private :: LONGreal = selected_real_kind(15,90)
real (kind = LONGreal), public :: real_variable

contains

     subroutine next ()
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
      integer, parameter :: icend = 80
      integer, parameter :: nodel = 3
      character (len=1), dimension(3), parameter :: delim = (/" ", ",", "="/)
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i, j, k, iend, istart, kmin, err
      logical, save :: first = .true.
!
!        on first pass, initialize icpnt pointer
!
      if (first) then
          icpnt = icend + 1
          first = .false.
      end if
!
!        if icpnt>icend, read the next record off unit 2 into
!        the string 'card'.  next verify that this is a non-
!        blank card.  if it is blank or an input comment card
!        (asterisk in column 1), skip it and get the next record
!
loop: do
          eoff = .false.
          if (icpnt > icend) then
              read (unit= nin, fmt="(a)", iostat = err) card
              if (err /= 0) then
                  eoff = .true.
                  icpnt = icend + 1
                  return 

              end if
              if (card(1:icend)==" " .or. card(1:1)=="*") then
                  cycle loop
              end if
              icpnt = 1
          end if
!
!        get the next sub-string.  we do this as follows.  we
!        look for the next delimeter.  if it is as the same
!        position as the current pointer, we advance the pointer
!        and try again.  if not, then the pointer is at the beginning
!        of a sub-string and the delimeter is trailing this sub-string.
!        note that we look for all the delimeters possible before
!        taking any action.
!
          do i = icpnt, icend
              istart = i
              kmin = 0
              do j = 1, nodel
                  k = index(card(i:icend),delim(j))
!
!        index returns positions relative the beginning of the
!        sub-string.  hence we add in the appropiate off-set to
!        give the index relative to the beginning of the string
!        card, not just the sub-string card(i:icend).
!
                  if (k /= 0) then
                      k = k + i - 1
                      if (kmin == 0) then
                          kmin = k
                      else
                          kmin = min(k,kmin)
                      end if
                  end if
              end do
!
!        if kmin is not equal to the current pointer position, then
!        it must be pointing at the trailing delimeter of a valid
!        sub-string.
!
              if (kmin /= i) then
                  if (kmin > 0) then
                      iend = kmin - 1
                      exit loop
                  end if
!
!        if we fall through, there was no delimeter
!        found on the remainder of this record.  this means
!        the entire remainder of this record is a valid sub-string
!
                 iend = icend
                 exit loop
              end if
          end do
!
!        if we fall through this loop, there were no more non-
!        delimeters on this record.  go get next record
!
          icpnt = icend + 1
      end do loop
!
!        put the sub-string into the string 'field'.  note
!        that fortran 77 pads the string with blanks
!
      field = card(istart:iend)
      icpnt = iend + 2
!
      end subroutine next

      subroutine value (number, itype)
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      real (kind = LONGreal), intent(out) :: number
      integer, intent(out) :: itype
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: err
!-----------------------------------------------
!
!        get next field off unit 2
!
      call next ()
      if (eoff) then
          itype = -1
      else
!
!        read field
!
          read (unit = field, fmt="(bn,f20.0)", iostat = err) number
          if (err /= 0) then
              itype = 0
          else
              itype = 1
          end if
      end if
!
      end subroutine value

      function convert_lower_case (input_string) result (output_string)
!
      character (len=*), intent(in) :: input_string
      character (len=1) :: output_string
      integer :: collating_difference
!
      if (ichar(input_string) >= ichar("A") .and. ichar(input_string) <= ichar("Z")) then
          collating_difference = ichar(input_string) - ichar("A")
          output_string = char(ichar("a") + collating_difference)
      else
          output_string = input_string
      end if
!
      end function convert_lower_case

      subroutine check_eof ()
!
      if (free_format_error_flag == (-1)) then
          print *," "
          print *,"Abort.  Unexpected end of file while reading input. " 
          print *,"Was reading the line:"
          print *,card
          print *," "
          stop
      end if
!
      end subroutine check_eof
!
      subroutine check_number ()
!
      if (free_format_error_flag == 0) then
          print *," "
          print *,"Abort.  Expected a number on input and instead encountered the word: "
          print *,field
          print *," "
          print *,"was reading the line:"
          print *,card
          print *," "
          stop
      end if
!
      end subroutine check_number



end module free_input
module read_input_m
!
!      Copyright (C) 1997 by Quetzal Computational Associates, Inc.
!      Address any questions about this copyright notice to:
!
!         Dr. John K. Prentice
!         Quetzal Computational Associates, Incorporated
!         3455 Main Avenue, Suite 4
!         Durango, CO   81301
!         USA
!
!         Phone:  970-382-8979
!         e-mail: quetzal@quetzalcoatl.com

use free_input

implicit none

private
public :: read_input

integer, parameter, private :: LONGreal = selected_real_kind(15,90)


contains

      subroutine read_input (input_file, dt, cmax, lambda, mu, yield_stress, R_infinity, b, &
                             X_infinity, gamma, eta, plastic_strain_threshold,              &
                             failure_threshold, spin_frequency, wire_test, coil_test,       &
                             wire_radius, coil_radius, coil_pitch, radius_of_curvature,     &
                             time_steps_per_spin_cycle, number_of_sample_points,            &
                             dump_history, crack_closure_parameter, spin_cycle_dump_interval)
!
!=========== formal variables =============
!
      character (len=*), intent(in) :: input_file
      real (kind = LONGreal), intent(out) :: dt, lambda, mu, yield_stress, R_infinity, b,   &
                                             X_infinity, gamma, eta,                        &
                                             plastic_strain_threshold, wire_radius,         &
                                             coil_radius, coil_pitch, radius_of_curvature,  &
                                             failure_threshold, crack_closure_parameter
      integer, intent(out) :: spin_frequency, number_of_sample_points, cmax,                &
                              time_steps_per_spin_cycle, spin_cycle_dump_interval
      logical, intent(out) :: wire_test, coil_test, dump_history
!
!========== internal variables ============
!
      real (kind = LONGreal) :: Poissons_ratio, Youngs_modulus, shear_modulus
      integer :: n
!
      open (unit=nin,file=input_file,status="old",form="formatted",action="read")
      icpnt = 9999
!
!        set defaults
!
      cmax = huge(0)
      spin_cycle_dump_interval = huge(0)
      time_steps_per_spin_cycle = -999
      lambda = -1.0e20_LONGreal
      mu = -1.0e20_LONGreal
      yield_stress = -1.0e20_LONGreal
      R_infinity = -1.0e20_LONGreal
      b = -1.0e20_LONGreal
      X_infinity = -1.0e20_LONGreal
      gamma = -1.0e20_LONGreal
      eta = -1.0e20_LONGreal
      plastic_strain_threshold = -1.0e20_LONGreal
      spin_frequency = -999
      Poissons_ratio = -1.0e20_LONGreal
      Youngs_modulus = -1.0e20_LONGreal
      shear_modulus = -1.0e20_LONGreal
      wire_test = .false.
      coil_test = .false.
      wire_radius = -1.0e20_LONGreal
      coil_radius = -1.0e20_LONGreal
      coil_pitch = -1.0e20_LONGreal
      radius_of_curvature = -1.0e20_LONGreal
      number_of_sample_points = -999
      failure_threshold = -1.0e20_LONGreal
      dump_history = .false.
      crack_closure_parameter = -1.0e20_LONGreal
!
!        parse input and read simulation parameters
!
      do 
          call next ()
          if (eoff) then
              exit
          else
              if (len(field) > 0) then
                  do n = 1, len(field)
                      field(n:n) = convert_lower_case(field(n:n))  
                  end do
              end if
!
              if (field == "maximum_simulation_spin_cycles") then
                  call value (real_variable, free_format_error_flag) 
                  call check_eof ()
                  call check_number ()
                  cmax = int(real_variable + 0.5_LONGreal)
              else if (field == "spin_cycle_dump_interval") then
                  call value (real_variable, free_format_error_flag) 
                  call check_eof ()
                  call check_number ()
                  spin_cycle_dump_interval = int(real_variable + 0.5_LONGreal)
              else if (field == "time_steps_per_spin_cycle") then
                  call value (real_variable, free_format_error_flag) 
                  call check_eof ()
                  call check_number ()
                  time_steps_per_spin_cycle = int(real_variable + 0.5_LONGreal)
              else if (field == "number_of_radial_sample_points") then
                  call value (real_variable, free_format_error_flag) 
                  call check_eof ()
                  call check_number ()
                  number_of_sample_points = int(real_variable + 0.5_LONGreal)
              else if (field == "crack_closure_parameter") then
                  call value (real_variable, free_format_error_flag) 
                  call check_eof ()
                  call check_number ()
                  crack_closure_parameter = real_variable
              else if (field == "poissons_ratio") then
                  call value (real_variable, free_format_error_flag) 
                  call check_eof ()
                  call check_number ()
                  Poissons_ratio = real_variable
              else if (field == "youngs_modulus") then
                  call value (real_variable, free_format_error_flag) 
                  call check_eof ()
                  call check_number ()
                  Youngs_modulus = real_variable
              else if (field == "shear_modulus") then
                  call value (real_variable, free_format_error_flag) 
                  call check_eof ()
                  call check_number ()
                  shear_modulus = real_variable
              else if (field == "yield_stress") then
                  call value (real_variable, free_format_error_flag) 
                  call check_eof ()
                  call check_number ()
                  yield_stress = real_variable
              else if (field == "asymptotic_stress_due_to_isotropic_strain_hardening") then
                  call value (real_variable, free_format_error_flag) 
                  call check_eof ()
                  call check_number ()
                  R_infinity = real_variable
              else if (field == "rate_constant_for_isotropic_strain_hardening") then
                  call value (real_variable, free_format_error_flag) 
                  call check_eof ()
                  call check_number ()
                  b = real_variable
              else if (field == "asymptotic_back_stress") then
                  call value (real_variable, free_format_error_flag) 
                  call check_eof ()
                  call check_number ()
                  X_infinity = real_variable
              else if (field == "rate_constant_for_back_stress") then
                  call value (real_variable, free_format_error_flag) 
                  call check_eof ()
                  call check_number ()
                  gamma = real_variable
              else if (field == "krajcinovic/lemaitre_damage_model_material_constant") then
                  call value (real_variable, free_format_error_flag) 
                  call check_eof ()
                  call check_number ()
                  eta = real_variable
              else if (field == "accumulated_plastic_strain_threshold_for_damage") then
                  call value (real_variable, free_format_error_flag) 
                  call check_eof ()
                  call check_number ()
                  plastic_strain_threshold = real_variable
              else if (field == "damage_threshold_for_material_failure") then
                  call value (real_variable, free_format_error_flag) 
                  call check_eof ()
                  call check_number ()
                  failure_threshold = real_variable
              else if (field == "spin_test_frequency") then
                  call value (real_variable, free_format_error_flag) 
                  call check_eof ()
                  call check_number ()
                  spin_frequency = int(real_variable + 0.5_LONGreal)
              else if (field == "simulate_wire_spin_test") then
                  wire_test = .true.
              else if (field == "simulate_coil_spin_test") then
                  coil_test = .true.
              else if (field == "dump_constitutive_history") then
                  dump_history = .true.
              else if (field == "wire_radius" .or. field == "filar_radius") then
                  call value (real_variable, free_format_error_flag) 
                  call check_eof ()
                  call check_number ()
                  wire_radius = real_variable
              else if (field == "coil_radius") then
                  call value (real_variable, free_format_error_flag) 
                  call check_eof ()
                  call check_number ()
                  coil_radius = real_variable
              else if (field == "coil_pitch") then
                  call value (real_variable, free_format_error_flag) 
                  call check_eof ()
                  call check_number ()
                  coil_pitch = real_variable
              else if (field == "radius_of_curvature") then
                  call value (real_variable, free_format_error_flag) 
                  call check_eof ()
                  call check_number ()
                  radius_of_curvature = real_variable
              else
                  print *," "
                  print *,"unrecognized word in simulation input, abort."
                  print *," "
                  print *,"The unrecognized word was: ",field
                  print *," "
                  stop
              end if
          end if
      end do
!
      close (unit = nin)
!
!        compute time step
!
      if (spin_frequency > 0 .and. time_steps_per_spin_cycle > 0) then
          dt = 1.0_LONGreal/(real(spin_frequency,LONGreal) *                                &
                                                    real(time_steps_per_spin_cycle,LONGreal))
      end if
!
!        check input to catch obvious errors
!
      if (.not. wire_test .and. .not. coil_test) then
          print *," "
          print *,"The type of test was not specified in the input."
          print *,"You must either specify simulate_wire_spin_test or simulate_coil_spin_test."
          print *,"Simulation aborted.  Please correct the input and rerun."
          print *," "
          stop
      else if (wire_test .and. coil_test) then
          print *," "
          print *,"You have specified both simulate_wire_spin_test and simulate_coil_spin_test."
          print *,"Only one of these can be specified for a given simulation."
          print *,"Simulation aborted.  Please correct the input and rerun."
          print *," "
          stop
      end if
!
      n = 0
      if (Poissons_ratio > 0.0_LONGreal) then
          n = n + 1
      end if
      if (Youngs_modulus > 0.0_LONGreal) then
          n = n + 1
      end if
      if (shear_modulus > 0.0_LONGreal) then
          n = n + 1
      end if
!
      if (n < 2) then
          print *," "
          print *,"You must specify two of the following items in the input."
          print *,"     Poissons_ratio"
          print *,"     Youngs_modulus"
          print *,"     shear_modulus"
          if (n == 0) then
              print *,"None of these was specified."
          else if (n == 1) then
              if (Poissons_ratio > 0.0_LONGreal) then
                  print *,"Only the Poissons_ratio was specified."
              else if (Youngs_modulus > 0.0_LONGreal) then
                  print *,"Only the Youngs_modulus was specified."
              else if (shear_modulus > 0.0_LONGreal) then
                  print *,"Only the shear_modulus was specified."
              end if
          end if
          print *,"Simulation aborted.  Please correct the input and rerun."
          print *," "
          stop
      else if (crack_closure_parameter < -1.0e19_LONGreal) then
          print *," "
          print *,"crack_closure_parameter was not set in the input."
          print *,"Simulation aborted.  Please correct the input and rerun."
          print *," "
          stop
      else if (crack_closure_parameter <= 0.0_LONGreal) then
          print *," "
          print *,"The crack_closure_parameter was less than or equal to zero."
          print *,"This value must be greater than zero and less than or equal to one."
          print *,"Simulation aborted.  Please correct the input and rerun."
          print *," "
          stop
      else if (crack_closure_parameter > 1.0_LONGreal) then
          print *," "
          print *,"The crack_closure_parameter was greater than one."
          print *,"This value must be greater than zero and less than or equal to one."
          print *,"Simulation aborted.  Please correct the input and rerun."
          print *," "
          stop
      else if (abs(Poissons_ratio - 0.5_LONGreal) < 1.0e-10_LONGreal) then
          print *," "
          print *,"The Poissons_ratio was set to one-half.  This value is physically"
          print *,"unrealistic (it is appropriate only to fluids)."
          print *,"Simulation aborted.  Please correct the input and rerun."
          print *," "
          stop
      else if (time_steps_per_spin_cycle == -999) then
          print *," "
          print *,"time_steps_per_spin_cycle was not set in the input."
          print *,"Simulation aborted.  Please correct the input and rerun."
          print *," "
          stop
      else if (time_steps_per_spin_cycle == 0) then
          print *," "
          print *,"The time_steps_per_spin_cycle in the input was set to zero."        
          print *,"This value must be an integer greater than zero."
          print *,"Simulation aborted.  Please correct the input and rerun."
          print *," "
          stop
      else if (number_of_sample_points == -999) then
          print *," "
          print *,"number_of_radial_sample_points was not set in the input."
          print *,"Simulation aborted.  Please correct the input and rerun."
          print *," "
          stop
      else if (number_of_sample_points == 0) then
          print *," "
          print *,"number_of_radial_sample_points in the input was set to zero."        
          print *,"This value must be an integer greater than zero."
          print *,"Simulation aborted.  Please correct the input and rerun."
          print *," "
          stop
      else if (spin_frequency == -999) then
          print *," "
          print *,"spin_test_frequency was not set in the input."
          print *,"Simulation aborted.  Please correct the input and rerun."
          print *," "
          stop
      else if (spin_frequency == 0) then
          print *," "
          print *,"The spin_test_frequency in the input was set to zero."        
          print *,"This value must be an integer greater than zero."
          print *,"Simulation aborted.  Please correct the input and rerun."
          print *," "
          stop
      else if (cmax <= 0) then
          print *," "
          print *,"The maximum_simulation_spin_cycles are negative or zero in the input."
          print *,"Simulation aborted.  Please correct the input and rerun."
          print *," "
          stop
      else if (spin_cycle_dump_interval <= 0) then
          print *," "
          print *,"The spin_cycle_dump_interval is negative or zero in the input."
          print *,"Simulation aborted.  Please correct the input and rerun."
          print *," "
          stop
      else if (yield_stress < -1.0e19_LONGreal) then
          print *," "
          print *,"yield_stress was not set in the input."
          print *,"Simulation aborted.  Please correct the input and rerun."
          print *," "
          stop
      else if (R_infinity < -1.0e19_LONGreal) then
          print *," "
          print *,"aymptotic_stress_due_to_isotropic_strain_hardening was not set in the input."
          print *,"Simulation aborted.  Please correct the input and rerun."
          print *," "
          stop
      else if (b < -1.0e19_LONGreal) then
          print *," "
          print *,"rate_constant_for_isotropic_strain_hardening was not set in the input."
          print *,"Simulation aborted.  Please correct the input and rerun."
          print *," "
          stop
      else if (X_infinity < -1.0e19_LONGreal) then
          print *," "
          print *,"asymptotic_back_stress was not set in the input."
          print *,"Simulation aborted.  Please correct the input and rerun."
          print *," "
          stop
      else if (gamma < -1.0e19_LONGreal) then
          print *," "
          print *,"rate_constant_for_back_stress was not set in the input."
          print *,"Simulation aborted.  Please correct the input and rerun."
          print *," "
          stop
      else if (eta < -1.0e19_LONGreal) then
          print *," "
          print *,"krajcinovic/lemaitre_damage_model_material_constant was not set in the input."
          print *,"Simulation aborted.  Please correct the input and rerun."
          print *," "
          stop
      else if (plastic_strain_threshold < -1.0e19_LONGreal) then
          print *," "
          print *,"accumulated_plastic_strain_threshold_for_damage was not set in the input."
          print *,"Simulation aborted.  Please correct the input and rerun."
          print *," "
          stop
      else if (wire_radius < -1.0e19_LONGreal) then
          print *," "
          if (wire_test) then
              print *,"wire_radius (or equivalently, filar_radius) was not set in the input."
          else if (coil_test) then
              print *,"filar_radius (or equivalently, wire_radius) was not set in the input."
          end if
          print *,"Simulation aborted.  Please correct the input and rerun."
          print *," "
          stop
      else if (coil_test .and. coil_radius < -1.0e19_LONGreal) then
          print *," "
          print *,"coil_radius was not set in the input."
          print *,"Simulation aborted.  Please correct the input and rerun."
          print *," "
          stop
      else if (coil_test .and. coil_pitch < -1.0e19_LONGreal) then
          print *," "
          print *,"coil_pitch was not set in the input."
          print *,"Simulation aborted.  Please correct the input and rerun."
          print *," "
          stop
      else if (radius_of_curvature < -1.0e19_LONGreal) then
          print *," "
          print *,"radius_of_curvature was not set in the input."
          print *,"Simulation aborted.  Please correct the input and rerun."
          print *," "
          stop
      else if (failure_threshold < -1.0e19_LONGreal) then
          print *," "
          print *,"damage_threshold_for_material_failure was not set in the input."
          print *,"Simulation aborted.  Please correct the input and rerun."
          print *," "
          stop
      else if (failure_threshold <= 0.0_LONGreal .or. failure_threshold > 1.0_LONGreal) then
          print *," "
          print *,"damage_threshold_for_material_failure was invalid in the input."
          print *,"The value must be greater than zero and less than or equal to one."
          print *,"In the input, the value was ", failure_threshold
          print *,"Simulation aborted.  Please correct the input and rerun."
          print *," "
          stop
      end if
!
!        compute Lame constants
!
      if (Poissons_ratio > 0.0_LONGreal .and. Youngs_modulus > 0.0_LONGreal) then
          lambda = -(Youngs_modulus * Poissons_ratio)/(2.0_LONGreal * Poissons_ratio**2 +   &
                                                               Poissons_ratio - 1.0_LONGreal)
          mu = 0.5_LONGreal * Youngs_modulus / (1.0_LONGreal + Poissons_ratio)
      else if (Poissons_ratio > 0.0_LONGreal .and. shear_modulus > 0.0_LONGreal) then
          mu = shear_modulus
          lambda = (2.0_LONGreal * mu * Poissons_ratio)/(1.0_LONGreal - 2.0_LONGreal *      &
                                                                              Poissons_ratio)
      else if (Youngs_modulus > 0.0_LONGreal .and. shear_modulus > 0.0_LONGreal) then
          mu = shear_modulus
          lambda = mu * (2.0_LONGreal * mu - Youngs_modulus)/(3.0_LONGreal * mu -           &
                                                                              Youngs_modulus)
      end if
!
      close (unit=nin)
!
      end subroutine read_input

end module read_input_m
module computer_time_m

!
!      copyright (c) 1997 by quetzal computational associates, inc.
!      address any questions about this copyright notice to:
!
!         john k. prentice
!         quetzal computational associates, incorporated
!         3455 main avenue, suite 4
!         durango, co   81301
!         usa
!
!         phone:  970-382-8979
!         e-mail: quetzal@quetzalcoatl.com
!

implicit none

public :: computer_time

!
integer, parameter, private :: LONGreal = selected_real_kind(15,90)

contains


     subroutine computer_time (tnow)
!
!        purpose:     return the elapsed system time for this
!                     process on a sun microsystems computer.
!        coded:       f90 version coded 1 may 1993
!        author:      john k. prentice
!
!        input:    none
!
!        output:
!
!        tnow      real      elapsed system time (seconds)
!
!
      real (kind = LONGreal), intent(out) :: tnow
!
      logical, save :: first = .true.
      logical, save :: first_flip
      integer :: counted, count_rate, count_max
      real (kind = LONGreal) :: trate, tmax
      real (kind = LONGreal), save :: tfirst
!
      call system_clock (counted, count_rate, count_max)
      if (counted < 0 .or. count_rate == 0) then
          tnow = 0.0_LONGreal
      else
          tnow = real(counted,LONGreal)
          trate = real(count_rate,LONGreal)
          tnow = tnow/trate
!
          if (first) then
              first = .false.
              tfirst = tnow
              first_flip = .true.
          else if (tnow < tfirst) then
              if (.not. first_flip) then
                  tmax = real(count_max,LONGreal)/trate
                  tfirst = tfirst - tmax
              else
                  tmax = real(count_max,LONGreal)/trate
                  tfirst = -(tmax - tfirst)
                  first_flip = .false.
              end if
          end if
!
          tnow   = tnow - tfirst
      end if
!
      end subroutine computer_time

end module computer_time_m

module perdida_m
!
!      Copyright (C) 1997 by Quetzal Computational Associates, Inc.
!      Address any questions about this copyright notice to:
!
!         Dr. John K. Prentice
!         Quetzal Computational Associates, Incorporated
!         3455 Main Avenue, Suite 4
!         Durango, CO   81301
!         USA
!
!         Phone:  970-382-8979
!         e-mail: quetzal@quetzalcoatl.com
!

implicit none

public :: perdida
private :: generalized_hookes_law, damage_rate

integer, parameter, private :: LONGreal = selected_real_kind(15,90)

contains

      subroutine perdida (dt, lambda, mu, yield_stress, R_infinity, b, X_infinity, gamma,   &
                          eta, plastic_strain_threshold, stress_tensor, strain_tensor,      &
                          plastic_strain_tensor, strain_rate_tensor,                        &
                          accumulated_plastic_strain, back_stress_tensor,                   &
                          isotropic_hardening_stress, damage, failure_threshold,            &
                          crack_closure_parameter)
!
!      Author:       Dr. John K. Prentice
!      Affiliation:  Quetzal Computational Associates, Inc.
!      Dates:        28 November 1997
!
!      Purpose:      Cumulative damage constitutive model for ductile materials.
!
!############################################################################################
!
!      Input:
!
!        dt                           [selected_real_kind(15,90)]
!                                     time step (sec)
!
!        lambda                       [selected_real_kind(15,90)]
!                                     Lame constant Lambda (dynes/cm**2)
!
!        mu                           [selected_real_kind(15,90)]
!                                     Lame constant mu (dynes/cm**2)
!
!        yield_stress                 [selected_real_kind(15,90)]
!                                     yield stress of material (dynes/cm**2)
!
!        R_infinity                   [selected_real_kind(15,90)]
!                                     asymptotic stress due to isotropic strain hardening
!                                     (dynes/cm**2)
!
!        b                            [selected_real_kind(15,90)]
!                                     rate constant for stress due to isotropic strain 
!                                     hardening (dimensionless)
!
!        X_infinity                   [selected_real_kind(15,90)]
!                                     asymptotic back stress (dynes/cm**2)
!
!        gamma                        [selected_real_kind(15,90)]
!                                     rate constant for back stress 
!                                     (dimensionless)
!
!        eta                          [selected_real_kind(15,90)]
!                                     material constant for the Krajcinovic/Lemaitre ductile
!                                     damage model (ergs/gm)
!
!        plastic_strain_threshold     [selected_real_kind(15,90)]
!                                     accumulated plastic strain threshold for damage to
!                                     occur (dimensionless)
!
!
!        strain_tensor                [selected_real_kind(15,90), dimension(3,3)]
!                                     total strain tensor at end of time step 
!                                     (dimensionless)
!
!        plastic_strain_tensor        [selected_real_kind(15,90), dimension(3,3)]
!                                     total plastic strain tensor at beginning of time step 
!                                     (dimensionless)
!
!        strain_rate_tensor           [selected_real_kind(15,90), dimension(3,3)]
!                                     total strain rate tensor at beginning of time step 
!                                     (1/sec)
!
!        accumulated_plastic_strain   [selected_real_kind(15,90)]
!                                     accumulated plastic strain at beginning of time step
!                                     (dimensionless)
!
!        back_stress_tensor           [selected_real_kind(15,90)]
!                                     back stress tensor at the beginning of time step 
!                                     (dynes/cm**2)
!
!        isotropic_hardening_stress   [selected_real_kind(15,90)]
!                                     stress due to isotropic strain hardening at beginning
!                                     of time step (dynes/cm**2)
!
!        damage                       [selected_real_kind(15,90)]
!                                     cumulative damage at beginning of time step 
!                                     (dimensionless)
!
!        failure_threshold            [selected_real_kind(15,90)]
!                                     damage at which material failure occurs.
!                                     (dimensionless)
!
!        crack_closure_parameter      [selected_real_kind(15,90)]
!                                     if the material is in compression, this parameter
!                                     accounts for partial closure of cracks when 
!                                     calculating the stress
!
!  
!     Output:
!   
!        stress_tensor                [selected_real_kind(15,90), dimension(3,3)]
!                                     total stress tensor at end of time step (dynes/cm**2)
!
!        plastic_strain_tensor        [selected_real_kind(15,90), dimension(3,3)]
!                                     total plastic strain tensor at end of time step 
!                                     (dimensionless)
!
!        accumulated_plastic_strain   [selected_real_kind(15,90)]
!                                     accumulated plastic strain at end of time step 
!                                     (dimensionless)
!
!        back_stress_tensor           [selected_real_kind(15,90)]
!                                     back stress tensor at the end of time step
!                                     (dynes/cm**2)
!
!        isotropic_hardening_stress   [selected_real_kind(15,90)]
!                                     stress due to isotropic strain hardening at end of
!                                     time step (dynes/cm**2)
!
!        damage                       [selected_real_kind(15,90)]
!                                     cumulative damage at end of time step (dimensionless)
!
!############################################################################################
!
!
!=========== formal variables =============
!
      real (kind = LONGreal), intent(in) :: dt, yield_stress, lambda, mu, R_infinity, b,    &
                                            X_infinity, gamma, eta, failure_threshold,      &
                                            plastic_strain_threshold,                       &
                                            crack_closure_parameter
      real (kind = LONGreal), dimension(:,:), intent(in) :: strain_rate_tensor,             &
                                                            strain_tensor
      real (kind = LONGreal), dimension(:,:), intent(inout) :: plastic_strain_tensor,       &
                                                               back_stress_tensor
      real (kind = LONGreal), dimension(:,:), intent(out) :: stress_tensor
      real (kind = LONGreal), intent(inout) :: damage, accumulated_plastic_strain,          &
                                               isotropic_hardening_stress
!
!========== internal variables ============
!
      integer :: i, j
      real (kind = LONGreal), dimension(3,3) :: deviatoric_stress_tensor,                   &
                                                deviatoric_strain_rate_tensor,              &
                                                damaged_dev_stress_tensor,                  &
                                                plastic_strain_rate_tensor,                 &
                                                back_stress_rate_tensor,                    &
                                                deviatoric_strain_tensor,                   &
                                                old_plastic_strain_tensor
      real (kind = LONGreal) :: equivalent_stress, isotropic_hardening_stress_rate,         &
                                triaxiality_ratio, Poissons_ratio, yield_function,          &
                                Youngs_modulus, term1, term2, term3, xi_dot,                &
                                hydrostatic_stress, hydrostatic_strain_rate,                &
                                yield_function_dot, plastic_time_step, damage_dot,          &
                                discriminant, strain_energy_density, frac,                  &
                                accumulated_plastic_strain_rate, hydrostatic_strain,        &
                                crack_parameter
!
      Poissons_ratio = 0.5_LONGreal * lambda / (lambda + mu)
      Youngs_modulus = (1.0_LONGreal - 2.0_LONGreal * Poissons_ratio) * (3.0_LONGreal *     &
                                                                  lambda + 2.0_LONGreal * mu)
!
!        make an initial calculation of the stress
!
      stress_tensor(:,:) = (1.0_LONGreal - damage) *                                        &
                                      generalized_hookes_law (strain_tensor(:,:) -          &
                                                      plastic_strain_tensor(:,:), lambda, mu)
!
!        compute hydrostatic stress at beginning of time step
!
      hydrostatic_stress = (stress_tensor(1,1) + stress_tensor(2,2) +                       &
                                                             stress_tensor(3,3))/3.0_LONGreal
!
!        if in compression, modify the stress to account for partial closure of cracks
!
      if (hydrostatic_stress < -1.0e-10_LONGreal .and. damage > 0.0_LONGreal) then
          crack_parameter = crack_closure_parameter
          stress_tensor(:,:) = (1.0_LONGreal - crack_parameter * damage) /                  &
                                                 (1.0_LONGreal - damage) * stress_tensor(:,:)
          hydrostatic_stress = (stress_tensor(1,1) + stress_tensor(2,2) +                   &
                                                             stress_tensor(3,3))/3.0_LONGreal
      else
          crack_parameter = 1.0_LONGreal
      end if 
!
!        compute deviatoric stress tensor at beginning of time step
!
      deviatoric_stress_tensor(:,:) = stress_tensor(:,:)
      deviatoric_stress_tensor(1,1) = stress_tensor(1,1) - hydrostatic_stress
      deviatoric_stress_tensor(2,2) = stress_tensor(2,2) - hydrostatic_stress
      deviatoric_stress_tensor(3,3) = stress_tensor(3,3) - hydrostatic_stress
!
      damaged_dev_stress_tensor(:,:) = deviatoric_stress_tensor(:,:)/(1.0_LONGreal -        &
                                                                    crack_parameter * damage)
!
!        compute the yield function
!
      equivalent_stress = sqrt(1.5_LONGreal* sum((damaged_dev_stress_tensor(:,:)            &
                                                              - back_stress_tensor(:,:))**2))
      yield_function =  equivalent_stress - isotropic_hardening_stress - yield_stress
!
!        if the yeild function is non-negative, compute the time derivative also.  If 
!        the yield function is negative, arbitrarily set the time derivative to a negative
!        number.  It doesn't matter what the value is, we just want to set a flag to
!        identify that no plastic strain is being accumulated in this time step.  
!
!        For the purposes of computing the time derivative of the yield function, for
!        computational convenience, we ignore the time derivative of damage and also
!        of the back stress. 
!
      if (yield_function < 0.0_LONGreal .or. equivalent_stress < 1.0e-10_LONGreal) then
          yield_function_dot = -1.0_LONGreal
      else
          hydrostatic_strain_rate = (strain_rate_tensor(1,1) + strain_rate_tensor(2,2) +    &
                                                        strain_rate_tensor(3,3))/3.0_LONGreal
          deviatoric_strain_rate_tensor(:,:) = strain_rate_tensor(:,:)
          deviatoric_strain_rate_tensor(1,1) = strain_rate_tensor(1,1) -                    &
                                                                      hydrostatic_strain_rate
          deviatoric_strain_rate_tensor(2,2) = strain_rate_tensor(2,2) -                    &
                                                                      hydrostatic_strain_rate
          deviatoric_strain_rate_tensor(3,3) = strain_rate_tensor(3,3) -                    &
                                                                      hydrostatic_strain_rate
!          
          yield_function_dot = 1.5_LONGreal / equivalent_stress *                           &
                          sum((damaged_dev_stress_tensor(:,:) - back_stress_tensor(:,:)) *  & 
                                generalized_hookes_law (strain_rate_tensor(:,:), lambda, mu))
      end if
!
!       if the yield function and/or its time derivative are negative, then we are done.
!       In this case, the strain is all elastic and the stress we have calculated is 
!       correct.  However, if the yield function and its time derivative are non-negative,
!       then we have exceeded the yield condition.  In this case, plastic work is being
!       done and we need to yield the material.
!
      if (yield_function >= 0.0_LONGreal .and. yield_function_dot >= 0.0_LONGreal) then
          old_plastic_strain_tensor(:,:) = plastic_strain_tensor(:,:)
!
!       modify the stress tensor to account for plasticity
!
          term1 = sum(damaged_dev_stress_tensor(:,:)**2)
          term2 = -2.0_LONGreal * sum(damaged_dev_stress_tensor(:,:) *                      &
                                                                     back_stress_tensor(:,:))
          term3 = sum(back_stress_tensor(:,:)**2) - (isotropic_hardening_stress +           &
                                                              yield_stress)**2 / 1.5_LONGreal
          discriminant = term2**2 - 4.0_LONGreal * term1 * term3
!
          if (discriminant < 0.0_LONGreal) then
              print *,"discriminant is negative in perdida, abort."
              stop
          end if
!
          frac = (-term2 + sqrt(discriminant))/(2.0_LONGreal * term1)
          if (frac < 0.0_LONGreal .or. frac > 1.0_LONGreal) then
              print *,"bad plastic strain fraction in perdida, abort.",frac
              stop
          end if
!              
          deviatoric_stress_tensor(:,:) = frac * deviatoric_stress_tensor(:,:)
          stress_tensor(:,:) = deviatoric_stress_tensor(:,:)
          stress_tensor(1,1) = stress_tensor(1,1) + hydrostatic_stress
          stress_tensor(2,2) = stress_tensor(2,2) + hydrostatic_stress
          stress_tensor(3,3) = stress_tensor(3,3) + hydrostatic_stress
!
!       somewhat inconsistently, compute the real accumulated plastic strain from the
!       total strain tensor using the yield function to pick out the elastic piece of
!       the strain
!
          hydrostatic_strain = (strain_tensor(1,1) + strain_tensor(2,2) +                   &
                                                          strain_tensor(3,3)) / 3.0_LONGreal
          deviatoric_strain_tensor(:,:) = strain_tensor(:,:)
          deviatoric_strain_tensor(1,1) = deviatoric_strain_tensor(1,1) - hydrostatic_strain
          deviatoric_strain_tensor(2,2) = deviatoric_strain_tensor(2,2) - hydrostatic_strain
          deviatoric_strain_tensor(3,3) = deviatoric_strain_tensor(3,3) - hydrostatic_strain
          plastic_strain_tensor(:,:) = frac * plastic_strain_tensor(:,:) + (1.0_LONGreal -  &
                                                        frac) * deviatoric_strain_tensor(:,:)
!
!       The time over which the plasticity and damage parameters are to be integrated
!       may be shorter than the current time step since we may have been elastic during
!       part of this time step.  So compute the plastic time step.
!
          plastic_time_step = (1.0_LONGreal - frac) * dt
!
!       compute the plastic strain rate tensor
!
          if (plastic_time_step <= 0.0_LONGreal) then
              plastic_strain_rate_tensor(:,:) = 0.0_LONGreal
          else
              plastic_strain_rate_tensor(:,:) = (plastic_strain_tensor(:,:) -              &
                                        old_plastic_strain_tensor(:,:)) / plastic_time_step 
          end if
!
!       Compute the accumulated plastic strain rate and the time derivative of
!       the Lagrange multiplier.
!
          accumulated_plastic_strain_rate = sqrt(sum(plastic_strain_rate_tensor(:,:)**2) /  &
                                                                                1.5_LONGreal)
          xi_dot = (1.0_LONGreal - crack_parameter * damage) *                              &
                                                              accumulated_plastic_strain_rate 
!
!       Next integrate the accumulated plastic strain rate to get the
!       new accumulated plastic strain
!
          accumulated_plastic_strain = accumulated_plastic_strain + plastic_time_step *     &
                                                              accumulated_plastic_strain_rate
!
!       Calculate the new back stress tensor and the isotropic stress
!
          back_stress_rate_tensor(:,:) = gamma * (X_infinity * (1.0_LONGreal -              &
                                           crack_parameter * damage) / 1.5_LONGreal *       &
                                                    plastic_strain_rate_tensor(:,:) -       &
                                                            back_stress_tensor(:,:) * xi_dot)
          isotropic_hardening_stress_rate = b * (R_infinity - isotropic_hardening_stress) * &
                                                                                       xi_dot
          back_stress_tensor(:,:) = back_stress_tensor(:,:) +                               &
                                             plastic_time_step * back_stress_rate_tensor(:,:)
          isotropic_hardening_stress = isotropic_hardening_stress +                         &
                                          plastic_time_step * isotropic_hardening_stress_rate
!
!       Finally, calculate the damage if the hydrostatic stress is positive (we assume
!       negative is compression and positive is tension)
!
          if (hydrostatic_stress > -1.0e-10_LONGreal) then
              equivalent_stress = sqrt(1.5_LONGreal* sum(damaged_dev_stress_tensor(:,:)**2))
              triaxiality_ratio = 2.0_LONGreal / 3.0_LONGreal * (1.0_LONGreal +             &
                                    Poissons_ratio) + 3.0_LONGreal * (1.0_LONGreal -        &
                                    2.0_LONGreal * Poissons_ratio) * (hydrostatic_stress /  &
                                    equivalent_stress)**2
              strain_energy_density = 0.5_LONGreal * equivalent_stress**2 *                 &
                                                           triaxiality_ratio / Youngs_modulus
              damage_dot = damage_rate (eta, accumulated_plastic_strain_rate,               &
                                        strain_energy_density, accumulated_plastic_strain,  &
                                            plastic_strain_threshold)
              damage = min(1.0_LONGreal, damage + damage_dot * plastic_time_step)
!
!      if the damage exceeds the failure threshold, fail the material completely
!
              if (damage >= failure_threshold) then
                  damage = 1.0_LONGreal
              end if
!
          end if
!
      end if
!
      end subroutine perdida


!---------------------------- FUNCTION GENERALIZED_HOOKES_LAW -------------------------------

      function generalized_hookes_law (strain_tensor, lambda, mu) result (stress_tensor)
!
!      Author:       Dr. John K. Prentice
!      Affiliation:  Quetzal Computational Associates, Inc.
!      Dates:        28 November 1997
!
!      Purpose:      Apply the generalized Hooke's law for elasticity to the strain tensor
!                    (or strain rate tensor) to compute the stress tensor (or stress rate
!                    tensor)
!
!############################################################################################
!
!      Input:
!
!        strain_tensor                [selected_real_kind(15,90), dimension(3,3)]
!                                     stress tensor
!
!        lambda                       [selected_real_kind(15,90)]
!                                     Lame constant Lambda
!
!        mu                           [selected_real_kind(15,90)]
!                                     Lame constant mu
!
!     Output:
!
!        stress_tensor                [selected_real_kind(15,90), dimension(3,3)]
!                                     stress tensor
!
!############################################################################################
!
!
!=========== formal variables =============
!
      real (kind = LONGreal), dimension(:,:), intent(in) :: strain_tensor
      real (kind = LONGreal), intent(in) :: lambda, mu
      real (kind = LONGreal), dimension(3,3) :: stress_tensor
!
!========== internal variables ============
!
      real (kind = LONGreal), dimension(6) ::generalized_strain_vector,                     &
                                             generalized_stress_vector
      real (kind = LONGreal), dimension(6,6) :: generalized_constitutive_tensor
      integer :: i
!
!        construct the generalized constitutive tensor for elasticity
!
      generalized_constitutive_tensor(:,:) = 0.0_LONGreal
      generalized_constitutive_tensor(1,1) = lambda + 2.0_LONGreal * mu
      generalized_constitutive_tensor(1,2) = lambda
      generalized_constitutive_tensor(1,3) = lambda
      generalized_constitutive_tensor(2,1) = lambda
      generalized_constitutive_tensor(2,2) = lambda + 2.0_LONGreal * mu
      generalized_constitutive_tensor(2,3) = lambda
      generalized_constitutive_tensor(3,1) = lambda
      generalized_constitutive_tensor(3,2) = lambda
      generalized_constitutive_tensor(3,3) = lambda + 2.0_LONGreal * mu
      generalized_constitutive_tensor(4,4) = mu
      generalized_constitutive_tensor(5,5) = mu
      generalized_constitutive_tensor(6,6) = mu
!
!        construct the generalized strain vector (using double index notation)
!
      generalized_strain_vector(1) = strain_tensor(1,1)
      generalized_strain_vector(2) = strain_tensor(2,2)
      generalized_strain_vector(3) = strain_tensor(3,3)
      generalized_strain_vector(4) = strain_tensor(2,3)
      generalized_strain_vector(5) = strain_tensor(1,3)
      generalized_strain_vector(6) = strain_tensor(1,2)
!
!        compute the generalized stress vector
!
      do i = 1, 6
          generalized_stress_vector(i) = dot_product(generalized_constitutive_tensor(i,:),  &   
                                                                generalized_strain_vector(:))
      end do
!
!        update the stress tensor 
!
      stress_tensor(1,1) = generalized_stress_vector(1)
      stress_tensor(2,2) = generalized_stress_vector(2)
      stress_tensor(3,3) = generalized_stress_vector(3)
      stress_tensor(2,3) = generalized_stress_vector(4)
      stress_tensor(1,3) = generalized_stress_vector(5)
      stress_tensor(1,2) = generalized_stress_vector(6)
      stress_tensor(3,2) = stress_tensor(2,3)
      stress_tensor(3,1) = stress_tensor(1,3)
      stress_tensor(2,1) = stress_tensor(1,2)
!
      end function generalized_hookes_law

!--------------------------- FUNCTION DAMAGE_RATE --------------------------------------------

      function damage_rate (eta, accumulated_plastic_strain_rate, strain_energy_density,    &
                                     accumulated_plastic_strain, plastic_strain_threshold)  &
                                             result (damage_dot)
!
!      Author:       Dr. John K. Prentice
!      Affiliation:  Quetzal Computational Associates, Inc.
!      Dates:        28 November 1997
!
!      Purpose:      Compute the time derivative of the damage.  This routine uses the damage 
!                    potential function of Krajcinovic and Lemaitre (1987).
!
!############################################################################################
!
!      Input:
!
!        eta                          [real, selected_real_kind(15,90)]
!                                     material specific constant (kg-sec/J)
!
!        accumulated_plastic_strain_rate   [real, selected_real_kind(15,90)]
!                                     accumulated plastic strain rate (1/sec)
!
!        strain_energy_density        [real, selected_real_kind(15,90)]
!                                     strain energy density
!
!        accumulated_plastic_strain   [real, selected_real_kind(15,90)]
!                                     accumulated plastic strain (dimensionless)
!
!        plastic_strain_threshold     [real, selected_real_kind(15,90)]
!                                     plastic strain threshold.  Damage will nucleate only
!                                     if the accumulated plastic strain exceeds this
!                                     threshold (dimensionless)
!
!
!     Output:
!
!        damage_dot                   [real, selected_real_kind(15,90)]
!                                     damage rate (1/sec)
!
!############################################################################################
!
!
!=========== formal variables =============
!
      real (kind = LONGreal), intent(in) :: accumulated_plastic_strain,                     &
                                            accumulated_plastic_strain_rate,                &
                                            plastic_strain_threshold, eta,                  &
                                            strain_energy_density
      real (kind = LONGreal) :: damage_dot
!
!========== internal variables ============
!
      if (accumulated_plastic_strain < plastic_strain_threshold) then
          damage_dot = 0.0_LONGreal
      else
          damage_dot = strain_energy_density / eta * accumulated_plastic_strain_rate
      end if
!
      end function damage_rate

end module perdida_m

      program iztaccihuatl
!
!############################################################################################
!
!      Author:       Dr. John K. Prentice
!      Affiliation:  Quetzal Computational Associates, Inc.
!      Dates:        28 November 1997
!
!      Purpose:      Driver for modeling ductile metal fatigue.
!
!      Copyright (C) 1997 by Quetzal Computational Associates, Inc.
!      Address any questions about this copyright notice to:
!
!         Dr. John K. Prentice
!         Quetzal Computational Associates, Incorporated
!         3455 Main Avenue, Suite 4
!         Durango, CO   81301
!         USA
!
!         Phone:  970-382-8979
!         e-mail: quetzal@quetzalcoatl.com
!
!############################################################################################
!
      use read_input_m
      use perdida_m
      use computer_time_m
!
      implicit none
!
      integer, parameter :: LONGreal = selected_real_kind(15,90)
      real (kind = LONGreal), parameter :: pi = 3.1415926535_LONGreal
      real (kind = LONGreal), parameter :: twopi = 2.0_LONGreal * pi
!
      real (kind = LONGreal) :: dt, yield_stress, lambda, mu, R_infinity, b, X_infinity,    &
                                gamma, eta, plastic_strain_threshold, wire_radius,          &
                                coil_radius, coil_pitch, radius_of_curvature, time,         &
                                failure_threshold, dr, Poissons_ratio, bulk_modulus,        &
                                coefficient, total_damage, total_surface_area, tnow,        &
                                crack_closure_parameter
      real (kind = LONGreal), dimension(:,:,:), allocatable :: strain_rate_tensor,          &
                                                               strain_tensor,               &
                                                               stress_tensor,               &
                                                               plastic_strain_tensor,       &
                                                               back_stress_tensor
      real (kind = LONGreal), dimension(:), allocatable :: damage, radius,                  &
                                                           accumulated_plastic_strain,      &
                                                           isotropic_hardening_stress,      &
                                                           max_strain, max_stress,          &
                                                           surface_area
      integer :: number_of_sample_points, spin_frequency, spin_cycles, cmax, n,             & 
                 time_steps_per_spin_cycle, next_dump, iteration_count,                     &
                 spin_cycle_dump_interval, nn
      character (len=80) :: input_file, output_file, file_name
      character (len=1) :: answer
      logical :: wire_test, coil_test, exists, failure, print_max, dump_history
      logical, dimension(:), allocatable :: failed
!
      input_file = "fatigue.in"
!
      inquire (file=input_file, exist=exists)
      if (.not. exists) then
          print *," "
          print *,"Input file does not exist, abort."
          print *," "
          stop
      end if
!
      output_file = "FATIGUE.OUT"
!
      open (unit=11,file=output_file,status="unknown",form="formatted",action="write")
!
!        read input file
!
      call read_input (input_file, dt, cmax, lambda, mu, yield_stress, R_infinity, b,       &
                       X_infinity, gamma, eta, plastic_strain_threshold, failure_threshold, &
                       spin_frequency, wire_test, coil_test, wire_radius, coil_radius,      &
                       coil_pitch, radius_of_curvature, time_steps_per_spin_cycle,          &
                       number_of_sample_points, dump_history, crack_closure_parameter,      &
                       spin_cycle_dump_interval)
!
!        allocate necessary memory
!
      allocate (strain_rate_tensor(3,3,number_of_sample_points))
      allocate (strain_tensor(3,3,number_of_sample_points))
      allocate (stress_tensor(3,3,number_of_sample_points))
      allocate (plastic_strain_tensor(3,3,number_of_sample_points))
      allocate (back_stress_tensor(3,3,number_of_sample_points))
      allocate (damage(number_of_sample_points))
      allocate (accumulated_plastic_strain(number_of_sample_points))
      allocate (isotropic_hardening_stress(number_of_sample_points))
      allocate (failed(number_of_sample_points))
      allocate (radius(number_of_sample_points))
      allocate (max_strain(number_of_sample_points))
      allocate (max_stress(number_of_sample_points))
      allocate (surface_area(number_of_sample_points))
!
!        compute the radius from the center of the wire for each of the sample points
!
      dr = wire_radius / real(number_of_sample_points, LONGreal)
      do n = 1, number_of_sample_points
          radius(n) = 0.5_LONGreal * dr + real(n-1,LONGreal) * dr
          surface_area(n) = pi * ((radius(n)+0.5_LONGreal*dr)**2 -                       &
                                                            (radius(n)-0.5_LONGreal*dr)**2)
      end do
      total_surface_area = pi * wire_radius**2
!
!        initialize constitutive values
!
      strain_tensor = 0.0_LONGreal
      strain_rate_tensor = 0.0_LONGreal
      stress_tensor = 0.0_LONGreal
      plastic_strain_tensor = 0.0_LONGreal
      back_stress_tensor = 0.0_LONGreal
      damage = 0.0_LONGreal
      accumulated_plastic_strain = 0.0_LONGreal
      isotropic_hardening_stress = 0.0_LONGreal
      failed = .false.
      bulk_modulus = (3.0_LONGreal * lambda + 2.0_LONGreal * mu) / 3.0_LONGreal
      Poissons_ratio = 0.5_LONGreal * (3.0_LONGreal * bulk_modulus - 2.0_LONGreal * mu) /   &
                                                           (3.0_LONGreal * bulk_modulus + mu)
      next_dump = 1
!
!        loop over time steps and perform calculation
!
      time = 0.0_LONGreal
      spin_cycles = 0
      max_strain = -huge(1.0_LONGreal)
      max_stress = -huge(1.0_LONGreal)
      print_max = .true.
      iteration_count = 9999
      do
         if (iteration_count < time_steps_per_spin_cycle) then
             iteration_count = iteration_count + 1
         else
             iteration_count = 1
             spin_cycles = spin_cycles + 1
         end if
         time = time + dt
!
!        define the total strain and strain rate tensors for this time step
!
!########
!        In order to remove proprietary information, the calculations done for the coil
!        test have been removed.  Consequently, this code only works for a wire
!        test.
!########
!
         if (wire_test) then
               do n = 1, number_of_sample_points
                   coefficient = (radius(n) / radius_of_curvature) * sin(twopi *            &
                                                       real(spin_frequency,LONGreal) * time)
                   strain_tensor(1,1,n) = - coefficient * Poissons_ratio
                   strain_tensor(2,2,n) = - coefficient * Poissons_ratio
                   strain_tensor(3,3,n) = coefficient
!
                   coefficient = twopi * real(spin_frequency,LONGreal) * (radius(n) /       &
                                                    radius_of_curvature) * cos(twopi *      &
                                                        real(spin_frequency,LONGreal) * time)

                   strain_rate_tensor(1,1,n) = -coefficient * Poissons_ratio
                   strain_rate_tensor(2,2,n) = - coefficient * Poissons_ratio
                   strain_rate_tensor(3,3,n) = coefficient
               end do
         else if (coil_test) then
             print *,"Abort, coil test not supported"
         end if
!
!        call the damage model to update the constitutive values for this material
!    
         do n = 1, number_of_sample_points
             if (.not. failed(n)) then
                 call perdida (dt, lambda, mu, yield_stress, R_infinity, b, X_infinity,     &
                               gamma, eta, plastic_strain_threshold, stress_tensor(:,:,n),  &
                               strain_tensor(:,:,n), plastic_strain_tensor(:,:,n),          &
                               strain_rate_tensor(:,:,n), accumulated_plastic_strain(n),    &
                               back_stress_tensor(:,:,n), isotropic_hardening_stress(n),    &
                               damage(n), failure_threshold, crack_closure_parameter)
             end if
         end do
!
         if (spin_cycles == 1 .and. print_max) then
             do n = 1, number_of_sample_points
                 max_strain(n) = max(max_strain(n), maxval(strain_tensor(:,:,n)))
                 max_stress(n) = max(max_stress(n), maxval(stress_tensor(:,:,n)))
             end do
         else if (print_max) then
             print_max = .false.
             write (unit=11,fmt="(a)") " "
             write (unit=11,fmt="(a)")                                                      &
                             "Maximum strains and stresses imposed on the undamaged sample: " 
             write (unit=11,fmt="(a)") " "
             do n = 1, number_of_sample_points
                 write (unit=11,fmt="(a,i4,a,1pe14.5,a,e14.5,a,e14.5)") "region ", n,       &
                              " r=",radius(n), " emax=",max_strain(n), " smax=",max_stress(n)
             end do
             write (unit=11,fmt="(a)") " "
             if (minval(accumulated_plastic_strain(:)) <= 0.0_LONGreal) then
                 total_damage = 0.0_LONGreal
                 do n = 1, number_of_sample_points
                    if (accumulated_plastic_strain(n) > 0.0_LONGreal) then
                        total_damage = total_damage + surface_area(n)
                    end if
                 end do
                 total_damage = total_damage / total_surface_area
                 if (total_damage < failure_threshold) then
                     write (unit=*,fmt="(a)") "Simulation terminated."
                     write (unit=*,fmt="(a)") "This sample will never fracture." 
                     write (unit=*,fmt="(a)")                                              &
                                             "The imposed stress is below the fatigue limit."
                     write (unit=*,fmt="(a)") "See the output file for more information."
                     print *," "
                     write (unit=11,fmt="(a)") "Simulation terminated."
                     write (unit=11,fmt="(a)") "This sample will never fracture." 
                     write (unit=11,fmt="(a)")                                              &
                                             "The imposed stress is below the fatigue limit." 
                     do n = 1, number_of_sample_points
                        if (accumulated_plastic_strain(n) > 0.0_LONGreal) then
                            write (unit=11,fmt="(a,1pe15.5)")                               &
                                                   "Damage can nucleate at radius ",radius(n)
                        end if
                     end do
                     stop
                 end if
             end if
         end if
!
!        update the failed array
!
         do n = 1, number_of_sample_points
             if (.not. failed(n) .and. damage(n) >= 1.0_LONGreal) then
                 failed(n) = .true.
                 print *," "
                 write (unit=*,fmt="(a)") " "
                 write (unit=*,fmt="(a,i4,a,1pe15.5,a,i9)")                                 &
                                      "-------> Material failure in region ", n," at r= ",  &
                                                           radius(n)," at cycle ",spin_cycles
                 write (unit=11,fmt="(a)") " "
                 write (unit=11,fmt="(a,i4,a,1pe15.5,a,i9)")                                &
                                      "-------> Material failure in region ", n," at r= ",  &
                                                           radius(n)," at cycle ",spin_cycles
             end if
         end do
!
!        check the damage to see if all fatigue criteria have been satisfied.  If so,
!        exit
!
         total_damage = sum(surface_area(:) * damage(:)) / total_surface_area
         if (total_damage >= failure_threshold) then
             failure = .true.
         else
             failure = .false.
         end if 
!
!         decide whether to take another time step or to stop simulation now
!
         if (failure) then
             write (unit=*,fmt="(a)") " "
             write (unit=*,fmt="(a)") "Simulation terminated."
             write (unit=*,fmt="(a,i10)") "     Spin cycle =       ",spin_cycles
             write (unit=*,fmt="(a,1pe15.5)") "     Time (sec)       = ", time
             write (unit=11,fmt="(a)") " "
             write (unit=11,fmt="(a)") "Simulation terminated."
             write (unit=11,fmt="(a,i10)") "     Spin cycle =       ",spin_cycles
             write (unit=11,fmt="(a,1pe15.5)") "     Time (sec)       = ", time
             if (wire_test) then
                 write (unit=*,fmt="(a,1pe15.5)")                                           &
                                      "The wire has fractured, total damage = ", total_damage
                 write (unit=11,fmt="(a,1pe15.5)")                                          &
                                      "The wire has fractured, total damage = ", total_damage
             else if (coil_test) then
                 write (unit=11,fmt="(a,1pe15.5)")                                          &
                                      "The coil has fractured, total damage = ", total_damage
                 print *,"The coil has fractured, total damage = ", total_damage
             end if
             write (unit=11,fmt="(a)") " "
             write (unit=11,fmt="(a)") "Final damage results:"
             do n = 1, number_of_sample_points
                 write (unit=11,fmt="(a,1pe14.5,a,e14.5,a,e14.5)") "     r = ",radius(n),   &
                       " p = ", accumulated_plastic_strain(n)," D = ", damage(n)
             end do
             write (unit=11,fmt="(a)") " "
             print *," "
             call computer_time (tnow)          
             write (unit=*,fmt="(a,1pe15.5)") "================= Elasped CPU time  = ", tnow
             write (unit=*,fmt="(a)") " "
             write (unit=11,fmt="(a,1pe15.5)") "================= Elasped CPU time  = ", tnow
             write (unit=11,fmt="(a)") " "
             exit
         else if (spin_cycles >= cmax .and.                                                 &
                                          iteration_count == time_steps_per_spin_cycle) then
             write (unit=11,fmt="(a)") " "
             write (unit=11,fmt="(a)") "Simulation terminated."
             write (unit=11,fmt="(a,i10)") "     Spin cycle =       ",spin_cycles
             write (unit=11,fmt="(a,1pe15.5)") "     Time (sec)       = ", time
             write (unit=11,fmt="(a)")                                                      &
                                 "The maximum number of simulation spin cycles was exceeded."
             write (unit=*,fmt="(a)") " "
             write (unit=*,fmt="(a)") "Simulation terminated."
             write (unit=*,fmt="(a,i10)") "     Spin cycle =       ",spin_cycles
             write (unit=*,fmt="(a,1pe15.5)") "     Time (sec)       = ", time
             write (unit=*,fmt="(a)")                                                      &
                                 "The maximum number of simulation spin cycles was exceeded."
             write (unit=11,fmt="(a)") "Final damage results:"
             do n = 1, number_of_sample_points
                 write (unit=11,fmt="(a,1pe14.5,a,e14.5,a,e14.5)") "     r = ",radius(n),   &
                       " p = ", accumulated_plastic_strain(n)," D = ", damage(n)
             end do
             write (unit=11,fmt="(a,f6.2)") "Total damage = ",total_damage
             write (unit=11,fmt="(a)") " "
             print *," "
             call computer_time (tnow)
             write (unit=*,fmt="(a,1pe15.5)") "================= Elasped CPU time  = ", tnow
             write (unit=*,fmt="(a)") " "
             write (unit=11,fmt="(a,1pe15.5)") "================= Elasped CPU time  = ", tnow
             write (unit=11,fmt="(a)") " "
             exit
         else if (spin_cycles >= next_dump .and.                                            &
                                           iteration_count == time_steps_per_spin_cycle) then
!
             if (next_dump == 1 .and. spin_cycle_dump_interval > 1) then
                 next_dump = spin_cycle_dump_interval
             else
                 next_dump = spin_cycles + spin_cycle_dump_interval
             end if 
!
             write (unit=11,fmt="(a)") " "
             write (unit=11,fmt="(a,i10)") "================= Spin cycle number = ",        &
                                                                                  spin_cycles
             write (unit=11,fmt="(a,1pe15.5)") "================= Simulation time   = ",    &
                                                                                         time
             write (unit=*,fmt="(a)") " " 
             write (unit=*,fmt="(a,i10)") "================= Spin cycle number = ",         &
                                                                                  spin_cycles
             write (unit=*,fmt="(a,1pe15.5)") "================= Simulation time   = ",     &
                                                                                         time
             call computer_time (tnow)
             write (unit=*,fmt="(a,1pe15.5)") "================= Elasped CPU time  = ", tnow
             write (unit=11,fmt="(a,1pe15.5)") "================= Elasped CPU time  = ", tnow
             write (unit=11,fmt="(a)") "================= Current results:"
             print *," "
             write (unit=11,fmt="(a)") " "
             if (maxval(damage(:)) > 0.0_LONGreal) then
                 do n = 1, number_of_sample_points
                     if (accumulated_plastic_strain(n) > 0.0_LONGreal) then
                         write (unit=11,fmt="(a,1pe14.5,a,e14.5,a,e14.5)") "     r = ",     &
                                          radius(n)," p = ", accumulated_plastic_strain(n), &
                                                                            " D = ",damage(n)
                     end if
                 end do     
             else if (spin_cycles /= 1) then
                 write (unit=11,fmt="(a)") "No damage has nucleated at this time."
             end if
             write (unit=11,fmt="(a)") " "
!
!        dump history points, if necessary
!
             if (dump_history) then
                 if (spin_cycles < 10) then
                     write(unit=file_name,fmt="(a,i1,a)") "cycle_",spin_cycles,".dat"
                 else if (spin_cycles < 100) then
                     write(unit=file_name,fmt="(a,i2,a)") "cycle_",spin_cycles,".dat"
                 else if (spin_cycles < 1000) then
                     write(unit=file_name,fmt="(a,i3,a)") "cycle_",spin_cycles,".dat"
                 else if (spin_cycles < 10000) then
                     write(unit=file_name,fmt="(a,i4,a)") "cycle_",spin_cycles,".dat"
                 else if (spin_cycles < 100000) then
                     write(unit=file_name,fmt="(a,i5,a)") "cycle_",spin_cycles,".dat"
                 else if (spin_cycles < 1000000) then
                     write(unit=file_name,fmt="(a,i6,a)") "cycle_",spin_cycles,".dat"
                 else if (spin_cycles < 10000000) then
                     write(unit=file_name,fmt="(a,i7,a)") "cycle_",spin_cycles,".dat"
                 else if (spin_cycles < 100000000) then
                     write(unit=file_name,fmt="(a,i8,a)") "cycle_",spin_cycles,".dat"
                 else if (spin_cycles < 1000000000) then
                     write(unit=file_name,fmt="(a,i9,a)") "cycle_",spin_cycles,".dat"
                 else
                     print *," "
                     print *,"Warning:  Number of spin cycles is too large to continue "
                     print *,"          to dump history files."
                     print *," "
                     dump_history = .false.
                     end if
             end if
!
             if (dump_history) then
                 open (unit=12,file=file_name,status="unknown",form="formatted",           &
                                                                              action="write")
                 do nn = 1, number_of_sample_points
                     write (unit=12,fmt="(i4,2x,1p4e15.5)") nn, radius(nn),                 &
                                                   accumulated_plastic_strain(nn), damage(nn)
                 end do
                 close (unit = 12)
             end if
         end if
      end do
!
!        deallocate memory
!
      deallocate (strain_rate_tensor)
      deallocate (strain_tensor)
      deallocate (stress_tensor)
      deallocate (plastic_strain_tensor)
      deallocate (back_stress_tensor)
      deallocate (damage)
      deallocate (accumulated_plastic_strain)
      deallocate (isotropic_hardening_stress)
      deallocate (failed)
      deallocate (max_strain)
      deallocate (max_stress)
      deallocate (surface_area)
!
      close (unit = 11)
!
      end program iztaccihuatl

