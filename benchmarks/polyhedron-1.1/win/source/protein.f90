module kind_definitions
    integer, parameter :: LONGreal = selected_real_kind(12,90)  
    integer, parameter :: SHORTreal = kind(1.0)
end module kind_definitions
MODULE TIMING_M

USE kind_definitions

real :: total_conformation_time, initial_conformation_time, final_conformation_time,     &
&       percent_conformation_time
real :: total_energy_time, initial_energy_time, final_energy_time, percent_energy_time
real :: total_time, initial_time, final_time

CONTAINS

subroutine second (tnow)
!
!        Copyright (C) 1993 John K. Prentice
!                           2119 Matthew N.W.
!                           Albuquerque, NM   87104-3210
!                           USA
!
!        PURPOSE:     Return the elapsed system time for this
!                     process on a Sun Microsystems computer.
!        CODED:       F90 version coded 1 May 1993
!        AUTHOR:      John K. Prentice
!        AFFILIATION: Quetzal Computational Associates
!                     3200 Carlisle N.E.
!                     Albuquerque, NM   87110-1664
!
!        INPUT:    none
!
!        OUTPUT:
!
!        tnow      real      elapsed system time (seconds)
!
!
!        EXTERNAL ROUTINES USED: etime
!
!
    implicit none
!
    real :: tnow
    integer :: count, count_rate
!
    call system_clock (count, count_rate)
    if (count_rate == 0) then
        print *,"no system clock, abort."
        stop
    else 
        tnow = real(count)/real(count_rate)
    end if
!
end subroutine second

END MODULE TIMING_M
module energy

USE kind_definitions
USE timing_m

contains

      subroutine compute_energy (number_of_polypeptides, maximum_polypeptide_length,        &
&                                polypeptide_length, polypeptide_sequence,                  &
&                                polypeptide_position, individual_conformations,            &
&                                combined_conformations, surface_conformations,             &
&                                contact_conformations, number_of_surfaces,                 &
&                                surface_sequence, surface_position, surface_length,        &
&                                maximum_surface_j, new_conformation, lattice_imin,         &
&                                lattice_imax, lattice_width, save_internal_conformation,   &
&                                save_surface_conformation, save_contact_conformation,      &
&                                save_total_conformation, native_conformation,              &
&                                average_internal_energy, average_contact_energy,           &
&                                average_surface_energy, HH_internal_energy,                &
&                                HP_internal_energy, PP_internal_energy, HH_surface_energy, &
&                                HP_surface_energy, PP_surface_energy, number_of_bins,      &
&                                no_of_e_bins_per_amino_acid)
!
!        sum up the number of hydrophobic/hydrophobic pairwise interactions
!        on the lattice.  Exclude connected pairs and only count
!        topological pairs
!   
      implicit none
!
      integer, intent(in) :: number_of_polypeptides, maximum_polypeptide_length,            &
&                            surface_length, maximum_surface_j, lattice_imin, lattice_imax, &
&                            lattice_width, number_of_surfaces, number_of_bins,             &
&                            no_of_e_bins_per_amino_acid
      integer, dimension(number_of_polypeptides), intent(in) :: polypeptide_length
      integer, dimension(maximum_polypeptide_length, number_of_polypeptides), intent(in) :: &
&                                                                        polypeptide_sequence
      integer, dimension(2,maximum_polypeptide_length,number_of_polypeptides),              &
&                                                       intent(inout) :: polypeptide_position
      real (kind = LONGreal), dimension(-number_of_bins:0,number_of_polypeptides),          &
&                                                   intent(inout) :: individual_conformations
      real (kind = LONGreal), dimension(-number_of_bins:0,number_of_surfaces),              &
&                                                  intent(inout) :: combined_conformations, &
&                                                                   average_internal_energy,&
&                                                                   average_contact_energy, &
&                                                                   average_surface_energy
      real (kind = LONGreal),dimension(-number_of_bins:0,                                   &
&                             number_of_polypeptides,number_of_surfaces), intent(inout) ::  &
&                                                                       surface_conformations
      real (kind = LONGreal), dimension(-number_of_bins:0),                                &
&                                                      intent(inout) :: contact_conformations
      integer, dimension(surface_length,number_of_surfaces), intent(in) :: surface_sequence
      integer, dimension(2,surface_length), intent(in) :: surface_position
      logical, intent(inout) :: new_conformation, native_conformation
!
      integer :: l, m, n, n1, n2, i1, j1, i2, j2, distance, internal_energy, surface_energy,&
&                total_internal_energy, total_surface_energy, contact_energy, total_energy, &
&                delta, k, energy_bin,  save_internal_bin, save_surface_bin,                &
&                save_contact_bin, save_total_bin, internal_bin, surface_bin, contact_bin,  &
&                total_bin
      integer, dimension(surface_length) :: shifted_surface_sequence
      real (kind = LONGreal) :: HH_internal_energy, HP_internal_energy, PP_internal_energy, &
&                               HH_surface_energy, HP_surface_energy, PP_surface_energy,    &
&                               energy, energy_width, true_internal_energy,                 &
&                               true_total_internal_energy, true_surface_energy,            &
&                               true_total_surface_energy, true_contact_energy,             &
&                               save_internal_conformation, save_surface_conformation,      &
&                               save_contact_conformation, save_total_conformation,         &
&                               true_total_energy
!
      call second (final_conformation_time)
      total_conformation_time = total_conformation_time + final_conformation_time -  &
&                               initial_conformation_time
      call second (initial_energy_time)
!
      energy_width = 1.0_LONGreal/real(no_of_e_bins_per_amino_acid,LONGreal)
!
      save_internal_bin = int(abs(save_internal_conformation)/energy_width + 0.5_LONGreal)   
      save_surface_bin = int(abs(save_surface_conformation)/energy_width + 0.5_LONGreal)   
      save_contact_bin = int(abs(save_contact_conformation)/energy_width + 0.5_LONGreal)   
      save_total_bin = int(abs(save_total_conformation)/energy_width + 0.5_LONGreal)   
!
!       compute the total energy of the individual polypeptides due to topological
!       contacts between hydrophobic amino acids within each polypeptide
!
      total_internal_energy = 0
      true_total_internal_energy = 0.0_LONGreal
      do m = 1, number_of_polypeptides
          internal_energy = 0
          true_internal_energy = 0.0_LONGreal
          if (polypeptide_length(m) >= 4) then
              do n1 = 1, polypeptide_length(m)-3
                  i1 = polypeptide_position(1,n1,m)
                  j1 = polypeptide_position(2,n1,m)
                  do n2 = n1+3, polypeptide_length(m)
                      i2 = polypeptide_position(1,n2,m)
                      j2 = polypeptide_position(2,n2,m)
                      distance = abs(i1-i2) + abs(j1-j2)
                      if (distance == 1) then
                          if (polypeptide_sequence(n1,1) == 0) then
                              if (polypeptide_sequence(n2,1) == 1) then
                                  energy = abs(HP_internal_energy)
                              else
                                  energy = abs(PP_internal_energy)
                              end if
                          else if (polypeptide_sequence(n1,1) == 1) then
                              if (polypeptide_sequence(n2,1) == 0) then
                                  energy = abs(HP_internal_energy)
                              else
                                  energy = abs(HH_internal_energy)
                              end if
                          end if         
                          energy_bin = int(energy/energy_width + 0.5_LONGreal)   
                          internal_energy = internal_energy - energy_bin
                          true_internal_energy = true_internal_energy - energy
                      end if
                  end do 
              end do 
          end if
          if (new_conformation .AND. m == 1) then
              individual_conformations(internal_energy,:) =                                 &
&                                  individual_conformations(internal_energy,:) + 1.0_LONGreal
              if (save_internal_conformation <= 0.0_LONGreal) then
                internal_bin = int(abs(true_internal_energy)/energy_width + 0.5_LONGreal)
                if (internal_bin == save_internal_bin) then
                  write(51,'(20i4)') polypeptide_position(1,1:polypeptide_length(1),1)
                  write(51,'(20i4)') polypeptide_position(2,1:polypeptide_length(1),1)
                end if
              end if
          end if
          total_internal_energy = total_internal_energy + internal_energy
          true_total_internal_energy = true_total_internal_energy + true_internal_energy
      end do
!
!        compute the interaction energy between polypeptides
!
      contact_energy = 0
      true_contact_energy = 0.0_LONGreal
      if (number_of_polypeptides > 1) then
          do m = 1, number_of_polypeptides - 1
out3:         do n1 = 1, polypeptide_length(m)
                  i1 = polypeptide_position(1,n1,m)
                  j1 = polypeptide_position(2,n1,m)
                  do n = m+1, number_of_polypeptides
                      do n2 = 1, polypeptide_length(n)
                          i2 = polypeptide_position(1,n2,n)
                          j2 = polypeptide_position(2,n2,n)
                          distance = abs(i1-i2) + abs(j1-j2)
                          if (distance == 1) then
                              if (polypeptide_sequence(n1,m) == 0) then
                                  if (polypeptide_sequence(n2,n) == 1) then
                                      energy = abs(HP_internal_energy)
                                  else
                                      energy = abs(PP_internal_energy)
                                  end if
                              else if (polypeptide_sequence(n1,m) == 1) then
                                  if (polypeptide_sequence(n2,n) == 0) then
                                      energy = abs(HP_internal_energy)
                                  else
                                      energy = abs(HH_internal_energy)
                                  end if
                              end if         
                              energy_bin = int(energy/energy_width + 0.5_LONGreal)   
                              contact_energy = contact_energy - energy_bin
                              true_contact_energy = true_contact_energy - energy
                          end if
                      end do
                  end do
              end do out3
          end do
      end if
      contact_conformations(contact_energy) = contact_conformations(contact_energy)         &
&                                                                              + 1.0_LONGreal
!
      if (save_contact_conformation <= 0.0_LONGreal .AND. .NOT. native_conformation) then
        contact_bin = int(abs(true_contact_energy)/energy_width + 0.5_LONGreal)
        if (contact_bin == save_contact_bin) then
          do m = 1, number_of_polypeptides
              write(53,'(20i4)') polypeptide_position(1,1:polypeptide_length(1),m)
              write(53,'(20i4)') polypeptide_position(2,1:polypeptide_length(1),m)
          end do
        end if
      end if
!
!        finally, compute the energy of interaction between all the polypeptides and all
!        cyclic permutations of the surface.  From this and the previous energies, compute
!        the total energy for the conformations of these proteins with each candidate
!        surface
!
      if (.NOT. native_conformation) then
        do l = 1, number_of_surfaces
          do m = 1, surface_length
!
!        cyclically permute the surface sequence by 1 amino acid
!
              total_surface_energy = 0
              true_total_surface_energy = 0.0_LONGreal
              shifted_surface_sequence = cshift(surface_sequence(:,l),m-1)
              do k = 1, number_of_polypeptides
                  surface_energy = 0
                  true_surface_energy = 0.0_LONGreal
                  do n1 = 1, polypeptide_length(k)
                      if (polypeptide_position(2,n1,k) <= maximum_surface_j + 1) then
                          i1 = polypeptide_position(1,n1,k)
                          j1 = polypeptide_position(2,n1,k)
                          if (i1 < lattice_imin) then
                              delta = mod(lattice_imin-1-i1,lattice_width) + 1
                              i1 = lattice_imax - delta + 1
                          else if (i1 > lattice_imax) then
                              delta = mod(i1-lattice_imax-1,lattice_width) + 1
                              i1 = lattice_imin + delta - 1
                          end if
                          do n2 = 1, surface_length
                              i2 = surface_position(1,n2)
                              j2 = surface_position(2,n2)
                              distance = abs(i1-i2) + abs(j1-j2)
                              if (distance == 1) then
                                  if (polypeptide_sequence(n1,k) == 0) then
                                      if (shifted_surface_sequence(n2) == 1) then
                                          energy = abs(HP_surface_energy)
                                      else
                                          energy = abs(PP_surface_energy)
                                      end if
                                  else if (polypeptide_sequence(n1,k) == 1) then
                                      if (shifted_surface_sequence(n2) == 0) then
                                          energy = abs(HP_surface_energy)
                                      else
                                          energy = abs(HH_surface_energy)
                                      end if
                                  end if         
                                  energy_bin = int(energy/energy_width + 0.5_LONGreal) 
                                  surface_energy = surface_energy - energy_bin
                                  true_surface_energy = true_surface_energy - energy
                              end if  
                          end do 
                      end if
                  end do
                  if (new_conformation .AND. k == 1) then
                      surface_conformations(surface_energy,:,l) =                           &
&                                    surface_conformations(surface_energy,:,l) + 1.0_LONGreal
                      if (save_surface_conformation <= 0.0_LONGreal .AND. l == 1) then
                        surface_bin = int(abs(true_surface_energy)/energy_width +         &
&                                                                               0.5_LONGreal)
                        if (surface_bin == save_surface_bin) then
                        write(52,'(20i4)') polypeptide_position(1,1:polypeptide_length(1),1)&
&                                                                                       + m-1
                        write(52,'(20i4)') polypeptide_position(2,1:polypeptide_length(1),1)
                        end if
                      end if
                  end if
                  total_surface_energy = total_surface_energy + surface_energy
                  true_total_surface_energy = true_total_surface_energy + true_surface_energy
              end do
              total_energy = total_internal_energy + total_surface_energy + contact_energy
              true_total_energy = true_total_internal_energy + true_total_surface_energy +  &
&                                                                         true_contact_energy
              combined_conformations(total_energy,l) =                                      &
&                                       combined_conformations(total_energy,l) + 1.0_LONGreal
              average_internal_energy(total_energy,l) =                                     &
&                                average_internal_energy(total_energy,l) +                  &
&                                                                  true_total_internal_energy
              average_surface_energy(total_energy,l) =                                      &
&                                 average_surface_energy(total_energy,l) +                  &
&                                                                   true_total_surface_energy
              average_contact_energy(total_energy,l) =                                      &
&                                   average_contact_energy(total_energy,l) +                &
&                                                                         true_contact_energy
              if (save_total_conformation <= 0.0_LONGreal .AND. l == 1) then
                total_bin = int(abs(true_total_energy)/energy_width + 0.5_LONGreal)
                if (total_bin == save_total_bin) then
                  do k = 1, number_of_polypeptides
                    write(54,'(20i4)') polypeptide_position(1,1:polypeptide_length(1),k)+m-1
                    write(54,'(20i4)') polypeptide_position(2,1:polypeptide_length(1),k)
                  end do
                end if
              end if
          end do
        end do
      end if
!
      new_conformation = .FALSE.
!
      call second (final_energy_time)
      total_energy_time = total_energy_time + final_energy_time - initial_energy_time
      initial_conformation_time = final_energy_time
      end subroutine compute_energy

end module energy


MODULE conformations

USE kind_definitions
USE energy

CONTAINS

      recursive subroutine conformation (number_of_polypeptides, maximum_polypeptide_length,&
&                                        polypeptide_length, polypeptide, amino_acid,       &
&                                        lattice, lattice_delta, lattice_imin,              &
&                                        lattice_imax, lattice_jmin, lattice_jmax,          &
&                                        lattice_width, polypeptide_sequence,               &
&                                        polypeptide_position, individual_conformations,    &
&                                        combined_conformations, surface_conformations,     &
&                                        contact_conformations, number_of_surfaces,         &
&                                        surface_sequence, surface_position,                &
&                                        surface_length, surface_contact,                   &
&                                        maximum_surface_j, new_conformation,               &
&                                        save_internal_conformation,                        &
&                                        save_surface_conformation,                         &
&                                        save_contact_conformation,                         &
&                                        save_total_conformation, native_conformation,      &
&                                        average_internal_energy, average_contact_energy,   &
&                                        average_surface_energy, HH_internal_energy,        &
&                                        HP_internal_energy, PP_internal_energy,            &
&                                        HH_surface_energy, HP_surface_energy,              &
&                                        PP_surface_energy, number_of_bins,                 &
&                                        no_of_e_bins_per_amino_acid)
!
!        recursively compute all conformations for multiple polypeptides on a lattice
!        with periodic boundaries
!
      implicit none
!
      integer, intent(in) :: number_of_polypeptides, lattice_imin, lattice_imax,            &
&                            lattice_jmin, lattice_jmax, maximum_polypeptide_length,        &
&                            surface_length, lattice_width, maximum_surface_j,              &
&                            number_of_surfaces, no_of_e_bins_per_amino_acid, number_of_bins
      integer, intent(inout) :: polypeptide, amino_acid
      integer, dimension(2,4), intent(in) :: lattice_delta
      integer, dimension(number_of_polypeptides), intent(in) :: polypeptide_length
      real (kind = LONGreal), dimension(-number_of_bins:0,number_of_polypeptides),          &
&                                                   intent(inout) :: individual_conformations
      real (kind = LONGreal),                                                               &
&                        dimension(-number_of_bins:0,number_of_surfaces),                   &
&                                                  intent(inout) :: combined_conformations, &
&                                                                   average_internal_energy,&
&                                                                   average_contact_energy, &
&                                                                   average_surface_energy
      real (kind = LONGreal), dimension(-number_of_bins:0,                                  &
&                             number_of_polypeptides,number_of_surfaces), intent(inout) ::  &
&                                                                       surface_conformations
      real (kind = LONGreal),                                                               &
&                        dimension(-number_of_bins:0), intent(inout) :: contact_conformations
      integer, dimension(maximum_polypeptide_length, number_of_polypeptides), intent(in) :: &
&                                                                        polypeptide_sequence
      integer, dimension(2,maximum_polypeptide_length,number_of_polypeptides),              &
&                                                       intent(inout) :: polypeptide_position
      integer, dimension(2,lattice_imin:lattice_imax,lattice_jmin:lattice_jmax),            &
&                                                                    intent(inout) :: lattice
      integer, dimension(surface_length,number_of_surfaces), intent(in) :: surface_sequence
      integer, dimension(2,surface_length), intent(in) :: surface_position
      logical, intent(inout) :: new_conformation, native_conformation
      logical, dimension(maximum_polypeptide_length,number_of_polypeptides),                &
&                                                            intent(inout) :: surface_contact
      real (kind = LONGreal), intent(in) :: HH_internal_energy, HP_internal_energy,         &
&                                           PP_internal_energy, HH_surface_energy,          &
&                                           HP_surface_energy, PP_surface_energy,           &
&                                           save_internal_conformation,                     &
&                                           save_surface_conformation,                      &
&                                           save_contact_conformation,                      &
&                                           save_total_conformation
!
      integer :: new, iold, jold, inew, jnew, i_periodic, delta, minimum_i, maximum_i,      &
&                minimum_j, maximum_j, old_i_lattice_origin, i, j, n, i_origin, j_origin,   &
&                i_polypeptide_origin, i1, j1, i_new, j_new
      logical :: contacted
!
!        on the first amino acid for any polypeptide, initialize the surface contact flag
!
      if (amino_acid == 1 .AND. .NOT. native_conformation) then
          surface_contact(1,polypeptide) = .FALSE.
          if (polypeptide_position (2,1,polypeptide) - 1 <= maximum_surface_j) then
contact1:   do i = polypeptide_position(1,1,polypeptide)-1,                                 &
&                                                     polypeptide_position(1,1,polypeptide)+1
              do j = polypeptide_position(2,1,polypeptide)-1,                               &
&                                                     polypeptide_position(2,1,polypeptide)+1
                if (i < lattice_imin) then
                    delta = mod(lattice_imin-1-i,lattice_width) + 1
                    i_periodic = lattice_imax - delta + 1
                else if (i > lattice_imax) then
                    delta = mod(i-lattice_imax-1,lattice_width) + 1
                    i_periodic = lattice_imin + delta - 1
                else
                    i_periodic = i
                end if
                if (j >= lattice_jmin .AND. j <= lattice_jmax) then
                    if (lattice(1,i_periodic,j) /= 0 .AND. lattice(2,i_periodic,j) == 0) then
                        surface_contact(1,polypeptide) = .TRUE.
                        exit contact1
                    end if
                end if
              end do
            end do contact1
          end if
      end if                  
!
!        increment amino acid residue counter for this polypeptide
!
      amino_acid = amino_acid + 1
!
!        loop over the possible positions of the next amino acid residue on this polypeptide
!
move: do new = 1, 4
!
!        if this is the terminal end of the 1st polypeptide, flip on the flag indicating
!        that this is a new conformation for that polypeptide
!
          if (polypeptide == 1 .AND. amino_acid == polypeptide_length(1))                   &
&                                                                   new_conformation = .TRUE.
!
!        determine location of next amino acid residue
!
          iold = polypeptide_position (1,amino_acid-1,polypeptide)
          jold = polypeptide_position (2,amino_acid-1,polypeptide)
          inew = iold + lattice_delta(1,new)
          jnew = jold + lattice_delta(2,new)
          if (jnew < lattice_jmin .OR. jnew > lattice_jmax) cycle move
!
!        if necessary, recompute the i coordinate of the amino acid to account for the
!        periodic boundary conditions
!
          if (inew < lattice_imin) then
              delta = mod(lattice_imin-1-inew,lattice_width) + 1
              i_periodic = lattice_imax - delta + 1
          else if (inew > lattice_imax) then
              delta = mod(inew-lattice_imax-1,lattice_width) + 1
              i_periodic = lattice_imin + delta - 1
          else
              i_periodic = inew
          end if
!
!        if the location of the next amino acid residue is unoccupied,
!        proceed on to the next one.  
!
          if (lattice(1,i_periodic,jnew) /= 0) cycle move 
!
          polypeptide_position(1,amino_acid,polypeptide) = inew
          polypeptide_position(2,amino_acid,polypeptide) = jnew
          lattice(1,i_periodic,jnew) = amino_acid
          lattice(2,i_periodic,jnew) = polypeptide
!
!        determine whether this amino acid is in contact with the surface
!
          if (.NOT. native_conformation) then
            surface_contact(amino_acid,polypeptide) = .FALSE.
            if (polypeptide_position (2,amino_acid,polypeptide) - 1 <= maximum_surface_j)then
              n = amino_acid
contact2:     do i = polypeptide_position(1,n,polypeptide)-1,                               &
&                                                     polypeptide_position(1,n,polypeptide)+1
                do j = polypeptide_position(2,n,polypeptide)-1,                             &
&                                                     polypeptide_position(2,n,polypeptide)+1
                  if (i < lattice_imin) then
                      delta = mod(lattice_imin-1-i,lattice_width) + 1
                      i_new = lattice_imax - delta + 1
                  else if (i > lattice_imax) then
                      delta = mod(i-lattice_imax-1,lattice_width) + 1
                      i_new = lattice_imin + delta - 1
                  else
                      i_new = i
                  end if
                  if (j >= lattice_jmin .AND. j <= lattice_jmax) then
                      if (lattice(1,i_new,j) /= 0 .AND. lattice(2,i_new,j) == 0) then
                          surface_contact(amino_acid,polypeptide) = .TRUE.
                          exit contact2
                      end if
                   end if
                end do
              end do contact2
            end if
          end if
!
!        At any point where we determine that the polypeptide cannot contact the surface, 
!        abort this conformation
!
          if (amino_acid == polypeptide_length(polypeptide) .AND.                           &
&                                                             .NOT. native_conformation) then
              contacted = .FALSE.
contact3:     do n = 1, polypeptide_length(polypeptide)
                  if (surface_contact(n,polypeptide)) then
                      contacted = .TRUE.
                      exit contact3
                  end if
              end do contact3
              if (.NOT. contacted) then
                  lattice(1,i_periodic,jnew) = 0
                  lattice(2,i_periodic,jnew) = 0
                  cycle move
              end if
          end if
!
!        If we are at the end of this polypeptide, start enumerating the conformations of 
!        the next polypeptide. If this is the end of the last polypeptide, return
!
          if (amino_acid == polypeptide_length(polypeptide)) then      
!
!        this is the terminal end of this polypeptide.  If this is not the last
!        polypeptide, start enumerating the conformations for the next one.  If
!        it is the last polypeptide, compute the density of states
!
              if (polypeptide == number_of_polypeptides) then
                  call compute_energy (number_of_polypeptides, maximum_polypeptide_length,  &
&                                      polypeptide_length, polypeptide_sequence,            &
&                                      polypeptide_position, individual_conformations,      &
&                                      combined_conformations, surface_conformations,       &
&                                      contact_conformations, number_of_surfaces,           &
&                                      surface_sequence, surface_position, surface_length,  &
&                                      maximum_surface_j, new_conformation, lattice_imin,   &
&                                      lattice_imax, lattice_width,                         &
&                                      save_internal_conformation,                          &
&                                      save_surface_conformation,                           &
&                                      save_contact_conformation, save_total_conformation,  &
&                                      native_conformation, average_internal_energy,        &
&                                      average_contact_energy, average_surface_energy,      &
&                                      HH_internal_energy, HP_internal_energy,              &
&                                      PP_internal_energy, HH_surface_energy,               &
&                                      HP_surface_energy, PP_surface_energy,                &
&                                      number_of_bins, no_of_e_bins_per_amino_acid)
              else 
                  polypeptide = polypeptide + 1
!
!        compute the i coordinate interval for the origin of this polypeptide
!
                  old_i_lattice_origin = polypeptide_position(1,1,polypeptide-1)
                  minimum_i = old_i_lattice_origin + 1
                  maximum_i = minimum_i + lattice_width - 1
                  do i_polypeptide_origin = minimum_i, maximum_i  
                    if (i_polypeptide_origin <= lattice_imax) then
                      if (i_polypeptide_origin < lattice_imin) then
                          delta = mod(lattice_imin-1-i_polypeptide_origin,lattice_width) + 1
                          i_origin = lattice_imax - delta + 1
                      else if (i_polypeptide_origin > lattice_imax) then
                          delta = mod(i_polypeptide_origin-lattice_imax-1,lattice_width) + 1
                          i_origin = lattice_imin + delta - 1
                      else
                          i_origin = i_polypeptide_origin
                      end if
surface_j:            do j_origin = lattice_jmax, lattice_jmin, -1
                          if (lattice(1,i_origin,j_origin) /= 0 .AND.                       &
&                                                     lattice(2,i_origin,j_origin) == 0) then
                              minimum_j = j_origin + 1
                              exit surface_j
                          end if
                      end do surface_j
                      minimum_j = max(minimum_j, lattice_jmin)
                      maximum_j = min(lattice_jmax, polypeptide_length(polypeptide))
                      do j_origin = minimum_j, maximum_j
                          i1 = i_origin
                          j1 = j_origin
                          if (lattice(1,i1,j1) == 0) then
                              lattice(1,i1,j1) = 1
                              lattice(2,i1,j1) = polypeptide
                              polypeptide_position(1,1,polypeptide) = i_polypeptide_origin
                              polypeptide_position(2,1,polypeptide) = j1
                              amino_acid = 1
                              surface_contact(amino_acid,polypeptide) = .FALSE.
                              if (polypeptide_position (2,amino_acid,polypeptide) - 1       &
&                                                                  <= maximum_surface_j) then
                                  n = amino_acid
contact4:                         do i = polypeptide_position(1,n,polypeptide)-1,           &
&                                                     polypeptide_position(1,n,polypeptide)+1
                                      do j = polypeptide_position(2,n,polypeptide)-1,       &
&                                                     polypeptide_position(2,n,polypeptide)+1
                                          if (i < lattice_imin) then
                                            delta = mod(lattice_imin-1-i,lattice_width) + 1
                                            i_new = lattice_imax - delta + 1
                                          else if (i > lattice_imax) then
                                            delta = mod(i-lattice_imax-1,lattice_width) + 1
                                            i_new = lattice_imin + delta - 1
                                          else
                                            i_new = i
                                          end if
                                          if (j >= lattice_jmin .AND. j <= lattice_jmax) then
                                            if (lattice(1,i_new,j) /= 0 .AND.               &
&                                                               lattice(2,i_new,j) == 0) then
                                              surface_contact(amino_acid,polypeptide)=.TRUE.
                                              exit contact4
                                            end if
                                          end if
                                      end do
                                  end do contact4
                              end if
                              call conformation (number_of_polypeptides,                    &
&                                                maximum_polypeptide_length,                &
&                                                polypeptide_length, polypeptide,           &
&                                                amino_acid, lattice, lattice_delta,        &
&                                                lattice_imin, lattice_imax, lattice_jmin,  &
&                                                lattice_jmax, lattice_width,               &
&                                                polypeptide_sequence,                      &
&                                                polypeptide_position,                      &
&                                                individual_conformations,                  &
&                                                combined_conformations,                    &
&                                                surface_conformations,                     &
&                                                contact_conformations, number_of_surfaces, &
&                                                surface_sequence, surface_position,        &
&                                                surface_length, surface_contact,           &
&                                                maximum_surface_j, new_conformation,       &
&                                                save_internal_conformation,                &
&                                                save_surface_conformation,                 &
&                                                save_contact_conformation,                 &
&                                                save_total_conformation,                   &
&                                                native_conformation,                       &
&                                                average_internal_energy,                   &
&                                                average_contact_energy,                    &
&                                                average_surface_energy, HH_internal_energy,&
&                                                HP_internal_energy, PP_internal_energy,    &
&                                                HH_surface_energy, HP_surface_energy,      &
&                                                PP_surface_energy, number_of_bins,         &
&                                                no_of_e_bins_per_amino_acid)
                              lattice(1,i1,j1) = 0
                              lattice(2,i1,j1) = 0
                          end if
                      end do
                    end if
                  end do
                  polypeptide = polypeptide - 1
                  amino_acid = polypeptide_length(polypeptide)
              end if
          else
              call conformation (number_of_polypeptides, maximum_polypeptide_length,        &
&                                polypeptide_length, polypeptide, amino_acid, lattice,      &
&                                lattice_delta, lattice_imin, lattice_imax, lattice_jmin,   &
&                                lattice_jmax, lattice_width, polypeptide_sequence,         &
&                                polypeptide_position, individual_conformations,            &
&                                combined_conformations, surface_conformations,             &
&                                contact_conformations, number_of_surfaces,                 &
&                                surface_sequence, surface_position, surface_length,        &
&                                surface_contact, maximum_surface_j, new_conformation,      &
&                                save_internal_conformation, save_surface_conformation,     &
&                                save_contact_conformation, save_total_conformation,        &
&                                native_conformation, average_internal_energy,              &
&                                average_contact_energy, average_surface_energy,            &
&                                HH_internal_energy, HP_internal_energy,                    &
&                                PP_internal_energy, HH_surface_energy, HP_surface_energy,  &
&                                PP_surface_energy, number_of_bins,                         &
&                                no_of_e_bins_per_amino_acid)
          end if
          lattice(1,i_periodic,jnew) = 0
          lattice(2,i_periodic,jnew) = 0
      end do move     
! 
      amino_acid = amino_acid - 1    
      if (amino_acid < 1) then
          polypeptide = polypeptide - 1
          amino_acid = polypeptide_length(polypeptide)
      end if
!
      end subroutine conformation


END MODULE conformations
      module free_format_parser_variables
      character   card*80, field*80, lfield*80
      integer   icpnt
      logical   eoff
      integer   nin, nout
      end module free_format_parser_variables
MODULE NEXT_M

USE free_format_parser_variables

CONTAINS

     subroutine next
!
      implicit none
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
      integer, parameter :: icend = 80
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i, j, k, iend, istart, kmin, nodel
      logical :: first
      character, dimension(3) :: delim

      save first
!-----------------------------------------------
      data first/.TRUE./
      data nodel, delim/3, ' ', ',', '='/
!
!        on first pass, initialize icpnt pointer
!
      if (first) then
          icpnt = icend + 1
          first = .FALSE.
      endif
!
!        if icpnt>icend, read the next record off unit 2 into
!        the string 'card'.  next verify that this is a non-
!        blank card.  if it is blank or an input comment card
!        (asterisk in column 1), skip it and get the next record
!
   10 continue
      eoff = .FALSE.
      if (icpnt > icend) then
          read (nin, '(a)', end=50) card
          if (card(1:icend)==' ' .or. card(1:1)=='*') go to 10
          icpnt = 1
      endif
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
                  endif
              endif
          end do
!
!        if kmin is not equal to the current pointer position, then
!        it must be pointing at the trailing delimeter of a valid
!        sub-string.
!
          if (kmin /= i) then
              if (kmin > 0) then
                  iend = kmin - 1
                  go to 40
              endif
!
!        if we fall through, there was no delimeter
!        found on the remainder of this record.  this means
!        the entire remainder of this record is a valid sub-string
!
              iend = icend
              go to 40
          endif
      end do
!
!        if we fall through this loop, there were no more non-
!        delimeters on this record.  go get next record
!
      icpnt = icend + 1
      go to 10
!
!        put the sub-string into the string 'field'.  note
!        that fortran 77 pads the string with blanks
!
   40 continue
      field = card(istart:iend)
      icpnt = iend + 2
      return 
!
!        end of file encountered, set flag and return
!
   50 continue
      eoff = .TRUE.
      icpnt = icend + 1
      return 
      end subroutine next

END MODULE NEXT_M
MODULE VALUE_M

USE free_format_parser_variables
USE kind_definitions
USE NEXT_M

CONTAINS

      subroutine value(result, itype)
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
!
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer itype
      real (kind = LONGreal) :: result
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
!-----------------------------------------------
!
!        get next field off unit 2
!
      call next
      if (eoff) then
          itype = -1
      else
!
!        read field as a numeric
!
          read (field, fmt='(bn,f20.0)', err=10) result
          itype = 1
          go to 20
!
!        the only possibility left is that this was an alphanumeric
!
   10     continue
          itype = 0
   20     continue
      endif
!
      end subroutine value

END MODULE VALUE_M
MODULE LEER_ENTRADA_M_1

USE kind_definitions
USE free_format_parser_variables
USE next_m
USE value_m

CONTAINS

      subroutine leer_entrada_1 (endrada_unidad, rendimiento_unidad,                        &
&                                  number_of_polypeptides, maximum_polypeptide_length,      &
&                                                         surface_length, number_of_surfaces)
!
!        read in problem input from the file "protein.in"
!
      implicit none
!
      integer :: endrada_unidad, rendimiento_unidad, number_of_polypeptides,                &
&                maximum_polypeptide_length, minimum_polypeptide_length, polypeptide_length,&
&                surface_length, number_of_surfaces
!
      real (kind = LONGreal) :: real_variable
      integer :: free_format_error_flag, m, n, integer_value, lattice_imin, lattice_imax
!
      nin = endrada_unidad
      nout = rendimiento_unidad
      maximum_polypeptide_length = -9999
      minimum_polypeptide_length = 9999
      number_of_surfaces = -1
!
      open(unit=nin,file='protein.in',status= 'old',form='formatted')
      rewind nin
      icpnt = 9999
!
!        define defaults
!
      number_of_polypeptides = 0
!
!        read input
!
in:   do
          call next
          if (len(field) > 0) then
              do n = 1, len(field)
                  field(n:n) = convert_lower_case(field(n:n))
              end do
          end if
!
          if (eoff) then
              close (unit = nin)
              exit in
!
!############################################################################################
!      protein_definition
!############################################################################################
!
          else if (field == 'polypeptide_definition') then
              call next
              if (eoff) then
                  write (nout,'(/''Unexpected end of file reading protein definition, '',   &
&                                                                             ''abort.''//)')
                  stop
              else if (len(field) > 0) then
                  do n = 1, len(field)
                      field(n:n) = convert_lower_case(field(n:n))
                  end do
              end if            
!
!############################################################################################
!      number_of_polypeptides
!############################################################################################
!
              if (field /= 'number_of_polypeptides') then
                  write (nout,'(/''First field in protein_defintion is not the number '',   &
&                                                                ''of proteins, abort.''//)')
                  stop
              end if
              call value (real_variable, free_format_error_flag)
              call check_eof
              call check_number
              number_of_polypeptides = int(real_variable + 0.5_LONGreal)
              if (number_of_polypeptides <= 0) then
                  write (nout,'(/''The number of polypeptides is less than or equal to '',  &
&                                                                       ''zero, abort.''//)')
                  stop
              end if
!
!############################################################################################
!      polypeptide_length
!############################################################################################
!
              call next
              if (eoff) then
                  write (nout,'(/''Unexpected end of file reading protein definition,'',    &
&                                                                            '' abort.''//)')
                  stop
              else if (len(field) > 0) then
                  do m = 1, len(field)
                      field(m:m) = convert_lower_case(field(m:m))
                  end do
              end if            
              if (field /= 'polypeptide_length') then
                  write (nout,'(/''Second field in protein_defintion is not the '',         &
&                                                         ''polypeptide length, abort.''//)')
                  stop
              end if
              call value (real_variable, free_format_error_flag)
              call check_eof
              call check_number
              integer_value = int(real_variable+0.5_LONGreal)
              if (integer_value < 0) then
                  write (nout,'(/''The polypeptide_length is zero or less, abort.''//)')
                  stop
              end if
              maximum_polypeptide_length = integer_value
!
!############################################################################################
!      lattice_imin
!############################################################################################
!
          else if (field == 'lattice_imin') then
              call value (real_variable, free_format_error_flag)
              call check_eof
              call check_number
              if (real_variable >= 0.0_LONGreal) then
                  lattice_imin = int(real_variable + 0.5_LONGreal)
              else
                  lattice_imin = int(real_variable - 0.5_LONGreal)
              end if
!
!############################################################################################
!      lattice_imax
!############################################################################################
!
          else if (field == 'lattice_imax') then
              call value (real_variable, free_format_error_flag)
              call check_eof
              call check_number
              if (real_variable >= 0.0_LONGreal) then
                  lattice_imax = int(real_variable + 0.5_LONGreal)
              else
                  lattice_imax = int(real_variable - 0.5_LONGreal)
              end if
!
!############################################################################################
!      surface_definition
!############################################################################################
!
          else if (field == 'surface_definition') then
              call next
              if (eoff) then
                  write (nout,'(/''Unexpected end of file reading protein definition, '',   &
&                                                                             ''abort.''//)')
                  stop
              else if (len(field) > 0) then
                  do n = 1, len(field)
                      field(n:n) = convert_lower_case(field(n:n))
                  end do
              end if            
!
!############################################################################################
!      number_of_surfaces
!############################################################################################
!
              if (field /= 'number_of_surfaces') then
                  write (nout,'(/''First field in surface_defintion is not the number '',   &
&                                                                ''of sufaces, abort.''//)')
                  stop 
              end if
              call value (real_variable, free_format_error_flag)
              call check_eof
              call check_number
              number_of_surfaces = int(real_variable + 0.5_LONGreal)
              if (number_of_surfaces <= 0) then
                  write (nout,'(/''The number of surfaces is less than or equal to zero, '',&
&                                                                             ''abort.''//)')
                  stop
              end if
          end if
      end do in
!
      surface_length = lattice_imax - lattice_imin + 1
!
!-----------------------------------------------------
!       INTERNAL SUBROUTINE
!-----------------------------------------------------
!
      contains
          subroutine check_eof
!
          if (free_format_error_flag == (-1)) then
              write (nout,'('' unexpected eof while reading link input, abort'')')
              write (nout,'(/'' was reading the input line: ''/1x,a80)') card
              stop
          endif
!
          end subroutine check_eof
!
          subroutine check_number
!
          if (free_format_error_flag == 0) then
              write (nout,'('' expected a number on input and instead '',                   &
&                    ''encountered the word: ''/                     1x,a80/'' abort''/)')  &
&                    field
              write (nout,'(/'' was reading the input line: ''/1x,a80)') card
              stop
          endif
!
          end subroutine check_number
!
          function convert_lower_case (input_string) result (output_string)
!
          character (len=1) :: input_string, output_string
          integer :: collating_difference
!
          if (ichar(input_string) >= ichar('A') .AND. ichar(input_string) <= ichar('Z')) then
              collating_difference = ichar(input_string) - ichar('A')
              output_string = char(ichar('a') + collating_difference)
          else
              output_string = input_string
          end if
!
          end function convert_lower_case

     end subroutine leer_entrada_1

END MODULE LEER_ENTRADA_M_1

MODULE LEER_ENTRADA_M_2

USE kind_definitions
USE free_format_parser_variables
USE next_m
USE value_m

CONTAINS

      subroutine leer_entrada_2 (endrada_unidad, rendimiento_unidad,                        &
&                                number_of_polypeptides, maximum_polypeptide_length,        &
&                                polypeptide_length, polypeptide_sequence, lattice_imin,    &
&                                lattice_imax, lattice_jmin, lattice_jmax, surface_length,  &
&                                number_of_surfaces, surface_sequence,                      &
&                                save_internal_conformation, save_surface_conformation,     &
&                                save_contact_conformation, save_total_conformation,        &
&                                native_conformation, HH_internal_energy,                   &
&                                HP_internal_energy, PP_internal_energy, HH_surface_energy, &
&                                HP_surface_energy, PP_surface_energy, temperature,         &
&                                energy_unit, no_of_e_bins_per_amino_acid)
!
!        read in problem input from the file "protein.in"
!
      implicit none
!
      integer :: endrada_unidad, rendimiento_unidad, number_of_polypeptides, lattice_imin,  &
&                lattice_imax, lattice_jmin, lattice_jmax, maximum_polypeptide_length,      &
&                surface_length, number_of_surfaces, no_of_e_bins_per_amino_acid
      integer, dimension(number_of_polypeptides) :: polypeptide_length
      integer, dimension(maximum_polypeptide_length, number_of_polypeptides) ::             &
&                                                                       polypeptide_sequence
      integer, dimension(surface_length,number_of_surfaces) :: surface_sequence
      logical :: native_conformation
      real (kind = LONGreal) :: HH_internal_energy, HP_internal_energy, PP_internal_energy, &
&                               HH_surface_energy, HP_surface_energy, PP_surface_energy,    &
&                               temperature, energy_unit, save_internal_conformation,       &
&                               save_surface_conformation, save_contact_conformation,       &
&                               save_total_conformation
!
      real (kind = LONGreal) :: real_variable
      integer :: free_format_error_flag, k, m, n
!
      nin = endrada_unidad
      nout = rendimiento_unidad
!
      open(unit=nin,file='protein.in',status= 'old',form='formatted')
      rewind nin
      icpnt = 9999
!
!        define defaults
!
      save_internal_conformation = 1.0_LONGreal
      save_surface_conformation = 1.0_LONGreal
      save_contact_conformation = 1.0_LONGreal
      save_total_conformation = 1.0_LONGreal
      lattice_imin = 9999
      lattice_imax = -9999
      lattice_jmin = 9999
      lattice_jmax = -9999
      native_conformation = .FALSE.
      HH_internal_energy = -2.3_LONGreal
      HP_internal_energy = -1.0_LONGreal
      PP_internal_energy = 0.0_LONGreal
      HH_surface_energy = -2.3_LONGreal
      HP_surface_energy = -1.0_LONGreal
      PP_surface_energy = 0.0_LONGreal
      energy_unit = 1.0_LONGreal
      no_of_e_bins_per_amino_acid = 10
      temperature = 273.0_LONGreal
!
!        read input
!
in:   do
          call next
          if (len(field) > 0) then
              do n = 1, len(field)
                  field(n:n) = convert_lower_case(field(n:n))
              end do
          end if
!
          if (eoff) then
              close (unit = nin)
              exit in
!
!############################################################################################
!      polypeptide_definition
!############################################################################################
!
          else if (field == 'polypeptide_definition') then
              call next
              if (eoff) then
                  write (nout,'(/''Unexpected end of file reading protein definition, '',   &
&                                                                             ''abort.''//)')
                  stop
              else if (len(field) > 0) then
                  do n = 1, len(field)
                      field(n:n) = convert_lower_case(field(n:n))
                  end do
              end if            
!
!      number_of_polypeptides
!
              if (field /= 'number_of_polypeptides') then
                  write (nout,'(/''First field in protein_defintion is not the number '',   &
&                                                                ''of proteins, abort.''//)')
                  stop
              end if
              call value (real_variable, free_format_error_flag)
              call check_eof
              call check_number
!
!      polypeptide_length
!
              call next
              if (eoff) then
                  write (nout,'(/''Unexpected end of file reading protein definition,'',    &
&                                                                            '' abort.''//)')
                  stop
              else if (len(field) > 0) then
                  do m = 1, len(field)
                      field(m:m) = convert_lower_case(field(m:m))
                  end do
              end if            
              if (field /= 'polypeptide_length') then
                  write (nout,'(/''Second field in protein_defintion is not the '',         &
&                                                         ''polypeptide length, abort.''//)')
                  stop
              end if
              call value (real_variable, free_format_error_flag)
              call check_eof
              call check_number
              polypeptide_length = int(real_variable + 0.5_LONGreal)
!
!############################################################################################
!      polypeptide_sequence
!############################################################################################
!
              call next
              if (eoff) then
                  write (nout,'(/''Unexpected end of file reading protein definition,'',    &
&                                                                            '' abort.''//)')
                  stop
              end if
              if (field /= 'polypeptide_sequence') then
                  write (nout,'(/''Second field in protein_defintion is not the '',         &
&                                                       ''polypeptide sequence, abort.''//)')
                  stop
              end if
              do m = 1, polypeptide_length(1)
                  call value (real_variable, free_format_error_flag)
                  call check_eof
                  call check_number
                  polypeptide_sequence(m,1:number_of_polypeptides) = int(real_variable +    &
&                                                                               0.5_LONGreal)
              end do
!
!############################################################################################
!      lattice_imin
!############################################################################################
!
          else if (field == 'lattice_imin') then
              call value (real_variable, free_format_error_flag)
              call check_eof
              call check_number
              if (real_variable >= 0.0_LONGreal) then
                  lattice_imin = int(real_variable + 0.5_LONGreal)
              else
                  lattice_imin = int(real_variable - 0.5_LONGreal)
              end if
!
!############################################################################################
!      lattice_imax
!############################################################################################
!
          else if (field == 'lattice_imax') then
              call value (real_variable, free_format_error_flag)
              call check_eof
              call check_number
              if (real_variable >= 0.0_LONGreal) then
                  lattice_imax = int(real_variable + 0.5_LONGreal)
              else
                  lattice_imax = int(real_variable - 0.5_LONGreal)
              end if
!
!############################################################################################
!      lattice_jmin
!############################################################################################
!
          else if (field == 'lattice_jmin') then
              call value (real_variable, free_format_error_flag)
              call check_eof
              call check_number
              if (real_variable >= 0.0_LONGreal) then
                  lattice_jmin = int(real_variable + 0.5_LONGreal)
              else
                  lattice_jmin = int(real_variable - 0.5_LONGreal)
              end if
!
!############################################################################################
!      lattice_jmax
!############################################################################################
!
          else if (field == 'lattice_jmax') then
              call value (real_variable, free_format_error_flag)
              call check_eof
              call check_number
              if (real_variable >= 0.0_LONGreal) then
                  lattice_jmax = int(real_variable + 0.5_LONGreal)
              else
                  lattice_jmax = int(real_variable - 0.5_LONGreal)
              end if
!
!############################################################################################
!      number_of_energy_bins_per_polypeptide_amino_acid
!############################################################################################
!
          else if (field == 'number_of_energy_bins_per_polypeptide_amino_acid') then
              call value (real_variable, free_format_error_flag)
              call check_eof
              call check_number
              no_of_e_bins_per_amino_acid = int(real_variable+0.5_LONGreal)
!
!############################################################################################
!      internal_hydrophobic/hydrophobic_interaction_energy
!############################################################################################
!
          else if (field == 'internal_hydrophobic/hydrophobic_interaction_energy') then
              call value (HH_internal_energy, free_format_error_flag)
              call check_eof
              call check_number
!
!############################################################################################
!      internal_hydrophobic/hydrophilic_interaction_energy
!############################################################################################
!
          else if (field == 'internal_hydrophobic/hydrophilic_interaction_energy') then
              call value (HP_internal_energy, free_format_error_flag)
              call check_eof
              call check_number
!
!############################################################################################
!      internal_hydrophilic/hydrophilic_interaction_energy
!############################################################################################
!
          else if (field == 'internal_hydrophilic/hydrophilic_interaction_energy') then
              call value (PP_internal_energy, free_format_error_flag)
              call check_eof
              call check_number
!
!############################################################################################
!      surface_hydrophobic/hydrophobic_interaction_energy
!############################################################################################
!
          else if (field == 'surface_hydrophobic/hydrophobic_interaction_energy') then
              call value (HH_surface_energy, free_format_error_flag)
              call check_eof
              call check_number
!
!############################################################################################
!      surface_hydrophobic/hydrophilic_interaction_energy
!############################################################################################
!
          else if (field == 'surface_hydrophobic/hydrophilic_interaction_energy') then
              call value (HP_surface_energy, free_format_error_flag)
              call check_eof
              call check_number
!
!############################################################################################
!      surface_hydrophilic/hydrophilic_interaction_energy
!############################################################################################
!
          else if (field == 'surface_hydrophilic/hydrophilic_interaction_energy') then
              call value (PP_surface_energy, free_format_error_flag)
              call check_eof
              call check_number
!
!############################################################################################
!      energy_unit
!############################################################################################
!
          else if (field == 'energy_unit') then
              call value (energy_unit, free_format_error_flag)
              call check_eof
              call check_number
!
!############################################################################################
!      temperature
!############################################################################################
!
          else if (field == 'temperature') then
              call value (temperature, free_format_error_flag)
              call check_eof
              call check_number
!
!############################################################################################
!      do_free_conformation
!############################################################################################
!
          else if (field == 'do_free_conformation') then
              native_conformation = .TRUE.
!
!############################################################################################
!      save_total_conformation_for_energy
!############################################################################################
!
          else if (field == 'save_total_conformation_for_energy') then
              call value (real_variable, free_format_error_flag)
              call check_eof
              call check_number
              save_total_conformation  = real_variable
!
!############################################################################################
!      save_internal_conformation_for_energy
!############################################################################################
!
          else if (field == 'save_internal_conformation_for_energy') then
              call value (real_variable, free_format_error_flag)
              call check_eof
              call check_number
              save_internal_conformation  = real_variable
!
!############################################################################################
!      save_surface_conformation_for_energy
!############################################################################################
!
          else if (field == 'save_surface_conformation_for_energy') then
              call value (real_variable, free_format_error_flag)
              call check_eof
              call check_number
              save_surface_conformation  = real_variable
!
!############################################################################################
!      save_contact_conformation_for_energy
!############################################################################################
!
          else if (field == 'save_contact_conformation_for_energy') then
              call value (real_variable, free_format_error_flag)
              call check_eof
              call check_number
              save_contact_conformation  = real_variable
!
!############################################################################################
!      surface_definition
!############################################################################################
!
          else if (field == 'surface_definition') then
              call next
              if (eoff) then
                  write (nout,'(/''Unexpected end of file reading surface definition, '',   &
&                                                                             ''abort.''//)')
                  stop
              else if (len(field) > 0) then
                  do n = 1, len(field)
                      field(n:n) = convert_lower_case(field(n:n))
                  end do
              end if            
!
!      number_of_surfaces
!
              if (field /= 'number_of_surfaces') then
                  write (nout,'(/''First field in surface_defintion is not the number '',   &
&                                                                ''of surfaces, abort.''//)')
                  stop
              end if
              call value (real_variable, free_format_error_flag)
              call check_eof
              call check_number
!
!      surface_sequence
!
              do k = 1, number_of_surfaces
                  call next
                  if (eoff) then
                      write (nout,'(/''Unexpected end of file reading surface definition,'',&
&                                                                            '' abort.''//)')
                      stop
                  else if (len(field) > 0) then
                      do m = 1, len(field)
                          field(m:m) = convert_lower_case(field(m:m))
                      end do
                  end if            
                  if (field /= 'surface_sequence') then
                      write (nout,'(/''Lost looking for surface sequence, abort.''//)')
                      stop
                  end if
                  do m = 1, surface_length
                      call value (real_variable, free_format_error_flag)
                      call check_eof
                      call check_number
                      surface_sequence(m,k) = int(real_variable + 0.5_LONGreal)
                  end do
              end do
!
!############################################################################################
!      unknown string
!############################################################################################
!
          else
              write (nout,'(/''Unrecognized input string, abort.''/''The input string was:''&
&                                /a//)') field
              stop
          end if
      end do in
!
!        do some simple checks on the input
!
      if (lattice_imin >= lattice_imax) then
          write (nout,'(/''The lattice i dimensions are bad, abort.''/)')
          write (nout,'(''imin, imax, jmin, jmax = '',4i5/)') lattice_imin, lattice_imax,   &
&                                                                  lattice_jmin, lattice_jmax
          stop
      else if (lattice_jmin >= lattice_jmax) then
          write (nout,'(/''The lattice j dimensions are bad, abort.''/)')
          write (nout,'(''imin, imax, jmin, jmax = '',4i5/)') lattice_imin, lattice_imax,   &
&                                                                  lattice_jmin, lattice_jmax
          stop
      else if (native_conformation) then
          if (number_of_polypeptides /= 1) then
              write (nout,'(/''You set the native conformation flag with more than one '',  &
&                                                                 ''polypeptide, abort.''/)')
              stop
          else if (lattice_jmax < 2*polypeptide_length(1)-1) then
              write (nout,'(/''lattice_jmax too small for native conformation calculation,''&
&                                                                              ,''abort''/)')
              stop
          else if (lattice_imin > -polypeptide_length(1)) then
              write (nout,'(/''lattice_imin too large for native conformation calculation,''&
&                                                                              ,''abort''/)')
              stop
          else if (lattice_imax < polypeptide_length(1)) then
              write (nout,'(/''lattice_imax too small for native conformation calculation,''&
&                                                                              ,''abort''/)')
              stop
          end if
      end if
!
!-----------------------------------------------------
!       INTERNAL SUBROUTINE
!-----------------------------------------------------
!
      contains
          subroutine check_eof
!
          if (free_format_error_flag == (-1)) then
              write (nout,'('' unexpected eof while reading link input, abort'')')
              write (nout,'(/'' was reading the input line: ''/1x,a80)') card
              stop
          endif
!
          end subroutine check_eof
!
          subroutine check_number
!
          if (free_format_error_flag == 0) then
              write (nout,'('' expected a number on input and instead '',                   &
&                    ''encountered the word: ''/                     1x,a80/'' abort''/)')  &
&                    field
              write (nout,'(/'' was reading the input line: ''/1x,a80)') card
              stop
          endif
!
          end subroutine check_number
!
          function convert_lower_case (input_string) result (output_string)
!
          character (len=1) :: input_string, output_string
          integer :: collating_difference
!
          if (ichar(input_string) >= ichar('A') .AND. ichar(input_string) <= ichar('Z')) then
              collating_difference = ichar(input_string) - ichar('A')
              output_string = char(ichar('a') + collating_difference)
          else
              output_string = input_string
          end if
!
          end function convert_lower_case

     end subroutine leer_entrada_2

END MODULE LEER_ENTRADA_M_2

      program superficie_proteina
!
!        compute the density of states for multiple amino acid polypeptides
!        interacting with a surface and with each other by explicit search of 
!        the total conformation space
!
!        John K. Prentice
!        Quetzal Computational Associates, Incorporated
!        August 1996
!
!        Copyright (c) by Quetzal Computational Associates, Incorporated, 1996, 1997
!
      USE kind_definitions
      USE leer_entrada_m_1
      USE leer_entrada_m_2
      USE conformations
      USE timing_m
!
      implicit none
!
!        endrada_unidad is the unit number for ASCII input
!        rendimiento_unidad is the unit number for ASCII output
!
      integer, parameter :: endrada_unidad = 5
      integer, parameter :: rendimiento_unidad = 7
      integer, parameter :: density_unit_number = 1

!
!        boltzmann_constant is the Boltzmann constant in meV/K
!
      real (kind = LONGreal), parameter :: boltzmann_constant = 8.617288e-2_LONGreal
!
!        conversion from kcal/mole to meV/molecule
!
      real (kind = LONGreal), parameter :: kcal_conversion = 43.438
!
      integer, dimension (:,:,:), allocatable :: lattice
      real (kind = LONGreal), dimension (:,:), allocatable :: individual_conformations,     &
&                                                             combined_conformations,       &
&                                                             average_internal_energy,      &
&                                                             average_contact_energy,       &
&                                                             average_surface_energy
      real (kind = LONGreal), dimension(:,:,:), allocatable :: surface_conformations
      real (kind = LONGreal), dimension (:), allocatable :: contact_conformations, percent, &
&                                                           density_of_states, probability
      integer, dimension (:,:), allocatable :: polypeptide_sequence, surface_sequence
      integer, dimension (:), allocatable :: polypeptide_length
      integer, dimension (:,:,:), allocatable :: polypeptide_position
      integer, dimension (:,:), allocatable :: surface_position
      integer, dimension (2,4) :: lattice_delta
      integer :: surface_length, i_polypeptide_origin, j_polypeptide_origin, unit_number,   &
&                amino_acid, number_of_polypeptides, lattice_imin, lattice_imax,            &
&                lattice_jmin, lattice_jmax, maximum_polypeptide_length, minimum_j,         &
&                maximum_j, i, j, m, n, polypeptide, i1, j1, lattice_width,                 &
&                maximum_surface_j, lowest_energy_state, i_new, delta, number_of_surfaces,  &
&                no_of_e_bins_per_amino_acid, number_of_bins
      real (kind = LONGreal) :: conformation_sum, partition_function_value, exponent,       &
&                               HH_internal_energy, HP_internal_energy, PP_internal_energy, &
&                               HH_surface_energy, HP_surface_energy, PP_surface_energy,    &
&                               temperature, energy_unit, energy_per_bin,                   &
&                               save_internal_conformation, save_surface_conformation,      &
&                               save_contact_conformation, save_total_conformation
      character (len=50) :: file_name
      logical :: new_conformation, native_conformation
      logical, dimension(:,:), allocatable :: surface_contact
!
      call second (initial_time)
      total_conformation_time = 0.0
      total_energy_time = 0.0
!
!        open the output file
!
      open (unit=rendimiento_unidad,file="protein.out",status="unknown",form="formatted")
!
!        read the input the first time to get the number_of_polypeptides
!
      call leer_entrada_1 (endrada_unidad, rendimiento_unidad, number_of_polypeptides,      &
&                          maximum_polypeptide_length, surface_length, number_of_surfaces)
!
!        read the input the second time to get rest of the input
!
      allocate (surface_sequence(surface_length,number_of_surfaces))
      allocate (surface_position(2,surface_length))
      allocate (polypeptide_length(number_of_polypeptides))
      allocate (polypeptide_sequence(maximum_polypeptide_length, number_of_polypeptides))
      call leer_entrada_2 (endrada_unidad, rendimiento_unidad, number_of_polypeptides,      &
&                          maximum_polypeptide_length, polypeptide_length,                  &
&                          polypeptide_sequence, lattice_imin, lattice_imax, lattice_jmin,  &
&                          lattice_jmax, surface_length, number_of_surfaces,                &
&                          surface_sequence, save_internal_conformation,                    &
&                          save_surface_conformation, save_contact_conformation,            &
&                          save_total_conformation, native_conformation,                    &
&                          HH_internal_energy, HP_internal_energy, PP_internal_energy,      &
&                          HH_surface_energy, HP_surface_energy, PP_surface_energy,         &
&                          temperature, energy_unit, no_of_e_bins_per_amino_acid)
!
      number_of_bins = 100 * no_of_e_bins_per_amino_acid * polypeptide_length(1)
!
!        allocate memory
!
      allocate (surface_contact(maximum_polypeptide_length,number_of_polypeptides))
      allocate (lattice(2,lattice_imin:lattice_imax,lattice_jmin:lattice_jmax))
      allocate (individual_conformations(-number_of_bins:0,number_of_polypeptides))
      allocate (surface_conformations(-number_of_bins:0,                                    &
&                                                 number_of_polypeptides,number_of_surfaces))
      allocate (combined_conformations(-number_of_bins:0,number_of_surfaces))
      allocate (average_internal_energy(-number_of_bins:0,number_of_surfaces))
      allocate (average_surface_energy(-number_of_bins:0,number_of_surfaces))
      allocate (average_contact_energy(-number_of_bins:0,number_of_surfaces))
      allocate (contact_conformations(-number_of_bins:0))
      allocate (polypeptide_position(2,maximum_polypeptide_length,number_of_polypeptides))
!
!        define the index shifts that define the possible location
!        of amino acid residue n+1, given the position of amino 
!        acid residue n
!
      lattice_delta(1,1) = 0
      lattice_delta(2,1) = 1
      lattice_delta(1,2) = -1
      lattice_delta(2,2) = 0
      lattice_delta(1,3) = 0
      lattice_delta(2,3) = -1
      lattice_delta(1,4) = 1
      lattice_delta(2,4) = 0
!
!        define the surface
!
      if (native_conformation) then
          surface_position(:,1:surface_length) = -99
          maximum_surface_j = -9999
      else
          i = 0
          do n = lattice_imin, lattice_imax
              i = i + 1
              surface_position(1,i) = n
              surface_position(2,i) = 0
              maximum_surface_j = 0
          end do
      end if
!
!        open units for saving conformations, if necessary, and write out surface
!        information to these files
!
      if (save_internal_conformation <= 0.0_LONGreal) then
          open (unit=51,file="internal.dat",status="unknown",form="formatted")
          write(51,'(''Lattice data:'')')
          write(51,'(4i10)') lattice_imin, lattice_imax, lattice_jmin, lattice_jmax
          write(51,'(''Surface conformation data:'')')
          write(51,'(i10)') surface_length
          write(51,'(40i2)') surface_sequence(1:surface_length,1)
          write(51,'(20i4)') surface_position(1,1:surface_length)
          write(51,'(20i4)') surface_position(2,1:surface_length)
          write(51,'(''Polypeptide data:'')')
          write(51,'(2i10)') number_of_polypeptides, polypeptide_length(1)
          write(51,'(40i2)') polypeptide_sequence(1:polypeptide_length(1),1)
          write(51,'(''Internal conformations:'')') 
      end if
      if (save_surface_conformation <= 0.0_LONGreal .AND. .NOT. native_conformation) then
          open (unit=52,file="surface.dat",status="unknown",form="formatted")
          write(52,'(''Lattice data:'')')
          write(52,'(4i10)') lattice_imin, lattice_imax, lattice_jmin, lattice_jmax
          write(52,'(''Surface conformation data:'')')
          write(52,'(i10)') surface_length
          write(52,'(40i2)') surface_sequence(1:surface_length,1)
          write(52,'(20i4)') surface_position(1,1:surface_length)
          write(52,'(20i4)') surface_position(2,1:surface_length)
          write(52,'(''Polypeptide data:'')')
          write(52,'(2i10)') number_of_polypeptides, polypeptide_length(1)
          write(52,'(40i2)') polypeptide_sequence(1:polypeptide_length(1),1)
          write(52,'(''Surface conformations:'')') 
      end if
      if (save_contact_conformation <= 0.0_LONGreal .AND. .NOT. native_conformation) then
          open (unit=53,file="contact.dat",status="unknown",form="formatted")
          write(53,'(''Lattice data:'')')
          write(53,'(4i10)') lattice_imin, lattice_imax, lattice_jmin, lattice_jmax
          write(53,'(''Surface conformation data:'')')
          write(53,'(i10)') surface_length
          write(53,'(40i2)') surface_sequence(1:surface_length,1)
          write(53,'(20i4)') surface_position(1,1:surface_length)
          write(53,'(20i4)') surface_position(2,1:surface_length)
          write(53,'(''Polypeptide data:'')')
          write(53,'(2i10)') number_of_polypeptides, polypeptide_length(1)
          write(53,'(40i2)') polypeptide_sequence(1:polypeptide_length(1),1)
          write(53,'(''Internal conformations:'')') 
      end if
      if (save_total_conformation <= 0.0_LONGreal .AND. .NOT. native_conformation) then
          open (unit=54,file="total.dat",status="unknown",form="formatted")
          write(54,'(''Lattice data:'')')
          write(54,'(4i10)') lattice_imin, lattice_imax, lattice_jmin, lattice_jmax
          write(54,'(''Surface conformation data:'')')
          write(54,'(i10)') surface_length
          write(54,'(40i2)') surface_sequence(1:surface_length,1)
          write(54,'(20i4)') surface_position(1,1:surface_length)
          write(54,'(20i4)') surface_position(2,1:surface_length)
          write(54,'(''Polypeptide data:'')')
          write(54,'(2i10)') number_of_polypeptides, polypeptide_length(1)
          write(54,'(40i2)') polypeptide_sequence(1:polypeptide_length(1),1)
          write(54,'(''Internal conformations:'')') 
      end if
!
!        initialize the conformation arrays
!
      individual_conformations = 0.0_LONGreal
      combined_conformations = 0.0_LONGreal
      surface_conformations = 0.0_LONGreal
      contact_conformations = 0.0_LONGreal
      average_internal_energy = 0.0_LONGreal
      average_surface_energy = 0.0_LONGreal
      average_contact_energy = 0.0_LONGreal
!
!        initialize the lattice.  
!
      lattice = 0
      lattice_width = lattice_imax - lattice_imin + 1
      if (.NOT. native_conformation) then
          do n = 1, surface_length
              i = surface_position(1,n)
              j = surface_position(2,n)
              lattice(1,i,j) = n
              lattice(2,i,j) = 0
          end do
      end if
!
!        loop over conformations of polypeptide 1 for all possible j, holding the
!        i coordinate of the 1st amino acid fixed
!
      if (.NOT. native_conformation) then    
          i_polypeptide_origin = lattice_imin       
sj:       do j_polypeptide_origin = lattice_jmax, lattice_jmin, -1
              if (lattice(1,i_polypeptide_origin,j_polypeptide_origin) /= 0 .AND.           &
&                             lattice(2,i_polypeptide_origin,j_polypeptide_origin) == 0) then
                  minimum_j = j_polypeptide_origin + 1
                  exit sj
              end if
          end do sj
          minimum_j = max(minimum_j, lattice_jmin)
          maximum_j = min(lattice_jmax, polypeptide_length(1))
      else
          i_polypeptide_origin = 0     
          minimum_j = polypeptide_length(1)
          maximum_j = polypeptide_length(1)
      end if
      do j_polypeptide_origin = minimum_j, maximum_j
!
!        location of amino acid residue 1 on polypeptide 1
!
          i1 = i_polypeptide_origin
          j1 = j_polypeptide_origin
          lattice(1,i1,j1) = 1
          lattice(2,i1,j1) = 1
          polypeptide_position(1,1,1) = i1
          polypeptide_position(2,1,1) = j1
          amino_acid = 1
          polypeptide = 1
          new_conformation = .TRUE.
!
!        determine whether this amino acid is in contact with the surface
!
          if (.NOT. native_conformation) then
              surface_contact(1,1) = .FALSE.
              if (polypeptide_position (2,1,1) - 1 <= maximum_surface_j) then
contact2:       do i = polypeptide_position(1,1,1)-1, polypeptide_position(1,1,1)+1
                  do j = polypeptide_position(2,1,1)-1, polypeptide_position(2,1,1)+1
                    if (i < lattice_imin) then
                        delta = mod(lattice_imin-1-i,lattice_width) + 1
                        i_new = lattice_imax - delta + 1
                    else if (i > lattice_imax) then
                        delta = mod(i-lattice_imax-1,lattice_width) + 1
                        i_new = lattice_imin + delta - 1
                    else
                        i_new = i
                    end if
                    if (lattice(1,i_new,j) /= 0 .AND. lattice(2,i_new,j) == 0) then
                        surface_contact(1,1) = .TRUE.
                        exit contact2
                    end if
                  end do
                end do contact2
              end if
          end if
!
!        recursively compute possible conformations and density of states
!
          call second (initial_conformation_time)
          call conformation (number_of_polypeptides, maximum_polypeptide_length,            &
&                            polypeptide_length, polypeptide, amino_acid, lattice,          &
&                            lattice_delta, lattice_imin, lattice_imax, lattice_jmin,       &
&                            lattice_jmax, lattice_width, polypeptide_sequence,             &
&                            polypeptide_position, individual_conformations,                &
&                            combined_conformations, surface_conformations,                 &
&                            contact_conformations, number_of_surfaces, surface_sequence,   &
&                            surface_position, surface_length, surface_contact,             &
&                            maximum_surface_j, new_conformation,                           &
&                            save_internal_conformation, save_surface_conformation,         &
&                            save_contact_conformation, save_total_conformation,            &
&                            native_conformation, average_internal_energy,                  &
&                            average_contact_energy, average_surface_energy,                &
&                            HH_internal_energy, HP_internal_energy, PP_internal_energy,    &
&                            HH_surface_energy, HP_surface_energy, PP_surface_energy,       &
&                            number_of_bins, no_of_e_bins_per_amino_acid)
          lattice(1,i1,j1) = 0
          lattice(2,i1,j1) = 0
      end do
!
!        print the density of states
!
      allocate (percent(-number_of_bins:0))
      allocate (density_of_states(-number_of_bins:0))
      allocate (probability(-number_of_bins:0))
      write(rendimiento_unidad,'(//''Number of polypeptides = '',i2)')                      &
&                                                                      number_of_polypeptides
      write(rendimiento_unidad,'(''Polypeptide length = '',i3)') polypeptide_length(1)
      if (native_conformation) then
          write(rendimiento_unidad,'(''No surface'')')
      else
          write(rendimiento_unidad,'(''Surface length = '',i10)') surface_length
      end if 
      write(rendimiento_unidad,'(''Lattice dimensions: imin='',i3,'', imax='',i3,           &
&                            '', jmin='',i3,'', jmax='',i3)') lattice_imin, lattice_imax,   &
&                            lattice_jmin, lattice_jmax
      write(rendimiento_unidad,'(''Polypeptide sequence: '',60i1)')                         &
&                                      (polypeptide_sequence(n,1), n=1,polypeptide_length(1))
      write(rendimiento_unidad,'(''Interaction energies between protein amino acids:'')')
      write(rendimiento_unidad,'(''    Hydrophobic/hydrophobic interaction energy = '',f7.3,&
&                                                            '' units'')') HH_internal_energy
      write(rendimiento_unidad,'(''    Hydrophobic/hydrophilic interaction energy = '',f7.3,&
&                                                            '' units'')') HP_internal_energy
      write(rendimiento_unidad,'(''    Hydrophilic/hydrophilic interaction energy = '',f7.3,&
&                                                            '' units'')') PP_internal_energy
      write(rendimiento_unidad,'(''Interaction energies between protein/surface amino '',   &
&                                                                               ''acids:'')')
      write(rendimiento_unidad,'(''    Hydrophobic/hydrophobic interaction energy = '',f7.3,&
&                                                             '' units'')') HH_surface_energy
      write(rendimiento_unidad,'(''    Hydrophobic/hydrophilic interaction energy = '',f7.3,&
&                                                             '' units'')') HP_surface_energy
      write(rendimiento_unidad,'(''    Hydrophilic/hydrophilic interaction energy = '',f7.3,&
&                                                             '' units'')') PP_surface_energy
      write(rendimiento_unidad,'(''Energy conversion = '',f7.3,'' meV/units'')') energy_unit      
      write(rendimiento_unidad,'(''                  = '',f7.3,'' kcal/mole/unit'')')       &
&                                                                energy_unit/kcal_conversion
      write(rendimiento_unidad,'(''Temperature = '',f8.3,'' K'')') temperature
!
      energy_per_bin = 1.0_LONGreal / real(no_of_e_bins_per_amino_acid,LONGreal)
      write(rendimiento_unidad,'(/''Density of states for internal conformations:'')')
      lowest_energy_state = 0
      do n = -number_of_bins, -1
          if (individual_conformations(n,1) > 0.5_LONGreal) then
              lowest_energy_state = n
              exit
          end if
      end do
      conformation_sum = sum(individual_conformations(:,1))

      percent = 0.0_LONGreal   ! JRA
      if (lowest_energy_state /= 0) then
          percent(lowest_energy_state:0) =                                                  &
&                        individual_conformations(lowest_energy_state:0,1) / conformation_sum
      else
          percent(0) = 1.0_LONGreal
      end if
      percent = percent * 100.0_LONGreal
!
!
!        compute the density of states at the assumed temperature
!
      do n = lowest_energy_state, 0
         exponent = abs(energy_unit) * real(n,LONGreal)*energy_per_bin /                   &
&                                                         (boltzmann_constant *  temperature)
          density_of_states(n) = individual_conformations(n,1) * exp(-exponent)
      end do
!
!        compute value of canonical partition function at the assumed temperature
!
      partition_function_value = sum(density_of_states(lowest_energy_state:0))
!
!        compute probability of occupation for each state
!
      probability(lowest_energy_state:0) = density_of_states(lowest_energy_state:0)         &
&                                                                  / partition_function_value
      do n = lowest_energy_state, 0
        if (individual_conformations(n,1) > 0) then
          write(rendimiento_unidad,'(''     E='',f7.3,'' D='',f12.0,'' %='',1pe9.3,         &
&                           '' P='',1pe9.3)') real(n,LONGreal)*energy_per_bin,              &
&                                    individual_conformations(n,1), percent(n),             &
&                                    probability(n)
        end if
      end do
!
          write(rendimiento_unidad,'(''                           '',                       &
&                                       ''Total conformations = '',f10.0/)') conformation_sum
          print * , 'Total conformations = ',conformation_sum
!
      if (number_of_polypeptides > 1) then
          write (rendimiento_unidad,'(/''Density of states for polypeptide/'',              &
&                                                                 ''polypeptide contact:'')')
          lowest_energy_state = 0
          do n = -number_of_bins, -1
              if (contact_conformations(n) > 0.5_LONGreal) then
                  lowest_energy_state = n
                  exit
              end if
          end do
          conformation_sum = sum(contact_conformations(:))
          if (lowest_energy_state /= 0) then
              percent(lowest_energy_state:0) =                                              &
&                             contact_conformations(lowest_energy_state:0) / conformation_sum
          else
              percent(0) = 1.0_LONGreal
          end if
          percent = percent * 100.0_LONGreal
!
!        compute the density of states at the assumed temperature
!
          do n = lowest_energy_state, 0
              exponent = abs(energy_unit) * real(n,LONGreal)*energy_per_bin /               &
&                                                         (boltzmann_constant *  temperature)
              density_of_states(n) = contact_conformations(n) * exp(-exponent)
          end do
!
!        compute value of canonical partition function at the assumed temperature
!
          partition_function_value = sum(density_of_states(lowest_energy_state:0))
!
!        compute probability of occupation for each state
!
          probability(lowest_energy_state:0) = density_of_states(lowest_energy_state:0)     &
&                                                                  / partition_function_value
          do n = lowest_energy_state, 0
            if (contact_conformations(n) > 0) then
              write(rendimiento_unidad,'(''     E='',f7.3,'' D='',f12.0,'' %='',1pe9.3,     &
&                                   '' P='',1pe9.3)')                                       &
&                                    real(n,LONGreal)*energy_per_bin,                       &
&                                    contact_conformations(n), percent(n),                  &
&                                    probability(n)
            end if
          end do
!
          write(rendimiento_unidad,'(''                           '',                       &
&                                       ''Total conformations = '',f10.0/)') conformation_sum
          print * , 'Total conformations = ',conformation_sum

      end if
!
      if (.NOT. native_conformation) then
          do m = 1, number_of_surfaces
              write(rendimiento_unidad,'(//''Surface number '',i3,'', sequence= '',         &
&                                     60i1/)') m, (surface_sequence(n,m), n=1,surface_length)
              write(rendimiento_unidad,'(/''Density of states for surface contact '',       &
&                                                                       ''conformations:'')')
               lowest_energy_state = 0
               do n = -number_of_bins, -1
                  if (surface_conformations(n,1,m) > 0.5_LONGreal) then
                      lowest_energy_state = n
                      exit
                  end if
              end do
              conformation_sum = sum(surface_conformations(:,1,m))
              if (lowest_energy_state /= 0) then
                  percent(lowest_energy_state:0) =                                          &
&                         surface_conformations(lowest_energy_state:0,1,m) / conformation_sum
              else
                  percent(0) = 1.0_LONGreal
              end if
              percent = percent * 100.0_LONGreal
!
!        compute the density of states at the assumed temperature
!
              do n = lowest_energy_state, 0
                  exponent = abs(energy_unit) * real(n,LONGreal)*energy_per_bin /           &
&                                                         (boltzmann_constant *  temperature)
                  density_of_states(n) = surface_conformations(n,1,m) * exp(-exponent)
              end do
!
!        compute value of canonical partition function at the assumed temperature
!
              partition_function_value = sum(density_of_states(lowest_energy_state:0))
!
!        compute probability of occupation for each state
!
              probability(lowest_energy_state:0) = density_of_states(lowest_energy_state:0) &
&                                                                  / partition_function_value
              do n = lowest_energy_state, 0
                if (surface_conformations(n,1,m) > 0) then
                  write(rendimiento_unidad,'(''     E='',f7.3,'' D='',f12.0,'' %='',1pe9.3, &
&                                   '' P='',1pe9.3)')                                       &
&                                    real(n,LONGreal)*energy_per_bin,                       &
&                                    surface_conformations(n,1,m), percent(n),              &
&                                    probability(n)
                end if
              end do
!
              write(rendimiento_unidad,'(''                           '',                   &
&                                      ''Total conformations = '',f10.0//)') conformation_sum
          print * , 'Total conformations = ',conformation_sum
!
              write (rendimiento_unidad,'(/''Total density of states:'')')
              lowest_energy_state = 0
              do n = -number_of_bins, -1
                  if (combined_conformations(n,m) > 0.5_LONGreal) then
                      lowest_energy_state = n
                      exit
                  end if
              end do
              conformation_sum = sum(combined_conformations(:,m))
              if (lowest_energy_state /= 0) then
                  percent(lowest_energy_state:0) =                                          &
&                          combined_conformations(lowest_energy_state:0,m) / conformation_sum
              else
                  percent(0) = 1.0_LONGreal
              end if
              percent = percent * 100.0_LONGreal
!
!        compute average energies for each energy state
!
              where (combined_conformations(:,m) /= 0.0_LONGreal)
                  average_internal_energy(:,m) = average_internal_energy(:,m) /             &
&                                      (combined_conformations(:,m) * number_of_polypeptides)
                  average_surface_energy(:,m) = average_surface_energy(:,m) /               &
&                                      (combined_conformations(:,m) * number_of_polypeptides)
                  average_contact_energy(:,m) = average_contact_energy(:,m) /               &
&                                         (combined_conformations(:,m) *                    &
&                                             real(max(2,number_of_polypeptides)-1,LONGreal))
              elsewhere
                  average_internal_energy(:,m) = 0.0_LONGreal
                  average_surface_energy(:,m) = 0.0_LONGreal
                  average_contact_energy(:,m) = 0.0_LONGreal
              end where
!
!        compute the density of states at the assumed temperature
!
              do n = lowest_energy_state, 0
                  exponent = abs(energy_unit) *real(n,LONGreal)*energy_per_bin /            &
&                                                         (boltzmann_constant *  temperature)
                  density_of_states(n) = combined_conformations(n,m) * exp(-exponent)
              end do
!
!        compute value of canonical partition function at the assumed temperature
!
              partition_function_value = sum(density_of_states(lowest_energy_state:0))
!
!        compute probability of occupation for each state
!
              probability(lowest_energy_state:0) = density_of_states(lowest_energy_state:0) &
&                                                                  / partition_function_value
              do n = lowest_energy_state, 0
                if (combined_conformations(n,m) > 0) then
                  write(rendimiento_unidad,'(''     E='',f7.3,'' D='',f12.0,'' %='',1pe9.3, &
&                           '' I='',0pf6.1,'' S='',f6.1,'' C='',f6.1,'' P='',1pe9.3)')      &
&                                    real(n,LONGreal)*energy_per_bin,                       &
&                                    combined_conformations(n,m), percent(n),               &
&                                    average_internal_energy(n,m),                          &
&                                    average_surface_energy(n,m),                           &
&                                    average_contact_energy(n,m), probability(n)
                end if
              end do
!
              write(rendimiento_unidad,'(''                           '',                   &
&                                       ''Total conformations = '',f10.0/)') conformation_sum
          print * , 'Total conformations = ',conformation_sum
          end do
      end if
!
!        deallocate memory
!
      deallocate (lattice)
      deallocate (polypeptide_length)
      deallocate (polypeptide_sequence)
      deallocate (individual_conformations)
      deallocate (combined_conformations)
      deallocate (surface_conformations)
      deallocate (contact_conformations)
      deallocate (polypeptide_position)
      deallocate (surface_sequence)
      deallocate (surface_position)
      deallocate (percent)
      deallocate (average_internal_energy)
      deallocate (average_surface_energy)
      deallocate (average_contact_energy)
      deallocate (density_of_states)
      deallocate (probability)
!
!        close units
!
!      close (unit = 6)
      if (save_internal_conformation <= 0.0_LONGreal) close (unit=51)
      if (.NOT. native_conformation) then
          if (save_surface_conformation <= 0.0_LONGreal) close (unit=52)
          if (save_contact_conformation <= 0.0_LONGreal) close (unit=53)
          if (save_total_conformation <= 0.0_LONGreal) close (unit=54)
      end if
!
      call second (final_time)
      total_time = final_time - initial_time
      percent_conformation_time = total_conformation_time/total_time*100.0
      percent_energy_time = total_energy_time/total_time*100.0
      write (*,'(//"total time for calculation   = ",1pe15.5)') total_time
      write (*,'("total time for conformations = ",1pe15.5,              &
&             ", % total time = ",0pf11.7)')  total_conformation_time,                     &
&            percent_conformation_time
      write (*,'("total time for energy calcs  = ",1pe15.5,                &
&             ", % total time = ",0pf11.7)') total_energy_time,                             &
&            percent_energy_time
!
      end program superficie_proteina


