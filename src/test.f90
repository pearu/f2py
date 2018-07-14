!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!> @brief Provides access to the members of the w3_kernel class.
!>
!> Accessor functions for the w3_kernel class are defined in this module.
!>
module compute_mass_matrix_kernel_w3_mod

  use argument_mod,              only: arg_type, func_type,    &
                                      GH_OPERATOR, GH_FIELD,   &
                                      GH_READ, GH_WRITE,       &
                                      ANY_SPACE_9,             &
                                      GH_BASIS, GH_DIFF_BASIS, &
                                      CELLS, GH_QUADRATURE_XYoZ
  use coordinate_jacobian_mod,   only: coordinate_jacobian
  use constants_mod,             only: r_def
  use finite_element_config_mod, only: rehabilitate
  use fs_continuity_mod,         only: W3
  use kernel_mod,                only: kernel_type

  implicit none

  !---------------------------------------------------------------------------
  ! Public types
  !---------------------------------------------------------------------------
  type, public, extends(kernel_type) :: compute_mass_matrix_kernel_w3_type
    private
    type(arg_type) :: meta_args(2) = (/              &
        arg_type(GH_OPERATOR, GH_WRITE, W3, W3),     &
        arg_type(GH_FIELD*3,  GH_READ,  ANY_SPACE_9) &
        /)
    type(func_type) :: meta_funcs(2) = (/     &
        func_type(W3, GH_BASIS),              &
        func_type(ANY_SPACE_9, GH_DIFF_BASIS) &
        /)
    integer :: iterates_over = CELLS
    integer :: gh_shape = GH_QUADRATURE_XYoZ
  contains
    procedure, nopass :: compute_mass_matrix_w3_code
  end type compute_mass_matrix_kernel_w3_type

  !---------------------------------------------------------------------------
  ! Constructors
  !---------------------------------------------------------------------------

  ! overload the default structure constructor for function space
  interface compute_mass_matrix_kernel_w3
    module procedure compute_mass_matrix_constructor
  end interface

  !---------------------------------------------------------------------------
  ! Contained functions/subroutines
  !---------------------------------------------------------------------------
  public compute_mass_matrix_w3_code

contains

type(compute_mass_matrix_kernel_w3_type) &
function compute_mass_matrix_constructor() result(self)
  return
end function compute_mass_matrix_constructor

!> @brief This subroutine computes the mass matrix for the w3 space
!! @param[in] cell Cell number
!! @param[in] nlayers Number of layers.
!! @param[in] ncell_3d ncell*ndf
!! @param[inout] mm Mass matrix data array
!! @param[in] ndf_w3 Number of degrees of freedom per cell for the operator space.
!! @param[in] basis_w3 Scalar basis functions evaluated at quadrature points.
!! @param[in] ndf_chi Number of degrees of freedom per cell for the coordinate field.
!! @param[in] undf_chi Number of unique degrees of freedum  for chi field
!! @param[in] map_chi Dofmap for the cell at the base of the column.
!! @param[in] diff_basis_chi Vector differential basis functions evaluated at quadrature points.
!! @param[inout] chi1 Physical coordinates in the first dir
!! @param[inout] chi2 Physical coordinates in the 2nd dir
!! @param[inout] chi3 Physical coordinates in the 3rd dir
!! @param[in] nqp_h Number of horizontal quadrature points
!! @param[in] nqp_v Number of vertical quadrature points
!! @param[in] wqp_h Horizontal quadrature weights
!! @param[in] wqp_v Vertical quadrature weights
subroutine compute_mass_matrix_w3_code(cell, nlayers, ncell_3d,        &
                                       mm,                             &
                                       chi1, chi2, chi3,               &
                                       ndf_w3, basis_w3,               &
                                       ndf_chi, undf_chi,              &
                                       map_chi, diff_basis_chi,        &
                                       nqp_h, nqp_v, wqp_h, wqp_v )

  implicit none

  !Arguments
  integer, intent(in)     :: cell, nqp_h, nqp_v
  integer, intent(in)     :: nlayers, ndf_w3, ndf_chi, undf_chi
  integer, intent(in)     :: ncell_3d

  integer, dimension(ndf_chi), intent(in) :: map_chi

  real(kind=r_def), dimension(ndf_w3,ndf_w3,ncell_3d),  intent(inout)  :: mm

  real(kind=r_def), dimension(3,ndf_chi,nqp_h,nqp_v), intent(in) :: diff_basis_chi
  real(kind=r_def), dimension(1,ndf_w3,nqp_h,nqp_v),  intent(in) :: basis_w3

  real(kind=r_def), dimension(undf_chi), intent(inout)           :: chi1
  real(kind=r_def), dimension(undf_chi), intent(inout)           :: chi2
  real(kind=r_def), dimension(undf_chi), intent(inout)           :: chi3

  real(kind=r_def), dimension(nqp_h), intent(in) :: wqp_h
  real(kind=r_def), dimension(nqp_v), intent(in) :: wqp_v

  !Internal variables
  integer                                      :: df, df2, k, ik
  integer                                      :: qp1, qp2
  real(kind=r_def), dimension(ndf_chi)         :: chi1_e, chi2_e, chi3_e
  real(kind=r_def)                             :: integrand
  real(kind=r_def), dimension(nqp_h,nqp_v)     :: dj
  real(kind=r_def), dimension(3,3,nqp_h,nqp_v) :: jac

  !loop over layers: Start from 1 as in this loop k is not an offset
  do k = 1, nlayers
    ik = k + (cell-1)*nlayers

    ! indirect the chi coord field here
    do df = 1, ndf_chi
      chi1_e(df) = chi1(map_chi(df) + k - 1)
      chi2_e(df) = chi2(map_chi(df) + k - 1)
      chi3_e(df) = chi3(map_chi(df) + k - 1)
    end do

    call coordinate_jacobian(ndf_chi, nqp_h, nqp_v, chi1_e, chi2_e, chi3_e,  &
                             diff_basis_chi, jac, dj)

    do df2 = 1, ndf_w3
      do df = df2, ndf_w3 ! mass matrix is symmetric
        mm(df,df2,ik) = 0.0_r_def
        do qp2 = 1, nqp_v
          do qp1 = 1, nqp_h
            if ( rehabilitate ) then
              ! With rehabilitation 
              ! W3 mapping is x -> \hat{x}
              integrand = wqp_h(qp1) * wqp_v(qp2) * & 
                       basis_w3(1,df,qp1,qp2)*basis_w3(1,df2,qp1,qp2)  &
                       *dj(qp1,qp2) 
            else
              ! Without rehabilitation 
              ! W3 mapping is x -> \hat{x}/det(J)
              integrand = wqp_h(qp1) * wqp_v(qp2) * & 
                       basis_w3(1,df,qp1,qp2)*basis_w3(1,df2,qp1,qp2)  &
                       /dj(qp1,qp2)  
            end if
            mm(df,df2,ik) = mm(df,df2,ik) + integrand
          end do
        end do
      end do
      do df = df2, 1, -1
        mm(df,df2,ik) = mm(df2,df,ik)
      end do
    end do
  end do ! end of k loop

end subroutine compute_mass_matrix_w3_code

end module compute_mass_matrix_kernel_w3_mod
