def test_procedure_interface():
    ''' Functional test that parser copes with a procedure declaration in
    a subroutine '''
    from fparser import api
    source_str = '''  subroutine divergence_diagnostic_alg(u, t, mesh_id)
    use dg_matrix_vector_kernel_mod, only: dg_matrix_vector_kernel_type
    use runtime_constants_mod,       only: get_mass_matrix, &
                                           get_div, &
                                           w3inv_id
    use operator_mod,                only: operator_type
    use field_mod,                   only: field_type, write_interface
    use fs_continuity_mod,           only: W3
    use psykal_lite_mod,             only: invoke_inner_prod
    implicit none 
    type(field_type), intent(in) :: u
    integer(i_def),   intent(in) :: t, mesh_id

    type(field_type)             :: divergence, div_u
    type(operator_type), pointer :: div => null(), m3_inv => null()
    real(r_def)                  :: l2

    procedure (write_interface), pointer  :: tmp_ptr

    divergence =  field_type( vector_space = function_space_collection%get_fs(mesh_id,element_order, W3) )
    div_u      =  field_type( vector_space = function_space_collection%get_fs(mesh_id,element_order, W3) )
    div    => get_div()
    m3_inv => get_mass_matrix(w3inv_id)
    call invoke( dg_matrix_vector_kernel_type( div_u, u, div) )

  end subroutine divergence_diagnostic_alg
'''
    tree = api.parse(source_str, isfree=True, isstrict=False,
                     ignore_comments=False)
    gen_code =  str(tree)
    assert "PROCEDURE (write_interface) , POINTER :: tmp_ptr" in gen_code

