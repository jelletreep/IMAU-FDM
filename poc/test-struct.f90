module model_settings_types
  implicit none
  private
  public :: minimum_values_t, general_settings_t, model_settings_t

  type :: minimum_values_t
    double precision :: ts_minimum
    double precision :: det2d_minimum
  end type minimum_values_t

  type :: general_settings_t
    character(len=:), allocatable :: restart_type
  end type general_settings_t

  type :: model_settings_t
    type(minimum_values_t)  :: minimum_values
    type(general_settings_t):: general_settings
  end type model_settings_t

contains

end module model_settings_types


module model_settings_io
  use, intrinsic :: iso_fortran_env, only: error_unit
  use tomlf
  use model_settings_types, only: model_settings_t
  implicit none
  private
  public :: load_model_settings_toml, print_model_settings

contains

  subroutine load_model_settings_toml(filename, settings)
    character(len=*), intent(in)  :: filename
    type(model_settings_t), intent(inout) :: settings

    integer                       :: fu, rc
    logical                       :: file_exists
    type(toml_table), allocatable :: table
    type(toml_table), pointer     :: child

    inquire(file=filename, exist=file_exists)
    if (.not. file_exists) then
      write(error_unit,'("Error: TOML file ",a," not found")') trim(filename)
      error stop 1
    end if

    open(action='read', file=filename, iostat=rc, newunit=fu)
    if (rc /= 0) then
      write(error_unit,'("Error: opening TOML file ",a," failed; iostat=",i0)') trim(filename), rc
      error stop 1
    end if

    call toml_parse(table, fu)
    close(fu)

    if (.not. allocated(table)) then
      write(error_unit,'("Error: TOML parsing failed for ",a)') trim(filename)
      error stop 1
    end if

    ! ---- [minimum_values] ----
    nullify(child)
    call get_value(table, 'minimum_values', child, requested=.false.)
    if (associated(child)) then
      call get_value(child, 'ts_minimum',    settings%minimum_values%ts_minimum)
      call get_value(child, 'det2d_minimum', settings%minimum_values%det2d_minimum)
    end if

    ! ---- [general_settings] ----
    nullify(child)
    call get_value(table, 'general_settings', child, requested=.false.)
    if (associated(child)) then
      call get_value(child, 'restart_type', settings%general_settings%restart_type)
    end if

    deallocate(table)
  end subroutine load_model_settings_toml


  subroutine print_model_settings(settings)
    type(model_settings_t), intent(in) :: settings

    print *, " "
    print *, "Parsed model settings:"
    print *, "  minimum_values.ts_minimum   = ", settings%minimum_values%ts_minimum
    print *, "  minimum_values.det2d_minimum= ", settings%minimum_values%det2d_minimum

    if (allocated(settings%general_settings%restart_type)) then
      print *, "  general_settings.restart_type = ", trim(settings%general_settings%restart_type)
    else
      print *, "  general_settings.restart_type = (not set)"
    end if
    print *, " "
  end subroutine print_model_settings

end module model_settings_io


program main
  use, intrinsic :: iso_fortran_env, only: error_unit
  use model_settings_types, only: model_settings_t
  use model_settings_io,    only: load_model_settings_toml, print_model_settings
  implicit none

  character(len=1024) :: model_settings_file
  type(model_settings_t) :: settings

  call get_command_argument(1, model_settings_file)
  if (len_trim(model_settings_file) == 0) then
    write(error_unit,'("Usage: ./read_settings model_settings.toml")')
    error stop 2
  end if

  call load_model_settings_toml(trim(model_settings_file), settings)
  call print_model_settings(settings)
end program main