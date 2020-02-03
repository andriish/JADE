%define debug_package %{nil}
%define _binaries_in_noarch_packages_terminate_build   0
%define    _use_internal_dependency_generator 0
%define __find_requires %{nil}
Name: JADE
Version:   2020.1
Release:        1%{?dist}
Summary:        JADE software

Group:		    System Environment/Libraries
License:	    GPLv2+
URL:		    https://github.com/andriish/JADE/archive/Feb2020.zip
Source:         %{name}-%{version}.tar.gz
BuildRoot:      %{_tmppath}/%{name}-%{version}-%{release}-root-%(%{__id_u} -n)
Prefix: /usr
BuildArch:      noarch

Vendor:         JADE collaboration
Requires: 		root gcc gcc-gfortran  HepMC3 HepMC3-devel HepMC3-search HepMC3-search-devel


%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
BuildRequires:	cmake >= 3.4.3
%else
BuildRequires:	cmake3 >= 3.4.3
%endif


%description
JADE software

%package        -n bazlib
Summary:        JADE software
Release:        1%{?dist}
Group:          System/Base

%description -n bazlib
JADE software

%prep
%setup -n %{name}-%{version}



%build

cd picocernlib
%if %{?fedora}%{!?fedora:0} || %{?rhel}%{!?rhel:0} >= 8
%cmake .
%else
cmake3 
%endif
cd ..


%install
cd picocernlib
%make_install
cd ..



%post
# the post section is where you can run commands after the rpm is installed.

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
/usr/*

%changelog
* Thu Mar 09 2017  Andrii Verbytskyi
- 0.1 Set-up the spec
