Name: JADE
Version: 2017.1
Release:        1%{?dist}
Summary:        JADE software

Group:		    System Environment/Libraries
License:	    GPLv2+
URL:		    http://wwwjade.mppmu.mpg.de/%{name}-%{version}.tar.gz
Source:         %{name}-%{version}.tar.gz
BuildRoot:      %{_tmppath}/%{name}-%{version}-%{release}-root-%(%{__id_u} -n)
Prefix: /usr
BuildArch:      %{_arch}

Vendor:         JADE collaboration
BuildRequires:  root gcc gfortran
Requires: 		root gcc gfortran 

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

#configure 

#make
%install
mkdir -p $RPM_BUILD_ROOT/opt/%{name}-%{version}
cp $RPM_BUILD_ROOT/opt/%{name}-%{version}

%post
# the post section is where you can run commands after the rpm is installed.

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
/opt/%{name}-%{version}

%changelog
* Thu Mar 09 2017  Andrii Verbytskyi
- 0.1 Set-up the spec
