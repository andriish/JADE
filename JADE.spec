Name: zevis
Version: 3.15
Release:        1%{?dist}
Summary:        Zeus event visualisation program

Group:		    System Environment/Libraries
License:	    GPLv2+
URL:		    /afs/desy.de/user/z/zevis/software/Released/standalone/%{name}-%{version}.tar.gz
Source:         %{name}-%{version}.tar.gz
BuildRoot:      %{_tmppath}/%{name}-%{version}-%{release}-root-%(%{__id_u} -n)
Prefix: /usr
BuildArch:      %{_arch}

Vendor:         ZEUS collaboration
BuildRequires:  root gcc
Requires: 		root 

%description
Zeus event visualisation program

%package        -n bazlib
Summary:        Zeus event visualisation program
Release:        1%{?dist}
Group:          System/Base

%description -n bazlib
Zeus event visualisation program


%prep

%setup -n %{name}-%{version}

%build
sh autogen.sh
%configure 
#make %{?_smp_mflags}
#make  bindir=/usr/bin libdir=/usr/%_lib ZARCH_TYPE="000" sharedir=/usr/share
#%make  bindir=/usr/bin libdir=/usr/%_lib ZARCH_TYPE="000" sharedir=/usr/share

make
%install
#make install bindir=$RPM_BUILD_ROOT/usr/bin libdir=$RPM_BUILD_ROOT/usr/%_lib ZARCH_TYPE="000" sharedir=$RPM_BUILD_ROOT/usr/share
#%make install bindir=$RPM_BUILD_ROOT/usr/bin libdir=$RPM_BUILD_ROOT/usr/%_lib ZARCH_TYPE="000" sharedir=$RPM_BUILD_ROOT/usr/share
%make_install

%post
# the post section is where you can run commands after the rpm is installed.

%clean
rm -rf $RPM_BUILD_ROOT

%postun -p /sbin/ldconfig

%files
%defattr(-,root,root,-)
/usr/bin/*
/usr/%_lib/*
/usr/share/%{name}/*

%changelog
* Fri Oct 10 2014  Andrii Verbytskyi
- 0.1 Set-up the spec
